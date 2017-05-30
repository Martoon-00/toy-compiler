{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE ViewPatterns     #-}

module Toy.X86.Translator
    ( compile
    , produceBinary
    ) where

import           Control.Lens          (Lens', ix, (%~), (&), (+=), (-=), (<&>), (^?))
import           Control.Monad         (forM, forM_, void)
import           Control.Monad.Fix     (mfix)
import           Control.Monad.State   (get, runState)
import           Control.Monad.Trans   (MonadIO (..))
import           Control.Monad.Writer  (MonadWriter, Writer, censor, runWriter, tell)
import qualified Data.DList            as D
import           Data.Functor          (($>))
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           ((<>))
import qualified Data.Set              as S
import qualified Data.Vector           as V
import           Formatting            (build, formatToString, int, (%))
import qualified Formatting            as F
import           GHC.Exts              (fromList)
import           System.FilePath.Posix ((</>))
import           System.Process        (proc)
import           Universum             (Text, first, toText)

import           Toy.Base              (FunSign (..), Var)
import qualified Toy.SM                as SM
import           Toy.X86.Data          (Inst (..), InstContainer, Insts, Operand (..),
                                        Program (..), StackDirection (..), eax, edx, jmp,
                                        ret, withStackSpace, (//), (?))
import           Toy.X86.Frame         (evalStackShift, mkFrame, resolveMemRefs)
import           Toy.X86.Optimize      (optimize)
import           Toy.X86.Process       (readCreateProcess)
import           Toy.X86.SymStack      (SymStackHolder, allocSymStackOp, occupiedRegs,
                                        peekSymStackOp, popSymStackOp, runSymStackHolder)


compile :: SM.Insts -> Insts
compile = mconcat . map compileFun . separateFuns

compileFun :: SM.Insts -> Insts
compileFun insts =
    let (name, args) = case insts ^? ix 0 of
            Just (SM.Enter n ai) -> (n, ai)
            _                    -> error "Where is my Enter?!"
        vars  = foldMap gatherLocals insts
        ((symStSpace, symStackSizeAtEnd), body) =
            fmap (V.fromList . D.toList) $
            first fst $
            runWriter $
            runSymStackHolder $
            mapM_ (step name) insts
        check = if symStackSizeAtEnd == 0 then id
                else error . badStackAtEnd symStackSizeAtEnd . Program
        frame = mkFrame args vars symStSpace
        post  = [ resolveMemRefs frame
                , fixMemRefs
                , mkStackShift (evalStackShift frame)
                , insertExit
                , optimize
                , check
                ] :: [Insts -> Insts]
    in foldl (&) body post
  where
    badStackAtEnd =
        formatToString ("Wrong sym stack size at end: "%int%"\n"%build)

type TransMonad = SymStackHolder (Writer (D.DList Inst))

step :: Var -> SM.Inst -> TransMonad ()
step calleeName = \case
    SM.Nop        -> return ()
    SM.Push v     -> do
        op <- allocSymStackOp
        tell [Mov (Const v) op]
    SM.Drop       -> void popSymStackOp
    SM.Dup        -> do
        from <- peekSymStackOp
        to <- allocSymStackOp
        tell [Mov from eax, Mov eax to]
    SM.Load v     -> do
        op <- allocSymStackOp
        tell [Mov (Local v) eax, Mov eax op]
    SM.Store v    -> do
        op <- popSymStackOp
        tell [Mov op eax, Mov eax (Local v)]
    SM.Bin op     -> do
        op2 <- popSymStackOp
        op1 <- popSymStackOp
        resOp <- allocSymStackOp
        tell $ binop op1 op2 op
        tell [Mov op2 eax, Mov eax resOp]
    SM.ArrayMake k -> do
        op <- allocSymStackOp
        tell [Mov (Const $ fromIntegral k) op]
        mkCall "allocate" 1
    SM.ArrayAccess -> do
        i <- popSymStackOp
        a <- popSymStackOp
        e <- allocSymStackOp
        tell
            [ Mov a eax
            , Mov i edx
            , Mov (HeapMemExt eax edx) eax
            , Mov eax e
            ]
    SM.ArraySet (fromIntegral -> i) -> do
        e <- popSymStackOp
        a <- popSymStackOp
        tell
            [ Mov a eax
            , BinOp "addl" (Const $ i * 4) eax
            , Mov e edx
            , Mov edx (HeapMem eax)
            ]
    SM.Label lid -> tell [Label lid]
    SM.Jmp   lid -> tell [jmp (SM.CLabel lid)]
    SM.JmpIf lid -> do
        op <- popSymStackOp
        tell
            [ Mov op eax
            , BinOp "cmp" (Const 0) eax
            , Jmp "ne" (SM.CLabel lid)
            ]
    SM.Call (FunSign name args) -> mkCall name (length args)
    SM.Ret -> do
        op <- popSymStackOp
        tell [Mov op eax, jmp (SM.ELabel calleeName)]
    SM.Enter{} -> tell [NoopOperator "int3"]
  where
    mkCall name argsNum =
        void . mfix $ \toBackup ->
        backupingOps toBackup $ censor (withStackSpace argsNum) $ do
            forM_ @[] [0 .. argsNum - 1] $ \i -> do
                op <- popSymStackOp
                tell
                    [ Mov op eax    -- TODO: with nice 'inRegs' :()
                    , Mov eax (HardMem i)
                    ]
            tell [Call name]

            toBackup' <- occupiedRegs

            op <- allocSymStackOp
            tell [Mov eax op]
            return toBackup'

separateFuns :: SM.Insts -> [SM.Insts]
separateFuns insts =
    case V.findIndex (\(i, v) -> isEnter v && i > 0) $ V.indexed insts of
        Nothing -> [insts]
        Just k  -> let (funInsts, remaining) = V.splitAt k insts
                   in  funInsts : separateFuns remaining
  where
    isEnter SM.Enter{} = True
    isEnter _          = False

gatherLocals :: SM.Inst -> S.Set Var
gatherLocals (SM.Store v) = [v]
gatherLocals _            = []

afterFunBeginning :: Lens' Insts Insts
afterFunBeginning f insts =
    let funLabelPos = fromMaybe (error "No fun label!")
                    $ V.findIndex isFunLabel insts
        (prelude, body) = V.splitAt (funLabelPos + 1) insts
    in  f body <&> \body' -> prelude <> body'
  where
    isFunLabel (Label (SM.FLabel _)) = True
    isFunLabel _                     = False

insertExit :: Insts -> Insts
insertExit = (<> [ret])

mkStackShift :: Int -> Insts -> Insts
mkStackShift 0     = id
mkStackShift shift = afterFunBeginning %~ withStackSpace shift

-- | Function 'step', when sets `Mem` indices, doesn't take into account
-- possible @Push@es and @Pop@s to stack. This functions fixes it.
--
-- This is done as post-processing because, unlike SM, in X86 (real) stack is
-- influenced by not many operations.
fixMemRefs :: Traversable f => f Inst -> f Inst
fixMemRefs insts =
    let (res, finalStackShift) = flip runState 0 $ forM insts $ \case
            op@(Push _)    -> (id += 1) $> op
            op@(Pop  _)    -> (id -= 1) $> op
            op@(ResizeStack Forward k)
                           -> (id += k) $> op
            op@(ResizeStack Backward k)
                           -> (id -= k) $> op
            (Mov o1 o2)    -> Mov <$> fixMemRef o1 <*> fixMemRef o2
            op             -> return op
    in  case finalStackShift of
        0     -> res
        extra -> error $ "Detected extra values at stack at the end: "
                       ++ show extra

  where
    fixMemRef (Mem i) = Mem . (i +) <$> get
    fixMemRef other   = return other

-- | Copies given operands to backup space.
-- It's essetial for this operation to not influence on sym stack.
-- Also, it should not be strict on first argument (for the sake of RecursiveDo MADNESS)
backupingOps :: (MonadWriter r m, InstContainer r) => [Operand] -> m a -> m a
backupingOps ops trans = do
    let copyWith f = fromList $ uncurry f <$> zip ops (Backup <$> [0..])
    tell $ copyWith Mov         // "buckup"
    res <- trans
    tell $ copyWith (flip Mov)  // "restore"
    return res

inRegsWith
    :: InstContainer r
    => Operand         -- ^ operand to temporally store argument in
    -> Operand         -- ^ argument
    -> (Operand -> r)  -- ^ action with given argument, placed in register
    -> r
inRegsWith _   op@(Reg _)   f = f op
inRegsWith aux op@(Const _) f = [Mov op aux] <> f aux
inRegsWith aux op           f = [Mov op aux] <> f aux <> [Mov aux op]

inRegs1 :: InstContainer r => Operand -> (Operand -> r) -> r
inRegs1 = inRegsWith eax

-- inRegs2 :: Operand -> Operand -> (Operand -> Operand -> [Inst]) -> [Inst]
-- inRegs2 op1 op2 f = inRegsWith eax op1 $ inRegsWith edx op2 . f

binop :: InstContainer r => Operand -> Operand -> Text -> r
binop op1 op2 = \case
    "+" -> inRegs1 op1 $ \op1' -> [BinOp "addl" op1' op2]
    "-" -> inRegs1 op1 $ \op1' -> [BinOp "subl" op2 op1', Mov op1' op2]
    "*" -> inRegs1 op2 $ \op2' -> [BinOp "imull" op1 op2']
    "/" -> idiv eax
    "%" -> idiv edx

    "<"  -> cmp "l"
    ">"  -> cmp "g"
    "<=" -> cmp "le"
    ">=" -> cmp "ge"
    "==" -> cmp "e"
    "!=" -> cmp "ne"

    "^" -> inRegs1 op1 $ \op1' -> [BinOp "xorl" op1' op2]
    "&" -> inRegs1 op1 $ \op1' -> [BinOp "andl" op1' op2]
    "|" -> inRegs1 op1 $ \op1' -> [BinOp "orl" op1' op2]
    "&&" -> "&&" ?
        [ BinOp "xor" eax eax
        , Mov op1 edx
        , BinOp "andl" edx edx
        , UnaryOp "setne" (Reg "ah")
        , Mov op2 edx
        , BinOp "andl" edx edx
        , UnaryOp "setne" (Reg "al")
        , BinOp "and" (Reg "ah") (Reg "al")
        , BinOp "xor" (Reg "ah") (Reg "ah")
        , Mov eax op2
        ]
    "||" -> inRegsWith edx op2 $ \op2' -> "||" ?
        [ BinOp "xor" eax eax
        , BinOp "orl" op1 op2'
        , UnaryOp "setne" (Reg "al")
        , Mov eax op2'
        ]

    unknown -> error $ "Unsupported operation: " ++ show unknown
  where
    idiv res = "/" ?
        [ Mov op1 eax
        , NoopOperator "cdq"
        , UnaryOp "idivl" op2
        , Mov res op2
        ]
    cmp kind = inRegsWith edx op2 $ \op2' -> "cmp" ?
        [ BinOp "xor" eax eax
        , BinOp "cmp" op2' op1
        , Set kind (Reg "al")
        , Mov eax op2'
        ]

produceBinary
    :: MonadIO m
    => FilePath
    -> FilePath
    -> Insts
    -> m (Either Text ())
produceBinary runtimePath outputPath insts = liftIO $ do
    let cmd = proc "g++"
            [ "-m32"                       -- for x32
            , runtimePath </> "runtime.o"
            , "-xassembler"                -- specifies language
            , "-"                          -- take source from stdin
            , "-o", outputPath
            ]
    res <- readCreateProcess cmd $ F.formatToString F.build (Program insts)
    return $ first toText res
