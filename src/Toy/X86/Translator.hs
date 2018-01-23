{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies    #-}

module Toy.X86.Translator
    ( compile
    , produceBinary
    ) where

import           Control.Lens          (ix, (+=), (-=))
import           Control.Monad         (forM_)
import           Control.Monad.State   (get, runState)
import           Control.Monad.Trans   (MonadIO (..))
import           Control.Monad.Writer  (MonadWriter, Writer, censor, runWriter, tell)
import qualified Data.DList            as D
import           Data.Functor          (($>))
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           ((<>))
import qualified Data.Vector           as V
import           Formatting            (build, int, sformat, (%))
import qualified Formatting            as F
import           GHC.Exts              (fromList)
import           System.FilePath.Posix ((</>))
import           System.Process        (proc)
import           Universum             hiding (Const, forM_)

import           Toy.Base              (FunSign (..), Value)
import qualified Toy.SM                as SM
import           Toy.X86.Data          (Inst (..), InstContainer, Insts, Operand (..),
                                        Program (..), StackDirection (..), eax, edx, jmp,
                                        ret, withStackSpace, (//), (?))
import           Toy.X86.Frame         (evalStackShift, mkFrame, resolveMemRefs)
import           Toy.X86.Optimize      (optimize)
import           Toy.X86.Process       (readCreateProcess)
import           Toy.X86.SymStack      (SymStackHolder, accountHardMem, allocSymStackOp,
                                        occupiedRegs, peekSymStackOp, popSymStackOp,
                                        runSymStackHolder)


compile :: SM.Insts -> Insts
compile = mconcat . map compileFun . separateFuns

compileFun :: SM.Insts -> Insts
compileFun insts =
    let (name, args) = case insts ^? ix 0 of
            Just (SM.Enter n ai) -> (n, ai)
            _                    -> error "Where is my Enter?!"
        vars  = SM.gatherLocals insts
        ((symStSpace, symStackSizeAtEnd), body) =
            fmap (V.fromList . D.toList) $
            first fst $
            runWriter $
            runSymStackHolder $
            mapM_ step insts
        check = if symStackSizeAtEnd == 0 then identity
                else error . badStackAtEnd symStackSizeAtEnd . Program
        frame = mkFrame args vars symStSpace
        post  = [ resolveMemRefs frame
                , fixMemRefs
                , mkStackShift (evalStackShift frame)
                , (<> [ret])
                , optimize
                , check
                , (("Function " <> show name) ?)
                ] :: [Insts -> Insts]
    in foldl (&) body post
  where
    badStackAtEnd = sformat ("Wrong sym stack size at end: "%int%"\n"%build)

type TransMonad = SymStackHolder (Writer (D.DList Inst))

step :: SM.Inst -> TransMonad ()
step = \case
    SM.Nop        -> return ()
    SM.Push v     -> do
        op <- allocSymStackOp
        tell [Mov (Const $ nTo31 v) op]
    SM.PushNull   -> do
        op <- allocSymStackOp
        tell [Mov (Const 0) op]
    SM.Drop       -> void popSymStackOp -- >>= killReference
    SM.Dup        -> do
        from <- peekSymStackOp
        to <- allocSymStackOp
        tell [Mov from eax, Mov eax to]
    SM.LoadNoGc v     -> do
        op <- allocSymStackOp
        tell [Mov (Local v) eax, Mov eax op]
    SM.Load v -> do
        step (SM.LoadNoGc v)
        peekSymStackOp >>= addReference
    SM.StoreInit v ->
        tell [Mov (Const 0) eax, Mov eax (Local v)]
    SM.Store v    -> do
        -- take care of @x <- x@
        op <- popSymStackOp
        tell [Mov op eax, Mov (Local v) edx, Mov eax (Local v), Mov edx op]
        killReference op
    SM.Bin op     -> do
        op2 <- popSymStackOp
        op1 <- popSymStackOp
        resOp <- allocSymStackOp
        tell $ binop op1 op2 op
        tell [Mov op2 eax, Mov eax resOp]
    SM.ArrayMake -> do
        mkCall "arrmakeu" 1 False
    SM.ArrayAccess -> censor ("array access" ?) $ do
        i   <- popSymStackOp
        a   <- popSymStackOp
        e   <- allocSymStackOp  -- @e@ shouldn't shadow @a@
        tmp <- allocSymStackOp
        tell $ from31 i
        tell
            [ Mov a eax
            , Mov i edx
            , Mov (HeapMemExt eax edx) eax
            , Mov eax tmp
            ]
        killReference a  -- TODO: what if @e@ expires here as well?
        tell
            [ Mov tmp eax, Mov eax e
            ]
        addReference e
        void popSymStackOp
    SM.ArraySet -> censor ("array set" ?) $ do
        e <- popSymStackOp
        i <- popSymStackOp
        a <- popSymStackOp
        replicateM_ 2 allocSymStackOp  -- preserve for gc
        tell $ from31 i
        tell
            [ Mov a eax
            , Mov i edx
            , BinOp "leal" (HeapMemExt eax edx) eax

            , Mov (HeapMem eax) edx
            , Mov edx i  -- using i as tmp var

            , Mov e edx
            , Mov edx (HeapMem eax)
            ]
        killReference i
        killReference a
        replicateM_ 2 popSymStackOp  -- preserve for gc
    SM.Label lid -> tell [Label lid]
    SM.Jmp   lid -> tell [jmp lid]
    SM.JmpIf lid -> do
        op <- popSymStackOp
        tell
            [ Mov op eax
            , BinOp "cmp" (Const $ nTo31 0) eax
            , Jmp "ne" lid
            ]
    SM.Call (FunSign name args) -> mkCall name (length args) True
    SM.JumpToFunEnd -> do
        tell [jmp SM.exitLabel]
    SM.FunExit -> do
        op <- popSymStackOp
        tell [Mov op eax]
        -- ret is performed outside because of stack shift
    SM.Enter{} -> tell
        [ NoopOperator "int3"  -- don't step out!
        ]
  where
    -- @doGc@ - whether to clean function arguments after call
    mkCall name argsNum doGc = censor ("call" ?) $ do
        toBackupWithoutRes <- occupiedRegs
        stackArgOps <- replicateM argsNum popSymStackOp
        replicateM_ argsNum allocSymStackOp  -- preserve them for gc
        tmpTop <- allocSymStackOp

        accountHardMem argsNum

        backupingOps toBackupWithoutRes $ censor (withStackSpace argsNum) $ do
            forM_ (zip [0..] stackArgOps) $ \(i, op) ->
                tell [Mov op eax, Mov eax (HardMem i)]
            tell [Call name]

            tell [Mov eax tmpTop]

        when doGc $ do
            forM_ stackArgOps $ \op -> do
                toBackupWithRes <- occupiedRegs
                backupingOps toBackupWithRes $
                    censor (("gc args" ?) . withStackSpace 1) $
                    tell
                        [ Mov op eax
                        , Mov eax (HardMem 0)
                        , Call "ref_counter_decrement"
                        ]
            censor ("ref cleaup" ?) $
                forM_ stackArgOps $ \op ->
                    tell
                        [ Mov (Const 0) eax
                        , Mov eax op
                        ]

        replicateM_ argsNum popSymStackOp
        op <- peekSymStackOp
        tell [Mov tmpTop eax, Mov eax op]

    singleOpCall funName op = do
        op' <- allocSymStackOp
        tell [Mov op eax, Mov eax op']
        mkCall funName 1 False
        void popSymStackOp
    addReference = singleOpCall "ref_counter_increment"
    killReference = singleOpCall "ref_counter_decrement"

separateFuns :: SM.Insts -> [SM.Insts]
separateFuns insts =
    case V.findIndex (\(i, v) -> isEnter v && i > 0) $ V.indexed insts of
        Nothing -> [insts]
        Just k  -> let (funInsts, remaining) = V.splitAt k insts
                   in  funInsts : separateFuns remaining
  where
    isEnter SM.Enter{} = True
    isEnter _          = False

afterFunBeginning :: Lens' Insts Insts
afterFunBeginning f insts =
    let funLabelPos = fromMaybe (error "No fun label!")
                    $ V.findIndex isFunLabel insts
        (prelude, body) = V.splitAt (funLabelPos + 1) insts
    in  f body <&> \body' -> prelude <> body'
  where
    isFunLabel (Label (SM.FLabel _)) = True
    isFunLabel _                     = False

mkStackShift :: Int -> Insts -> Insts
mkStackShift 0     = identity
mkStackShift shift = afterFunBeginning %~ withStackSpace shift

-- | Function 'step', when sets `Mem` indices, doesn't take into account
-- possible @Push@es and @Pop@s to stack. This functions fixes it.
--
-- This is done as post-processing because, unlike SM, in X86 (real) stack is
-- influenced by not many operations.
fixMemRefs :: Traversable f => f Inst -> f Inst
fixMemRefs insts =
    let (res, finalStackShift) = flip runState 0 $ forM insts $ \case
            op@(Push _)    -> (identity += 1) $> op
            op@(Pop  _)    -> (identity -= 1) $> op
            op@(ResizeStack Forward k)
                           -> (identity += k) $> op
            op@(ResizeStack Backward k)
                           -> (identity -= k) $> op
            (Mov o1 o2)    -> Mov <$> fixMemRef o1 <*> fixMemRef o2
            op             -> return op
    in  case finalStackShift of
        0     -> res
        extra -> error $ "Detected extra values at stack at the end: "
                       <> show extra

  where
    fixMemRef (Mem i) = Mem . (i +) <$> get
    fixMemRef other   = return other

-- | Copies given operands to backup space.
-- It's essetial for this operation to not influence on sym stack.
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

-- | Apply operations to work with 31-byte arithmetics
in31BA :: InstContainer r => Operand -> Operand -> r -> r
in31BA op1 op2 insts = mconcat
    [ from31 op1, from31 op2
    , insts
    , to31 op1, to31 op2
    ]

-- Convertions between 31-bit (with lowest bit = 1) numbers and normal numbers.
-- Normal number: sign  sign  value
--                  1     1     30
-- 31-bit number: sign  value   1
--                  1     30    1
from31, to31 :: InstContainer r => Operand -> r
from31 op = "from31" ?
    [ UnaryOp "sarl" op
    ]
to31 op   = "to31" ?
    [ Mov op eax
    , BinOp "andl" (Const $ 2 ^ (31 :: Int)) eax
    , UnaryOp "sall" op
    , BinOp "addl" (Const 1) op
    , Mov (Const $ 2 ^ (31 :: Int) - 1) edx
    , BinOp "andl" edx op
    , BinOp "addl" eax op
    ]

nTo31 :: Value -> Int32
nTo31 x = fromIntegral x * 2 + 1

binop :: InstContainer r => Operand -> Operand -> Text -> r
binop op1 op2 action =
    (action ?) . in31BA op1 op2 $
    case action of
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
        "&&" ->
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
        "||" -> inRegs1 op2 $ \op2' ->
          [ BinOp "xor" edx edx
          , BinOp "orl" op1 op2'
          , UnaryOp "setne" (Reg "dl")
          , Mov edx op2'
          ]

        unknown -> error $ "Unsupported operation: " <> show unknown
  where
    idiv res =
        [ Mov op1 eax
        , NoopOperator "cdq"
        , UnaryOp "idivl" op2
        , Mov res op2
        ]
    cmp kind = inRegs1 op2 $ \op2' ->
        [ BinOp "xor" edx edx
        , BinOp "cmp" op2' op1
        , Set kind (Reg "dl")
        , Mov edx op2'
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
