{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies    #-}

module Toy.X86.Translator
    ( compile
    , produceBinary
    ) where

import           Control.Lens          (Lens', ix, (%~), (&), (+=), (-=), (<&>), (^?))
import           Control.Monad         (forM, join)
import           Control.Monad.State   (get, runState)
import           Control.Monad.Trans   (MonadIO (..))
import           Data.Functor          (($>))
import           Data.Maybe            (fromMaybe)
import           Data.Monoid           ((<>))
import qualified Data.Set              as S
import           Data.Text             (Text)
import qualified Data.Vector           as V
import           Formatting            (build, formatToString, int, (%))
import qualified Formatting            as F
import           GHC.Exts              (fromList)
import           System.FilePath.Posix ((</>))
import           System.Process        (proc)

import           Toy.Exp               (FunSign (..), Var)
import qualified Toy.SM                as SM
import           Toy.X86.Data          (Inst (..), Insts, Operand (..), Program (..),
                                        StackDirection (..), eax, edx, jmp, ret,
                                        withStackSpace, (//), (?))
import           Toy.X86.Frame         (evalStackShift, mkFrame, resolveMemRefs)
import           Toy.X86.Optimize      (optimize)
import           Toy.X86.Process       (readCreateProcess)
import           Toy.X86.SymStack      (SymStackHolder, allocSymStackOp, occupiedRegs,
                                        popSymStackOp, runSymStackHolder)

compile :: SM.Insts -> Insts
compile = mconcat . map compileFun . separateFuns

compileFun :: SM.Insts -> Insts
compileFun insts =
    let (name, args) = case insts ^? ix 0 of
            Just (SM.Enter n ai) -> (n, ai)
            _                    -> error "Where is my Enter?!"
        vars  = foldMap gatherLocals insts
        ((symStSpace, symStackSizeAtEnd), body) = runSymStackHolder $
            join . fmap fromList <$> mapM (step name) insts
        check = if symStackSizeAtEnd == 0 then id
                else error . formatToString ("Wrong sym stack size at end: "%
                     int%"\n"%build) symStackSizeAtEnd . Program
        frame = mkFrame args vars symStSpace
        post  = [ resolveMemRefs frame
                , fixMemRefs
                , mkStackShift (evalStackShift frame)
                , insertExit
                , optimize
                ] :: [Insts -> Insts]
    in check $ foldl (&) body post

-- TODO: correct errors processing
step :: Var -> SM.Inst -> SymStackHolder [Inst]
step calleeName = \case
    SM.Nop        -> pure []
    SM.Push v     -> allocSymStackOp <&> \op -> [Mov (Const v) op]
    SM.Drop       -> popSymStackOp $> []
    SM.Load v     -> allocSymStackOp <&> \op -> [Mov (Local v) eax, Mov eax op]
    SM.Store v    -> popSymStackOp <&> \op -> [Mov op eax, Mov eax (Local v)]
    SM.Bin op    -> do
        op2 <- popSymStackOp
        op1 <- popSymStackOp
        resOp <- allocSymStackOp
        return $ binop op1 op2 op <> [Mov op2 eax, Mov eax resOp]
    SM.Label lid -> pure [Label lid]
    SM.Jmp   lid -> pure [jmp (SM.CLabel lid)]
    SM.JmpIf lid -> popSymStackOp <&> \op ->
        [ Mov op eax
        , BinOp "cmp" (Const 0) eax
        , Jmp "ne" (SM.CLabel lid)
        ]
    SM.Call (FunSign name args) -> do
        part1 <- rolloutSymStackOps (length args) [Call name]
        part2 <- allocSymStackOp <&> \op -> [Mov eax op]
        return $ part1 <> part2
    SM.Ret       -> do
        p1 <- popSymStackOp <&> \op -> [Mov op eax]
        return $ p1 <> [jmp (SM.ELabel calleeName)]
    SM.Enter{}   -> pure [NoopOperator "int3"]

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

-- | Puts symbolic stack on real stack. Symbolic stack becomes empty
rolloutSymStackOps :: Int -> [Inst] -> SymStackHolder [Inst]
rolloutSymStackOps argsNum insts = do
    rolling <- fmap mconcat . forM [0 .. argsNum - 1] $ \i ->
        popSymStackOp <&> \op ->
            [ Mov op eax                       -- TODO: with nice 'inRegs' :()
            , Mov eax (HardMem i)
            ]
    toBackup <- occupiedRegs
    return . backupingOps toBackup $ withStackSpace argsNum $
        rolling <> insts

backupingOps :: [Operand] -> [Inst] -> [Inst]
backupingOps ops insts =
    let assoc = zip ops (Backup <$> [0..])
        backup  = map (uncurry Mov) assoc
        restore = map (uncurry $ flip Mov) assoc
    in  mconcat
        [ backup  // "buckup"
        , insts
        , restore  // "restore"
        ]

inRegsWith
    :: Operand              -- operand to temporally store argument in
    -> Operand              -- argument
    -> (Operand -> [Inst])  -- action with given argument, placed in register
    -> [Inst]
inRegsWith _   op@(Reg _)   f = f op
inRegsWith aux op@(Const _) f = [Mov op aux] <> f aux
inRegsWith aux op           f = [Mov op aux] <> f aux <> [Mov aux op]

inRegs1 :: Operand -> (Operand -> [Inst]) -> [Inst]
inRegs1 = inRegsWith eax

-- inRegs2 :: Operand -> Operand -> (Operand -> Operand -> [Inst]) -> [Inst]
-- inRegs2 op1 op2 f = inRegsWith eax op1 $ inRegsWith edx op2 . f

binop :: Operand -> Operand -> Text -> [Inst]
binop op1 op2 = \case
    "+" -> inRegs1 op1 $ \op1' -> [BinOp "addl" op1' op2]
    "-" -> inRegs1 op1 $ \op1' -> [BinOp "subl" op2 op1', Mov op1' op2]  -- TODO: ???
    "*" -> inRegs1 op2 $ \op2' -> [BinOp "imull" op1 op2']
    "/" -> idiv eax
    "%" -> idiv edx

    "<"  -> cmp "g"  -- TODO: ?????
    ">"  -> cmp "l"
    "<=" -> cmp "ge"
    ">=" -> cmp "le"
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
        , BinOp "cmp" op1 op2'
        , Set kind (Reg "al")
        , Mov eax op2'
        ]

produceBinary
    :: MonadIO m
    => FilePath
    -> FilePath
    -> Insts
    -> m (Either String ())
produceBinary runtimePath outputPath insts = liftIO $ do
    let cmd = proc "gcc"
            ["-m32"                        -- for x32
            , runtimePath </> "runtime.o"
            , "-xassembler"                -- specifies language
            , "-"                          -- take source from stdin
            , "-o", outputPath
            ]
    readCreateProcess cmd $ F.formatToString F.build (Program insts)
