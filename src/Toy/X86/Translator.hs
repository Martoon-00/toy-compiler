{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE ViewPatterns    #-}

module Toy.X86.Translator
    ( compile
    , produceBinary
    ) where

import           Control.Lens          (at, ix, (%~), (&), (<&>), (^.), (^?))
import           Control.Monad         (join)
import           Control.Monad.Trans   (MonadIO (..))
import qualified Data.Map              as M
import           Data.Monoid           ((<>))
import qualified Data.Set              as S
import           Data.Text             (Text)
import qualified Data.Vector           as V
import qualified Formatting            as F
import           GHC.Exts              (fromList)
import           System.FilePath.Posix ((</>))
import           System.Process        (proc)

import           Toy.Exp               (FunSign (..), Var)
import qualified Toy.SM                as SM
import           Toy.X86.Data          (Inst (..), Insts, Operand (..), Program (..), eax,
                                        edx, esp, jmp, traverseOperands, (?))
import           Toy.X86.Optimize      (optimize)
import           Toy.X86.SymStack      (SymStackHolder, SymStackSize, allocSymStackOp,
                                        popSymStackOp, runSymStackHolder)
import           Toy.X86.Util          (readCreateProcess)

compile :: SM.Insts -> Insts
compile = mconcat . map compileFun . separateFuns

compileFun :: SM.Insts -> Insts
compileFun insts =
    let args = case insts ^? ix 0 of
            Just (SM.Enter argsInfo) -> argsInfo
            _                        -> error "Where is my Enter?!"
        vars    = foldr S.delete (foldMap gatherLocals insts) args
        ilocals = M.fromList $ zip (S.toList vars ++ args) [0..]
        (symStackSize, body) = fmap (join . fmap fromList)
                             $ runSymStackHolder $ mapM (step ilocals) insts
        post    = [ resolveStackRefs symStackSize
                  , mkStackShift $ length ilocals + fromIntegral symStackSize
                  , optimize
                  ] :: [Insts -> Insts]
    in  foldl (&) body post

-- TODO: correct errors processing
step :: M.Map Var Int -> SM.Inst -> SymStackHolder [Inst]
step locals = \case
    SM.Nop        -> pure []
    SM.Push v     -> allocSymStackOp <&> \op -> [Mov (Const v) op]
    SM.Load v     ->
        case locals ^. at v of
            Nothing  -> error "No such variable"
            Just idx -> allocSymStackOp <&> \op -> [Mov (Mem idx) eax, Mov eax op]
    SM.Store v    ->
        case locals ^. at v of
            Nothing  -> error "undetected variable!"
            Just idx -> popSymStackOp <&> \op -> [Mov op eax, Mov eax (Mem idx)]
    SM.Read      -> allocSymStackOp <&> \op -> [Call "read", Mov eax op]
    SM.Write     -> popSymStackOp <&> \op -> [Mov op eax, Mov eax (Stack 0), Call "write"]
    SM.Bin op    -> do
        op2 <- popSymStackOp
        op1 <- popSymStackOp
        resOp <- allocSymStackOp
        return $ binop op1 op2 op <> [Mov op2 eax, Mov eax resOp]
    SM.Label lid -> pure [Label lid]
    SM.Jmp   lid -> pure [jmp (SM.CLabel lid)]
    SM.JmpIf lid -> popSymStackOp <&> \op -> [Mov op eax, BinOp "cmp" (Const 0) eax, Jmp "ne" (SM.CLabel lid)]
    SM.Call (FunSign name _)
                 -> pure [Call name]
    SM.Ret       -> pure [NoopOperator "ret"]
    SM.Enter _   -> pure [NoopOperator "int3"]

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

mkStackShift :: Int -> Insts -> Insts
mkStackShift shift insts =
    let stackSh = Const . fromIntegral $ 4 * shift
        prefix = BinOp "subl" stackSh esp
        suffix = BinOp "addl" stackSh esp
    in  [prefix] <> insts <> [suffix]

resolveStackRefs :: Traversable f => SymStackSize -> f Inst -> f Inst
resolveStackRefs (fromIntegral -> s) = fmap $ traverseOperands %~ resolve
  where
    resolve (Mem a  ) = Mem (a + s)
    resolve (Stack a) = Mem a
    resolve other     = other

inRegsWith
    :: Operand              -- operand to temporally store argument in
    -> Operand              -- argument
    -> (Operand -> [Inst])  -- action with given argument, placed in register
    -> [Inst]
inRegsWith _   op@(Reg _) f = f op
inRegsWith aux op         f = [Mov op aux] <> f aux <> [Mov aux op]

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
