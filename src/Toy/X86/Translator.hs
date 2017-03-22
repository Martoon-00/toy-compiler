{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE TypeFamilies     #-}

module Toy.X86.Translator
    ( compile
    , produceBinary
    ) where

import           Control.Lens        (at, (+=), (-=), (^.))
import           Control.Monad       (forM)
import           Control.Monad.State (get, runState)
import           Control.Monad.Trans (MonadIO (..))
import           Data.Functor        (($>))
import qualified Data.Map            as M
import           Data.Monoid         ((<>))
import qualified Data.Set            as S
import           Data.Text           (Text)
import qualified Formatting          as F
import           GHC.Exts            (fromList)
import           System.Process      (proc)

import           Toy.Exp             (Var)
import qualified Toy.SM              as SM
import           Toy.X86.Data        (Inst (..), Insts, Operand (..), Program (..), eax,
                                      edi, edx, esi, esp)
import           Toy.X86.Util        (readCreateProcess)

compile :: SM.Insts -> Insts
compile insts =
    let locals  = foldMap gatherLocals insts
        ilocals = M.fromList $ flip zip [0..] $ S.toList locals
        stackSh = Const $ 4 * length locals
        prefix  = [BinOp "subl" stackSh esp]
        suffix  = [BinOp "addl" stackSh esp]
        body    = fromList . step ilocals =<< insts
        body'   = fixMemRefs body
    in  prefix <> body' <> suffix

-- TODO: rewrite with smart pseudo-stack
-- TODO: correct errors processing
step :: M.Map Var Int -> SM.Inst -> [Inst]
step locals = \case
    SM.Nop     -> []
    SM.Push v  -> [Mov (Const v) eax, Push eax]
    SM.Load v  ->
        case locals ^. at v of
            Nothing  -> error "No such variable"
            Just idx -> [Mov (Mem idx) eax, Push eax]
    SM.Store v ->
        case locals ^. at v of
            Nothing  -> error "undetected variable!"
            Just idx -> [Pop eax, Mov eax (Mem idx)]
    SM.Read   -> [Call "read", Push eax]
    SM.Write  -> [Pop eax, Call "write"]
    SM.Bin op -> [Pop op2, Pop op1] <> binop op <> [Push op2]

-- | Function 'step', when sets `Mem` indices, doesn't take into account
-- possible @Push@es and @Pop@s to stack. This functions fixes it.
--
-- This is done as post-processing because, unlike SM, in X86 (real) stack is
-- influenced by only two operations.
fixMemRefs :: Traversable f => f Inst -> f Inst
fixMemRefs insts =
    let (res, finalStackShift) = flip runState 0 $ forM insts $ \case
            op@(Push _)   -> (id += 1) $> op
            op@(Pop  _)   -> (id -= 1) $> op
            (Mov o1 o2) -> Mov <$> fixMemRef o1 <*> fixMemRef o2
            op            -> return op
    in  case finalStackShift of
        0     -> res
        extra -> error $ "Detected extra values at stack at the end: "
                       ++ show extra

  where
    fixMemRef (Mem i) = Mem . (i +) <$> get
    fixMemRef other   = return other

gatherLocals :: SM.Inst -> S.Set Var
gatherLocals (SM.Store v) = [v]
gatherLocals _            = []

-- | Register which is used as operand for binary operations.
op1, op2 :: Operand
op1 = esi
op2 = edi

binop :: Text -> [Inst]
binop = \case
    "+" -> [BinOp "addl" op1 op2]
    "-" -> [BinOp "subl" op1 op2]
    "*" -> [BinOp "imull" op1 op2]
    "/" -> idiv eax
    "%" -> idiv edx

    "<"  -> cmp "l"
    ">"  -> cmp "g"
    "<=" -> cmp "le"
    ">=" -> cmp "ge"
    "==" -> cmp "e"
    "!=" -> cmp "ne"

    unknown -> error $ "Unsupported operation: " ++ show unknown
  where
    idiv res =
        [ Mov op2 eax
        , NoopOperator "ctld"
        , UnaryOp "idiv" op1
        , Mov res op2
        ]
    cmp kind =
        [ BinOp "xor" eax eax
        , BinOp "orl" op1 op2
        , Set kind (Reg "al")
        , Mov eax op2
        ]

-- Executes gcc with flags:
--
-- * @-m32@        - for x32 machine
--
-- * @-xassembler@ - specifies language (required for next option)
--
-- * @-@           - take source from /stdin/
produceBinary
    :: MonadIO m
    => FilePath
    -> FilePath
    -> Insts
    -> m (Either String ())
produceBinary runtimePath outputPath insts = liftIO $ do
    let cmd = proc "gcc"
            ["-m32", runtimePath, "-xassembler", "-", "-o", outputPath]
    readCreateProcess cmd $ F.formatToString F.build (Program insts)
