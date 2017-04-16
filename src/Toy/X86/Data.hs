{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE QuasiQuotes #-}

module Toy.X86.Data
    ( Operand (..)
    , Inst (..)
    , Insts
    , Program (..)
    , (?)
    , jmp
    , eax
    , edx
    , esi
    , edi
    , esp
    ) where

import           Control.Lens           (Cons, cons)
import           Data.List              (intersperse)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import           Data.Text.Buildable    (Buildable (..))
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Vector            as V
import           Formatting             (bprint, int, stext, (%))
import qualified Formatting             as F
import           GHC.Exts               (toList)
import           Prelude                hiding (unlines)
import qualified Text.RawString.QQ      as QQ

import           Toy.Exp                (Value)
import           Toy.SM                 (LabelId)

data Operand
    = Reg String
    -- ^ Register
    | Const Value
    -- ^ Constant
    | Mem Int
    -- ^ Memory reference. This keeps amount of /qword/s to look back on stack
    deriving (Show, Eq)

eax, edx, esi, edi, esp :: Operand
eax = Reg "eax"
edx = Reg "edx"
esi = Reg "esi"
edi = Reg "edi"
esp = Reg "esp"

instance Buildable Operand where
    build (Reg   r) = "%" <> build r
    build (Const v) = "$" <> build v
    build (Mem   i) = bprint (int%"("%F.build%")") (4 * i) esp

-- not very beautiful :(
data Inst
    = Mov Operand Operand
    | Push Operand
    | Pop Operand
    | Call Text
    | BinOp Text Operand Operand
    | UnaryOp Text Operand
    | NoopOperator Text
    | Set Text Operand
    | Comment Text
    | Label LabelId
    | Jmp Text LabelId
    deriving (Show, Eq)

jmp :: LabelId -> Inst
jmp = Jmp "mp"

(?) :: Cons s s Inst Inst => Text -> s -> s
(?) comment = cons $ Comment comment

buildInst :: Buildable b => Text -> [b] -> Builder
buildInst name ops =
    bprint (stext%"\t"%F.builder) name $
    mconcat $ intersperse ",\t" $ build <$> ops

instance Buildable Inst where
    build = \case
        Mov op1 op2     -> buildInst "movl" [op1, op2]
        Push op1        -> buildInst "pushl" [op1]
        Pop  op1        -> buildInst "popl" [op1]
        Call name       -> buildInst "call" [name]
        BinOp o op1 op2 -> buildInst o [op1, op2]
        UnaryOp o op    -> buildInst o [op]
        NoopOperator o  -> build o
        Set kind op     -> bprint ("set"%pad%" "%pad) kind op
        Comment d       -> bprint ("\t# "%pad) d
        Label lid       -> bprint ("L"%F.build%":") lid
        Jmp kind lid    -> bprint ("j"%pad%" L"%pad) kind lid
      where
        pad :: F.Buildable a => F.Format r (a -> r)
        pad = F.right 6 ' '

type Insts = V.Vector Inst

newtype Program = Program Insts
    deriving (Eq, Show)

instance Buildable Program where
    build (Program insts) =
        prefix <> mconcat (("\t" <>) . (<> "\n") . build <$> (toList insts))
      where
        prefix = [QQ.r|
.section     .text
.global      main

main:
|]
