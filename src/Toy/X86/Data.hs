{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE QuasiQuotes #-}

module Toy.X86.Data
    ( Operand (..)
    , Inst (..)
    , Insts
    , Program (..)
    , StackDirection (..)

    , (?)
    , traverseOperands
    , jmp
    , ret

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

import           Toy.Exp                (Value, Var)
import           Toy.SM                 (LabelId)

data Operand
    = Reg String
    -- ^ Register
    | Const Value
    -- ^ Constant
    | Mem Int
    -- ^ Memory reference. This keeps amount of /qword/s to look back on stack
    | Stack Int
    -- ^ Stack reference. Temporally used in conversion from symbolic stack
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
    build (Stack _) = error "Stack reference remains upon compilation"

data StackDirection
    = Backward
    | Forward
    deriving (Show, Eq)

data Inst
    = Mov Operand Operand
    | Push Operand
    | Pop Operand
    | BinOp Text Operand Operand
    | UnaryOp Text Operand
    | NoopOperator Text
    | Set Text Operand
    | Comment Text
    | Label LabelId
    | Jmp Text LabelId
    | Call Var
    | ResizeStack StackDirection Int
    deriving (Show, Eq)

jmp :: LabelId -> Inst
jmp = Jmp "mp"

ret :: Inst
ret = NoopOperator "ret"

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
        Label lid       -> bprint (F.build%":") lid
        Jmp kind lid    -> bprint ("j"%pad%" "%pad) kind lid
        ResizeStack d k ->
            let op    = case d of { Backward -> "addl"; Forward -> "subl" }
                shift = Const . fromIntegral $ k * 4
            in build $ BinOp op shift esp
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

|]

traverseOperands :: Applicative f => (Operand -> f Operand) -> Inst -> f Inst
traverseOperands f (Mov a b)         = Mov <$> f a <*> f b
traverseOperands f (Push k )         = Push <$> f k
traverseOperands f (Pop k  )         = Pop <$> f k
traverseOperands _ o@Call{}          = pure o
traverseOperands f (BinOp o op1 op2) = BinOp o <$> f op1 <*> f op2
traverseOperands f (UnaryOp o op)    = UnaryOp o <$> f op
traverseOperands _ o@NoopOperator{}  = pure o
traverseOperands f (Set k op)        = Set k <$> f op
traverseOperands _ o@Comment{}       = pure o
traverseOperands _ o@Label{}         = pure o
traverseOperands _ o@Jmp{}           = pure o
traverseOperands _ o@ResizeStack{}   = pure o
