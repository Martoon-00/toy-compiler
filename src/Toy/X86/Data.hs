{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TypeFamilies    #-}

module Toy.X86.Data
    ( Operand (..)
    , Inst (..)
    , Insts
    , Program (..)
    , StackDirection (..)

    , (?)
    , (//)
    , jmp
    , ret

    , eax
    , edx
    , esi
    , edi
    , esp

    , withStackSpace
    , traverseOperands

    , InstContainer
    ) where

import           Data.Int               (Int32)
import           Data.List              (intersperse)
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import           Data.Text.Buildable    (Buildable (..))
import           Data.Text.Lazy.Builder (Builder)
import qualified Data.Vector            as V
import           Formatting             (bprint, int, stext, (%))
import qualified Formatting             as F
import           GHC.Exts               (IsList (..))
import           GHC.Exts               (toList)
import           Prelude                hiding (null, unlines)
import qualified Text.RawString.QQ      as QQ
import           Universum              (Container (null), One (..))

import           Toy.Base               (Var)
import           Toy.SM                 (JmpLabelForm (..), LabelId)

-- TODO: Aaa, 100500 constructors. Decrease?
data Operand
    = Reg Text
    -- ^ Register
    | Const Int32
    -- ^ Constant
    | Mem Int
    -- ^ Memory reference. This keeps amount of /qword/s to look back on stack
    | HardMem Int
    -- ^ Like `Mem`, but won't be moved by 'Toy.X86.Translator.fixMemRefs'
    | HeapMem Operand
    -- ^ Memory reference to heap. Operand must be register  -- TODO: force by types?
    | HeapMemExt Operand Operand
    -- ^ Memory reference to heap (a + 4 * b). Operands must be registers
    | Local Var
    -- ^ Reerence to local variable
    | Stack Int
    -- ^ Stack reference. Temporally used in conversion from symbolic stack
    | Backup Int
    -- ^ Space for registers backup on function call
    deriving (Show, Eq)

eax, edx, esi, edi, esp :: Operand
eax = Reg "eax"
edx = Reg "edx"
esi = Reg "esi"
edi = Reg "edi"
esp = Reg "esp"

instance Buildable Operand where
    build (Reg   r)        = "%" <> build r
    build (Const v)        = "$" <> build (toInteger v)
    build (Mem   i)        = bprint (int%"("%F.build%")") (4 * i) esp
    build (HardMem i)      = build (Mem i)
    build (HeapMem b)      = bprint ("("%F.build%")") b
    build (HeapMemExt a b) = bprint ("("%F.build%", "%F.build%", 4)") a b
    build (Local _)        = error "Local var reference remains upon compilation"
    build (Stack _)        = error "Stack reference remains upon compilation"
    build (Backup _)       = error "Backup reference remains upon compilation"

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

(?) :: InstContainer l => Text -> l -> l
(?) comment insts
    | null insts = mempty
    | otherwise = mconcat
        [ one $ Comment comment
        , insts
        , one $ Comment (comment <> " end")
        ]

(//) :: InstContainer l => l -> Text -> l
(//) = flip (?)

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
        Comment d       -> bprint ("# "%pad) d
        Label lid       -> bprint (F.build%":") lid
        Jmp kind lid    -> bprint ("j"%pad%" "%pad) kind (JmpLabelForm lid)
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
        prefix <> mconcat (("\t" <>) . (<> "\n") . build <$> toList insts)
      where
        prefix = [QQ.r|
.section     .text
.global      main

|]

withStackSpace
    :: InstContainer l => Int -> l -> l
withStackSpace 0 insts = insts
withStackSpace k insts = mconcat
    [ one $ ResizeStack Forward k
    , insts
    , one $ ResizeStack Backward k
    ]

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

type InstContainer l =
    ( Monoid l
    , Container l
    , One l
    , IsList l
    , Item l ~ Inst
    , OneItem l ~ Inst
    )
