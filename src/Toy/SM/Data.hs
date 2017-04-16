{-# LANGUAGE TemplateHaskell #-}

module Toy.SM.Data where

import           Control.Lens        (makeLenses)
import           Data.Default        (Default (..))
import qualified Data.Map            as M
import           Data.Text.Buildable (Buildable (..))
import qualified Data.Vector         as V
import           Formatting          (bprint, (%))
import qualified Formatting          as F

import           Toy.Exp             (BinOp, LocalVars, Value, Var)

type IP = Int

data LabelId
    = CLabel Int  -- control
    | FLabel Var  -- function
    deriving (Eq, Ord, Show)

instance Buildable LabelId where
    build (CLabel l) = bprint ("L"%F.build) l
    build (FLabel n) = bprint ("F"%F.build) n

-- | Statement of a program.
data Inst
    = Push Value
    | Pop
    | Bin BinOp
    | Load Var
    | Store Var
    | Read
    | Write
    | Label LabelId
    | Jmp LabelId
    | JmpIf LabelId
    | Call LabelId
    | Nop
    deriving (Eq, Show)

type Insts = V.Vector Inst

-- | State of execution
data ExecState = ExecState
    { _esLocals :: LocalVars
      -- ^ local variables values
    , _esStack  :: [Value]
      -- ^ current stack
    , _esIp     :: IP
      -- ^ instruction pointer, no of command to execute next
    } deriving (Eq, Show)

makeLenses ''ExecState

instance Default ExecState where
    def = ExecState M.empty [] 0
