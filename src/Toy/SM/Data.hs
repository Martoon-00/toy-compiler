{-# LANGUAGE TemplateHaskell #-}

module Toy.SM.Data where

import           Control.Lens (makeLenses)
import           Data.Default (Default (..))
import qualified Data.Map     as M
import qualified Data.Vector  as V

import           Toy.Exp      (BinOp, LocalVars, Value, Var)

type IP = Int

type LabelId = Int

-- | Statement of a program.
data Inst
    = Push Value
    | Bin BinOp
    | Load Var
    | Store Var
    | Read
    | Write
    | Label LabelId
    | Jmp LabelId
    | JmpIf LabelId
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
