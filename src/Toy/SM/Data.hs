{-# LANGUAGE TemplateHaskell #-}

module Toy.SM.Data where

import           Control.Lens (makeLenses)
import qualified Data.Map     as M
import qualified Data.Vector  as V

import           Toy.Exp      (BinOp, LocalVars, Value, Var)

type IP = Int

-- | Statement of a program.
data Inst
    = Push Int
    | Bin BinOp
    | Ld Var
    | St Var
    | Read
    | Write
    | Nop
    deriving (Eq, Show)

type Insts = V.Vector Inst

-- | State of execution
data ExecState = ExecState
    { _esIn    :: [Value]
      -- ^ represents stdin, extractable by `Read`
    , _esOut   :: [Value]
      -- ^ represents stdout, modifiable by `Write`
      -- (list header corresponds to current end of output)
    , _esVars  :: LocalVars
      -- ^ local variables values
    , _esStack :: [Value]
      -- ^ current stack
    , _esInsts :: Insts
      -- ^ instructions list
    , _esIp    :: IP
      -- ^ instruction pointer, no of command to execute next
    } deriving (Eq, Show)

makeLenses ''ExecState

-- | Execution error
type Error = String

-- | Result of execution
type Exec = Either Error ExecState

-- | Execution state at beginning of program and with empty input
simpleExecState :: Insts -> ExecState
simpleExecState = anExecState []

-- | Execution state at beginning of program
anExecState :: [Value] -> Insts -> ExecState
anExecState is insts = ExecState is [] M.empty [] insts 0

-- | Get input and output streams.
-- Unlike input, output stream has LIFO order
getIO :: ExecState -> ([Value], [Value])
getIO (ExecState is os _ _ _ _) = (is, os)
