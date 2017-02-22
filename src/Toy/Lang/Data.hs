{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}

module Toy.Lang.Data where

import           Control.Lens ((%~), _Left)
import qualified Data.Map     as M

import           Toy.Data     (Exp (..), LocalVars, Value, Var)

-- | Statement of a program.
data Stmt
    = Var := Exp
    | Read Var
    | Write Exp
    | If Exp Stmt Stmt
    | While Exp Stmt
    | Seq Stmt Stmt
    | Skip
    | Int Int Stmt  -- interrupt, with interrupt code - for debug purposes
    deriving (Eq, Show)

infix 0 :=

instance Monoid Stmt where
    mempty = Skip
    mappend = Seq

-- | State of execution
data ExecState = ExecState
    { esInputStream  :: [Value]
      -- ^ represents stdin, extractable by `Read`
    , esOutputStream :: [Value]
      -- ^ represents stdout, modifiable by `Write`
      -- (list header corresponds to current end of output)
    , esLocalVars    :: LocalVars
      -- ^ local variables values
    , esStatement    :: Stmt
      -- ^ remaining commands to execute
    } deriving (Eq, Show)

-- | Execution error
type Error = (Stmt, String)

-- | Result of execution
type Exec = Either Error ExecState

-- | Result of expression evaluation
type Calc = Either String Value

-- | Adds current statement info to probable evaluation error
withStmt :: Stmt -> Either String a -> Either Error a
withStmt stmt = _Left %~ (stmt, )

-- | Execution state at beginning of program and with empty input
simpleExecState :: Stmt -> ExecState
simpleExecState = anExecState []

-- | Execution state at beginning of program
anExecState :: [Value] -> Stmt -> ExecState
anExecState is = ExecState is [] M.empty

-- | Interrupt with no continuation
int :: Int -> Stmt
int code = Int code Skip

-- | Get input and output streams.
-- Output stream is in FIFO order
getIO :: ExecState -> ([Value], [Value])
getIO (ExecState is os _ _) = (is, os)
