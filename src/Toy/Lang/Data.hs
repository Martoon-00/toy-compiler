{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}

module Toy.Lang.Data where

import           Control.Lens ((%~), _Left)
import qualified Data.Map     as M

import           Toy.Exp.Data (Exp (..), LocalVars, Value, Var)

-- | Statement of a program.
data Stmt
    = Var := Exp
    | Read Var
    | Write Exp
    | If Exp Stmt Stmt
    | DoWhile Stmt Exp  -- ^ @do .. while@ is the most optimal / easy loop from
                        -- asm point of view
    | Seq Stmt Stmt
    | Skip
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
type Error = String

-- | Result of execution
type Exec = Either Error ExecState

-- | Adds current statement info to probable evaluation error
withStmt :: Stmt -> Either String a -> Either Error a
withStmt stmt = _Left %~ ((show stmt ++ ": ") ++)

-- | Execution state at beginning of program and with empty input
simpleExecState :: Stmt -> ExecState
simpleExecState = anExecState []

-- | Execution state at beginning of program
anExecState :: [Value] -> Stmt -> ExecState
anExecState is = ExecState is [] M.empty

-- | Get input and output streams.
-- Unlike input, output stream has LIFO order
getIO :: ExecState -> ([Value], [Value])
getIO (ExecState is os _ _) = (is, os)


-- | @while@ loop in terms of `Stmt`.
while :: Exp -> Stmt -> Stmt
while cond stmt = If cond (DoWhile stmt cond) Skip
