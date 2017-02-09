{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}

module Compiler.Data where

import           Control.Lens ((%~), _Left)
import qualified Data.Map     as M
import           Data.String  (IsString (..))

-- | Variable name
newtype Var = Var String
    deriving (Eq, Ord, Show, IsString)

-- | Expression type
type Value = Int

-- | Current state of local variables
type LocalVars = M.Map Var Value

-- | Expression
data Exp
    = ValueE Value

    | VarE Var

    | Exp :+ Exp
    | Exp :- Exp
    | Exp :* Exp
    | Exp :/ Exp
    | Exp :% Exp

    | NotE Exp
    | Exp :&& Exp
    | Exp :|| Exp
    | Exp :^ Exp
    | Exp :& Exp
    | Exp :| Exp

    | Exp :> Exp
    | Exp :< Exp
    | Exp :>= Exp
    | Exp :<= Exp
    | Exp :== Exp
    | Exp :!= Exp

    deriving (Eq, Show)

instance Num Exp where
    a + b = a :+ b
    a - b = a :- b
    a * b = a :* b
    abs = undefined
    signum = undefined
    fromInteger = ValueE . fromInteger

instance IsString Exp where
    fromString = VarE . fromString

-- | Statement of a program.
data Stmt
    = Var := Exp
    | ReadS Var
    | WriteS Exp
    | IfS Exp Stmt Stmt
    | WhileS Exp Stmt
    | SequenceS Stmt Stmt
    | SkipS
    | IntS Int Stmt  -- interrupt, with interrupt code - for debug purposes
    deriving (Eq, Show)

infix 0 :=

instance Monoid Stmt where
    mempty = SkipS
    mappend = SequenceS

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
simpleExecState = ExecState [] [] M.empty

-- | Interrupt with no continuation
int :: Int -> Stmt
int code = IntS code SkipS

-- | Get input and output streams.
-- Output stream is in FIFO order
getIO :: ExecState -> ([Value], [Value])
getIO (ExecState is os _ _) = (is, os)
