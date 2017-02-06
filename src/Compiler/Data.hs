module Compiler.Data where

import qualified Data.Map as M

type Var = String

type Value = Int

type LocalVars = M.Map Var Value

data Exp
    = ValueE Value
    -- TODO
    deriving (Show)

data Stmt
    = Var := Exp
    | ReadS Var
    | WriteS Exp
    | IfS Exp Stmt Stmt
    | WhileS Exp Stmt
    | SequenceS Stmt Stmt
    | SkipS
    deriving (Show)

data ExecState = ExecState [Value] [Value] LocalVars Stmt
    deriving (Show)

type Error = (Stmt, String)

type Exec = Either Error ExecState
type Calc = Either Error Value

eval :: LocalVars -> Exp -> Calc
eval _ (ValueE v) = Right v
