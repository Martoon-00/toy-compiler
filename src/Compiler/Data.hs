module Compiler.Data where

import qualified Data.Map as M

type Var = String

type Value = Int

type LocalVars = M.Map Var Value

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
type Calc = Either String Value

withStmt :: Stmt -> Either String a -> Either Error a
withStmt stmt (Left e)  = Left (stmt, e)
withStmt _    (Right r) = Right r
