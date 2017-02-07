{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Compiler.Data where

import qualified Data.Map    as M
import           Data.String (IsString (..))

newtype Var = Var String
    deriving (Eq, Ord, Show, IsString)

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

data ExecState = ExecState [Value] [Value] LocalVars Stmt
    deriving (Eq, Show)

type Error = (Stmt, String)

type Exec = Either Error ExecState
type Calc = Either String Value

withStmt :: Stmt -> Either String a -> Either Error a
withStmt stmt (Left e)  = Left (stmt, e)
withStmt _    (Right r) = Right r

simpleExecState :: Stmt -> ExecState
simpleExecState = ExecState [] [] M.empty

int :: Int -> Stmt
int code = IntS code SkipS
