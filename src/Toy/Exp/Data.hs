{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Toy.Exp.Data where

import           Control.Monad.Trans.Either (EitherT (..))
import           Data.Conduit               (ConduitM)
import           Data.Int                   (Int32)
import qualified Data.Map                   as M
import           Data.String                (IsString (..))
import           Data.Text                  (Text)
import           Universum                  (type ($), Buildable)

-- | Variable name
newtype Var = Var String
    deriving (Eq, Ord, Show, IsString, Buildable)

-- | Expression type
type Value = Int32

type LocalVars = M.Map Var Value

data FunSign = FunSign Var [Var]
    deriving (Show, Eq)

type UnaryOp = Text

type BinOp = Text

-- | Monad with input/output capabilities
type ExecInOut = ConduitM Value Value

-- | Monad where execution happens
type Exec m = ExecInOut $ EitherT String m

-- | Expression
data Exp
    = ValueE Value
    | VarE Var
    | ReadE
    | UnaryE UnaryOp Exp
    | BinE BinOp Exp Exp
    | FunE Var [Exp]
    deriving (Eq, Show)

instance IsString Exp where
    fromString = VarE . fromString

instance Num Exp where
    (+) = BinE "+"
    (-) = BinE "-"
    (*) = BinE "*"
    abs = undefined
    signum = undefined
    fromInteger = ValueE . fromInteger
