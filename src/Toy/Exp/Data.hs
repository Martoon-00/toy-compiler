{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Toy.Exp.Data where

import           Control.Monad.State        (StateT)
import           Control.Monad.Trans.Either (EitherT)
import           Data.Conduit               (ConduitM)
import           Data.Functor.Identity      (Identity)
import           Data.Int                   (Int32)
import qualified Data.Map                   as M
import           Data.String                (IsString (..))
import           Data.Text                  (Text)
import           Universum                  (type ($))

-- | Variable name
newtype Var = Var String
    deriving (Eq, Ord, Show, IsString)

-- | Expression type
type Value = Int32

-- | Current state of local variables
type LocalVars = M.Map Var Value

-- | Unary operation
type UnaryOp = Text

-- | Binary operation
type BinOp = Text

type Exec = ConduitM Value Value $ StateT LocalVars $ EitherT String Identity

-- | Expression
data Exp
    = ValueE Value
    | VarE Var
    | ReadE
    | UnaryE UnaryOp Exp
    | BinE BinOp Exp Exp
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
