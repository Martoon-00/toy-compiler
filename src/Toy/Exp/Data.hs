{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Toy.Exp.Data where

import qualified Data.Map    as M
import           Data.String (IsString (..))
import           Data.Text   (Text)

-- | Variable name
newtype Var = Var String
    deriving (Eq, Ord, Show, IsString)

-- | Expression type
type Value = Int

-- | Current state of local variables
type LocalVars = M.Map Var Value

-- | Unary operation
type UnaryOp = Text

-- | Binary operation
type BinOp = Text

-- | Expression
data Exp
    = ValueE Value
    | VarE Var
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
