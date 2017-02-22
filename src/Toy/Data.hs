{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}

module Toy.Data where

import qualified Data.Map    as M
import           Data.String (IsString (..))

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
