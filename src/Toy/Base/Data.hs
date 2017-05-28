{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Toy.Base.Data where

import           Control.Monad.Trans.Either (EitherT (..))
import           Data.Conduit               (ConduitM)
import           Data.Int                   (Int32)
import qualified Data.Map                   as M
import           Data.String                (IsString (..))
import           Universum                  (type ($), Buildable, Text)

-- | Variable name
newtype Var = Var Text
    deriving (Eq, Ord, Show, IsString, Buildable)

-- | Expression type
type Value = Int32

type LocalVars = M.Map Var Value

-- | Function signature
data FunSign = FunSign Var [Var]
    deriving (Show, Eq)

type UnaryOp = Text

type BinOp = Text

-- | Monad with input/output capabilities
type ExecInOut = ConduitM Value Value

-- | Monad where execution happens
type Exec m = ExecInOut $ EitherT Text m
