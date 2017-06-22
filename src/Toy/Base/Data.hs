{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Toy.Base.Data where

import           Control.Monad.Trans.Either (EitherT (..))
import           Data.Conduit               (ConduitM)
import           Data.Int                   (Int32)
import           Data.String                (IsString (..))
import qualified Prelude
import           Universum

-- | Variable name
newtype Var = Var Text
    deriving (Eq, Ord, IsString, Buildable)

instance Show Var where
    show (Var name) = show name

-- | Expression type
type Value = Int32

type UnaryOp = Text

type BinOp = Text

-- | Monad with input/output capabilities
type ExecInOut = ConduitM Value Value

-- | Monad where execution happens
type Exec m = ExecInOut $ EitherT Text m
