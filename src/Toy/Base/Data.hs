{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Toy.Base.Data where

import           Control.Monad.Trans.Either (EitherT (..))
import           Data.Bits                  (Bits, FiniteBits)
import           Data.Conduit               (ConduitM)
import           Data.Int                   (Int32)
import           Data.String                (IsString (..))
import           Prelude                    (Read (readsPrec))
import qualified Prelude
import           Universum

import           Toy.Util.Bits              (clearPHBit)

-- | Variable name
newtype Var = Var Text
    deriving (Eq, Ord, IsString, Buildable)

instance Show Var where
    show (Var name) = show name

-- | Expression type
newtype Value = Value Int32
    deriving (Eq, Ord, Enum, Num, Real, Integral, Bits, FiniteBits, NFData
             , Buildable)

instance Read Value where
    readsPrec = map (first Value) ... readsPrec

instance Show Value where
    show (Value v) = show v

instance Bounded Value where
    minBound = Value $ clearPHBit minBound
    maxBound = Value $ clearPHBit maxBound

type UnaryOp = Text

type BinOp = Text

-- | Monad with input/output capabilities
type ExecInOut = ConduitM Value Value

-- | Monad where execution happens
type Exec m = ExecInOut $ EitherT Text m
