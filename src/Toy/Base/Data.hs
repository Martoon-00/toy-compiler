{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Toy.Base.Data where

import           Control.Lens  (both)
import           Data.Bits     (Bits (..), FiniteBits)
import           Data.Conduit  (ConduitM)
import           Data.Int      (Int32)
import           Data.String   (IsString (..))
import           Prelude       (Read (readsPrec))
import qualified Prelude
import           Universum

import qualified Toy.Constants as C
import           Toy.Util.Bits (clearPHBit, setPHBit)

-- | Variable name
newtype Var = Var Text
    deriving (Eq, Ord, IsString, Buildable)

instance Show Var where
    show (Var name) = show name

-- | Expression type
newtype Value = Value Int32
    deriving (Eq, Ord, Enum, Bits, FiniteBits, NFData
             , Buildable)

instance Read Value where
    readsPrec = map (first Value) ... readsPrec

instance Show Value where
    show (Value v) = show v

instance Bounded Value where
    minBound = Value $ setPHBit minBound
    maxBound = Value $ clearPHBit maxBound

keep31 :: Int32 -> Value
keep31 x
    | C.use31Arith = Value $ (if testBit x 31 then setBit else clearBit) x 30
    | otherwise    = Value x

instance Num Value where
    Value a + Value b = keep31 $ a + b
    Value a - Value b = keep31 $ a - b
    Value a * Value b = keep31 $ a * b
    abs (Value x) = keep31 $ abs x
    signum (Value x) = Value $ signum x
    fromInteger = keep31 . fromInteger

instance Real Value where
    toRational (Value x) = toRational x

instance Integral Value where
    quotRem (Value a) (Value b) = both %~ keep31 $ quotRem a b
    toInteger (Value x) = toInteger x

type UnaryOp = Text

type BinOp = Text

-- | Monad with input/output capabilities
type ExecInOut = ConduitM Value Value

-- | Monad where execution happens
type Exec m = ExecInOut $ ExceptT Text m
