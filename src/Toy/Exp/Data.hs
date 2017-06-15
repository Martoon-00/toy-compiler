{-# LANGUAGE TemplateHaskell #-}

module Toy.Exp.Data where

import           Control.Lens              (makePrisms)
import           Control.Monad.Error.Class (MonadError (..))
import           Data.IORef                (IORef)
import           Data.String               (IsString (..))
import qualified Data.Vector               as V
import qualified Prelude
import           Universum

import           Toy.Base.Data             (BinOp, UnaryOp, Value, Var)

-- TODO:
data Boxing
    = Boxed
    | Unboxed
    deriving (Eq, Show, Enum, Bounded)

type MRef = IORef

-- | Expression
data Exp
    = ValueE Value
    | VarE Var
    | UnaryE UnaryOp Exp
    | BinE BinOp Exp Exp
    | FunE Var [Exp]
    | ArrayUninitE Int  -- ^ uninitialized array
    | ArrayAccessE Exp Exp  -- array & index
    deriving (Show)

instance IsString Exp where
    fromString = VarE . fromString

instance Num Exp where
    (+) = BinE "+"
    (-) = BinE "-"
    (*) = BinE "*"
    abs = error "Num abs: undefined"
    signum = error "Num sugnum: undefined"
    fromInteger = ValueE . fromInteger

-- | @read@ expression.
readE :: Exp
readE = FunE "read" []


-- | Evaluated expression
data ExpRes
   = ValueR Value
   | ArrayR (MRef (Maybe (V.Vector ExpRes)))
   | NotInitR

makePrisms ''ExpRes

instance Show ExpRes where
    show (ValueR n) = show n
    show (ArrayR _) = "<array>"
    show NotInitR   = "<undefined>"


valueOnly
    :: (IsString s, MonadError s m)
    => m ExpRes -> Text -> m Value
valueOnly action (fromString . toString -> desc) =
    maybe (throwError desc) pure . preview _ValueR =<< action

arrayOnly
    :: (IsString s, MonadError s m)
    => m ExpRes -> Text -> m (MRef (Maybe (V.Vector ExpRes)))
arrayOnly action (fromString . toString -> desc) =
    maybe (throwError desc) pure . preview _ArrayR =<< action
