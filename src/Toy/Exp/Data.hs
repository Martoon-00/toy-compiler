{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}

module Toy.Exp.Data where

import           Control.Lens   (makeLenses, makePrisms)
import           Data.String    (IsString (..))
import qualified Data.Vector    as V
import           Formatting     (formatToString, shown, (%))
import qualified Prelude
import           Universum

import           Toy.Base.Data  (BinOp, UnaryOp, Value, Var)
import           Toy.Exp.RefEnv (MRef, MRefId)

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

-- | Array with some meta info
data ArrayInnards = ArrayInnards
    { _aiRefCounter :: Int
    , _aiRefId      :: MRefId
    , _aiArray      :: V.Vector ExpRes
    }

instance Show ArrayInnards where
    show ArrayInnards {..} =
        formatToString
            (shown % " (" %shown % ", " %shown % ")")
            _aiArray
            _aiRefId
            _aiRefCounter

-- | Evaluated expression
data ExpRes
   = ValueR Value
   | ArrayR (MRef (Maybe ArrayInnards))
   | NotInitR

instance Show ExpRes where
    show (ValueR n) = show n
    show (ArrayR _) = "<array>"
    show NotInitR   = "<undefined>"

makeLenses ''ArrayInnards
makePrisms ''ExpRes
