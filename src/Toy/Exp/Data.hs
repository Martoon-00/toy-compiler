module Toy.Exp.Data where

import           Data.String   (IsString (..))

import           Toy.Base.Data (BinOp, UnaryOp, Value, Var)

-- | Expression
data Exp
    = ValueE Value
    | VarE Var
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


-- | @read@ expression.
readE :: Exp
readE = FunE "read" []

-- | Parameters of function call. Appears in many types, so extracted to
-- separate type alias.
-- type FunCallParams = (Var, [Exp])
