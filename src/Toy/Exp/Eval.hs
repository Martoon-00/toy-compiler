module Toy.Exp.Eval
    ( Calc
    , eval
    ) where

import qualified Data.Map           as M
import           Toy.Exp.Data       (Exp (..), LocalVars, Value)
import           Toy.Exp.Operations (binOp, unaryOp)
import           Toy.Exp.Util       (arithspoon)

-- | Result of expression evaluation
type Calc = Either String Value

-- | Evaluate expression in given variables context
eval :: Exp -> LocalVars -> Calc
eval e vars = ev e
  where
    ev (ValueE v   ) = Right v
    ev (VarE v     ) =
        maybe (Left $ "No variable " ++ show v ++ " defined") Right $
        M.lookup v vars
    ev (UnaryE op v) = arithspoon =<< (unaryOp op <$> ev v)
    ev (BinE op a b) = arithspoon =<< (binOp op <$> ev a <*> ev b)
