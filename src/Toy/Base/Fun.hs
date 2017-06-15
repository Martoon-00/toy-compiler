module Toy.Base.Fun
    ( FunSign (..)
    , stdFunExamples
    ) where

import           Toy.Base.Data (Var)
import           Toy.Exp.Data  (Exp (..))
import           Universum

-- | Function signature
data FunSign = FunSign Var [Var]
    deriving (Show, Eq)

-- | Just for ~fun~ convinience.
(%) :: Var -> [Exp] -> (Var, [Exp])
(%) = (,)

stdFunExamples :: [(Var, [Exp])]
stdFunExamples =
    [ "read"    % []
    , "write"   % [0]
    , "arrlen"  % [ArrayUninitE 0]
    , "arrmake" % [3, 5]
    , "Arrmake" % [2, ArrayUninitE 0]
    ]


