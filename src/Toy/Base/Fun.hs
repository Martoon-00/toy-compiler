{-# LANGUAGE TupleSections #-}

module Toy.Base.Fun
    ( FunSign (..)
    , stdFunExamples
    , stdFuns
    ) where

import           Toy.Base.Data (Var)
import           Toy.Exp.Data  (Exp)

-- | Function signature
data FunSign = FunSign Var [Var]
    deriving (Show, Eq)


-- | Just for convinience
(%) :: Var -> [Exp] -> (Var, [Exp])
(%) = (,)

stdFunExamples :: [(Var, [Exp])]
stdFunExamples =
    [ "read"  % []
    , "write" % [0]
    ]

stdFuns :: [Var]
stdFuns = map fst stdFunExamples
