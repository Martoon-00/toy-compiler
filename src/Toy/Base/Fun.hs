module Toy.Base.Fun
    ( FunSign (..)
    , stdFunExamples
    ) where

import           Toy.Base.Data (Var)
import           Toy.Exp.Data  (Exp (..))
import           Universum

import           Toy.Util      ((>:))

-- | Function signature
data FunSign = FunSign Var [Var]
    deriving (Show, Eq)

stdFunExamples :: [(Var, [Exp])]
stdFunExamples =
    [ "read"    >: []
    , "write"   >: [0]
    , "arrlen"  >: [ArrayUninitE 0]
    , "arrmake" >: [3, 5]
    , "Arrmake" >: [2, ArrayUninitE 0]
    ]


