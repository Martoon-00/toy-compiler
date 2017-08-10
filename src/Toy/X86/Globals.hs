-- | Global variables in runtime environment

module Toy.X86.Globals where

import           Toy.Base (Value, Var)

outIndicatorVar :: Var
outIndicatorVar = "out_indicator"

globalVars :: [(Var, Value)]
globalVars =
    [ (outIndicatorVar, 0)
    ]


