-- | Some types at top of expression type.

module Toy.Exp.Ext where

import qualified Data.Map     as M

import           Toy.Base     (Var)
import           Toy.Exp.Data (ExpRes)


type LocalVars = M.Map Var ExpRes


