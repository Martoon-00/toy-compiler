-- | Some utility functions on SM instructions analysis

module Toy.SM.Util where

import           Data.Foldable (Foldable, foldMap)
import qualified Data.Set      as S
import           Universum     hiding (Foldable, foldMap)

import           Toy.Base      (Var)
import           Toy.SM.Data   (Inst (..))

gatherLocals :: Foldable t => t Inst -> S.Set Var
gatherLocals = foldMap gather
  where
     gather :: Inst -> S.Set Var
     gather (Store v) = one v
     gather _         = mempty

