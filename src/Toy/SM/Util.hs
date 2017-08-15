-- | Some utility functions on SM instructions analysis

module Toy.SM.Util where

import           Data.Foldable (Foldable, foldMap)
import qualified Data.Set      as S
import           Universum     hiding (Foldable, foldMap)

import           Toy.Base      (Var)
import           Toy.SM.Data   (Inst (..), LabelId (..), UserLabelId,
                                nonlocalLabelsTableLabel)

gatherLocals :: Foldable t => t Inst -> S.Set Var
gatherLocals = foldMap gather
  where
    gather :: Inst -> S.Set Var
    gather (Store v)     = one v
    gather (StoreInit v) = one v
    gather _             = mempty

gatherULabels :: Foldable t => t Inst -> S.Set UserLabelId
gatherULabels = foldMap $ \case
    (Label (ULabel id)) -> one id
    _                   -> mempty

countOutLabels :: Foldable t => t Inst -> Bool
countOutLabels = getAny . foldMap (Any . (== Jmp nonlocalLabelsTableLabel))
