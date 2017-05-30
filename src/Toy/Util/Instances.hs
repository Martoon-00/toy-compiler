{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-orphans   #-}

-- | Some missing instances for standart types

module Toy.Util.Instances () where

import           Control.Lens (Snoc (..), prism)
import qualified Data.DList   as D
import           Universum    (One (..))

instance One (D.DList x) where
    type OneItem (D.DList x) = x
    one = D.fromList . one

instance Snoc (D.DList a) (D.DList a) a a where
    _Snoc = prism (uncurry D.snoc) undefined

instance Traversable D.DList where
    traverse f l = fmap D.fromList $ traverse f (D.toList l)
