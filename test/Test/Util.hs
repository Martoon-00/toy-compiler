{-# LANGUAGE MultiParamTypeClasses #-}

module Test.Util
    ( Extract (..)
    , instsSM
    ) where

import           Control.Monad.Trans  (MonadIO (..))
import           Test.Hspec.Core.Spec (SpecM (..))
import           Test.QuickCheck      (Large (..), NonNegative (..), Property, Small (..),
                                       conjoin, property, (.&&.))

import qualified Toy.SM               as SM

instsSM :: SM.Insts -> SM.Insts
instsSM = id

instance Monoid Property where
    mempty = property True
    mappend = (.&&.)
    mconcat = conjoin

instance MonadIO (SpecM a) where
    liftIO = SpecM . liftIO


class Extract a p where
    extract :: p -> a

instance Extract a a where
    extract = id

instance Extract a (NonNegative a) where
    extract = getNonNegative

instance Extract a (Large a) where
    extract = getLarge

instance Extract a (NonNegative (Small a)) where
    extract = getSmall . getNonNegative
