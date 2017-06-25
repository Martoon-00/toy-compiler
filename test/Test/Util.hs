{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Util
    ( Extract (..)
    , VerySmall (..)
    , instsSM
    ) where

import           Control.Monad.Trans  (MonadIO (..))
import           Test.Hspec.Core.Spec (SpecM (..))
import           Test.QuickCheck      (Arbitrary (..), Large (..), NonNegative (..),
                                       Property, Small (..), conjoin, property, (.&&.))
import           Universum

import qualified Toy.SM               as SM

instsSM :: SM.Insts -> SM.Insts
instsSM = identity

instance Monoid Property where
    mempty = property True
    mappend = (.&&.)
    mconcat = conjoin

instance MonadIO (SpecM a) where
    liftIO = SpecM . liftIO


newtype VerySmall a = VerySmall
    { getVerySmall :: a
    } deriving (Eq, Ord, Show, Num, Enum)

verySmallLimit :: Num a => a
verySmallLimit = 5

instance (Arbitrary a, Integral a, Enum a) => Arbitrary (VerySmall a) where
    arbitrary = VerySmall . flip rem verySmallLimit <$> arbitrary
    shrink k = [k + 1 .. verySmallLimit - 1]


class Extract a p where
    extract :: p -> a

instance Extract a a where
    extract = identity

instance Extract a (Large a) where
    extract = getLarge

instance Extract a b => Extract a (NonNegative b) where
    extract = extract . getNonNegative

instance Extract a (Small a) where
    extract = getSmall

instance Extract a (VerySmall a) where
    extract = getVerySmall
