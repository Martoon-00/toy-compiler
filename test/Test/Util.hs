module Test.Util
    ( instsSM
    ) where

import           Control.Monad.Trans  (MonadIO (..))
import           Test.Hspec.Core.Spec (SpecM (..))
import           Test.QuickCheck      (Property, conjoin, property, (.&&.))

import qualified Toy.SM               as SM

instsSM :: SM.Insts -> SM.Insts
instsSM = id

instance Monoid Property where
    mempty = property True
    mappend = (.&&.)
    mconcat = conjoin

instance MonadIO (SpecM a) where
    liftIO = SpecM . liftIO
