{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Test.Execution.Data
    ( In
    , Out
    , InOut
    , Meta (..)
    , withEmptyInput
    , metaCounterexample
    ) where

import           Data.Text           (Text)
import           Data.Text.Buildable (Buildable (..))
import           Formatting          ((%))
import qualified Formatting          as F
import           Test.QuickCheck     (Property, counterexample)
import           Toy.Exp             (Value)

type In = [Value]
type Out = [Value]
type InOut = (In, Out)

data Meta = Meta
    { metaName :: Text
    , metaBody :: Text
    }

instance Buildable Meta where
    build Meta{..} =
        F.bprint ("\n=== "%F.stext%" ===\n"%F.stext%"\n--^--^--\n")
        metaName metaBody


withEmptyInput :: Out -> InOut
withEmptyInput = ([], )

metaCounterexample :: [Meta] -> Property -> Property
metaCounterexample = flip . foldr $ counterexample . F.formatToString F.build
