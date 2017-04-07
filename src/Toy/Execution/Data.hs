{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Toy.Execution.Data
    ( In
    , Out
    , InOut
    , Meta (..)
    , withEmptyInput
    ) where

import           Data.Text           (Text)
import           Data.Text.Buildable (Buildable (..))
import           Formatting          ((%))
import qualified Formatting          as F
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
