{-# LANGUAGE RecordWildCards #-}

module Test.Walker.Instances
    ( ProgramTestData (..)
    , FullTestData (..)
    ) where

import           Test.Walker.Extractor (TestCaseData (..), file, readAll,
                                        readWithExtension)
import           Toy.Exp               (Value)
import qualified Toy.Lang              as L

data ProgramTestData = ProgramTestData
    { ptdProgram :: L.Program
    } deriving (Show)

instance TestCaseData ProgramTestData where
    tryGetTestCaseData = readAll $
        ProgramTestData <$> file ()

data FullTestData = FullTestData
    { ftdProgram :: L.Program
    , ftdInput   :: [Value]
    , ftdOutput  :: [Value]
    } deriving (Show)

instance TestCaseData FullTestData where
    tryGetTestCaseData = readWithExtension $ do
        ftdProgram <- file ".prog"
        ftdInput   <- file ".in"
        ftdOutput  <- file ".out"
        return FullTestData{..}
