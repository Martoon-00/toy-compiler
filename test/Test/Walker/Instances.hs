{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeFamilies  #-}

module Test.Walker.Instances
    ( ProgramTestData (..)
    , FullTestData (..)
    ) where

import           Test.Walker.Extractor (Extension, TestCaseData (..), gatherFile,
                                        readTestCase)
import           Toy.Base              (Value)
import qualified Toy.Lang              as L

data ProgramTestData = ProgramTestData
    { ptdProgram :: L.Program
    } deriving (Show)

instance TestCaseData ProgramTestData where
    type PathDiffObj ProgramTestData = ()
    mkTestCollector = readTestCase $
        ProgramTestData <$> gatherFile ()

data FullTestData = FullTestData
    { ftdProgram :: L.Program
    , ftdInput   :: [Value]
    , ftdOutput  :: [Value]
    } deriving (Show)

instance TestCaseData FullTestData where
    type PathDiffObj FullTestData = Extension
    mkTestCollector = readTestCase $ do
        ftdProgram <- gatherFile ".prog"
        ftdInput   <- gatherFile ".in"
        ftdOutput  <- gatherFile ".out"
        return FullTestData{..}
