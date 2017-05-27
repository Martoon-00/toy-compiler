{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}

module Test.Walker.Instances
    ( ProgramTestData (..)
    , FullTestData (..)
    ) where

import           Test.Walker.Extractor (Extension, TestCaseData (..), gatherFile,
                                        readTestCase)
import           Toy.Exp               (Value)
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
    mkTestCollector = readTestCase $
        FullTestData
        <$> gatherFile ".prog"
        <*> gatherFile ".in"
        <*> gatherFile ".out"
