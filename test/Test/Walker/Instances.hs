{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Test.Walker.Instances
    ( ProgramTestData (..)
    , FullTestData (..)
    ) where

import           Test.Util             (Parsable (..))
import           Test.Walker.Extractor (TestCaseData (..), file, readWithExtension)
import           Toy.Exp               (Value)
import qualified Toy.Lang              as L

instance Parsable L.Stmt where
    parseData = L.parse

data ProgramTestData = ProgramTestData
    { ptdProgram :: L.Stmt
    } deriving (Show)

instance TestCaseData ProgramTestData where
    tryGetTestCaseData = readWithExtension $
        ProgramTestData <$> file ".prog"

data FullTestData = FullTestData
    { ftdProgram :: L.Stmt
    , ftdInput   :: [Value]
    , ftdOutput  :: [Value]
    } deriving (Show)

instance TestCaseData FullTestData where
    tryGetTestCaseData = readWithExtension $ do
        ftdProgram <- file ".prog"
        ftdInput   <- file ".in"
        ftdOutput  <- file ".out"
        return FullTestData{..}
