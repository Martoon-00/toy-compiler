{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards   #-}

module Test.Walker.Instances
    ( ProgramTestData (..)
    , FullTestData (..)
    ) where

import           Control.Applicative   (many)
import           Data.Attoparsec.Text  (decimal, parseOnly, signed, space)

import           Test.Walker.Extractor (Parsable (..), TestCaseData (..), file,
                                        readWithExtension)
import           Toy.Exp               (Value)
import qualified Toy.Lang              as L

instance Parsable L.Stmt where
    parseData = L.parse

instance Parsable [Value] where
    parseData = parseOnly $ many (many space *> signed decimal)


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
