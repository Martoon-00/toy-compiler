{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Test.Lang.ParserSpec
    ( spec
    ) where

import           Control.Lens    (has, _Left)
import           Test.Hspec      (Spec, describe)
import           Test.QuickCheck (Property, counterexample, property)

import           Test.Walker     (ProgramTestData (..), describeDir)

spec :: Spec
spec = describe "parser" $ do
    describeDir "test/cases/parser/well" parseWellTest
    describeDir "test/cases/parser/bad"  parseBadTest

type TestData = Either String ProgramTestData

parseWellTest :: TestData -> Property
parseWellTest (Left err) = counterexample ("Parse failed: " ++ err) False
parseWellTest _          = property True

parseBadTest :: TestData -> Property
parseBadTest = property . has _Left
