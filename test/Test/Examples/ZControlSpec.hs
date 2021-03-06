{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Test.Examples.ZControlSpec
    ( spec
    ) where

import           Test.Hspec      (Spec, describe)
import           Test.QuickCheck (Property, counterexample)
import           Universum

import           Test.Arbitrary  ()
import           Test.Execution  (TestRes (..), describeExecWays, (>-*->))
import           Test.Walker     (Extension, TestCaseData (..), describeDir, gatherFile,
                                  readTestCase)
import           Toy.Base        (OutputValues (..), Value)
import           Toy.Execution   (ExecWay (..), asIs, defCompileX86, translateLang,
                                  (<~~>))
import qualified Toy.Lang        as L


spec :: Spec
spec = do
    let ways =
            [ Ex asIs
            , Ex translateLang
            , Ex $ translateLang <~~> defCompileX86
            ]
    describeExecWays ways $ \way -> do
        describe "control" $
            describeDir "./test/cases/control/" (controlTest way)

data ControlTestData = ControlTestData
    { ctdProgram :: L.Program
    , ctdInput   :: [Value]
    , ctdOutput  :: OutputValues
    } deriving (Show)

instance TestCaseData ControlTestData where
    type PathDiffObj ControlTestData = Extension
    mkTestCollector = readTestCase $
        ControlTestData
        <$> gatherFile ".expr"
        <*> gatherFile ".input"
        <*> gatherFile ".log"

controlTest :: ExecWay L.Program -> Either Text ControlTestData -> Property
controlTest _ (Left err) =
    counterexample ("Parse failed: " ++ toString err) False
controlTest way (Right ControlTestData{..}) =
    (ctdInput >-*-> TestRes (getOutputValues ctdOutput)) ctdProgram way
