{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Test.Examples.ControlSpec
    ( spec
    ) where

import           Test.Hspec      (Spec, describe)
import           Test.QuickCheck (Property, counterexample)

import           Test.Arbitrary  ()
import           Test.Execution  (TestRes (..), describeExecWays, (>-*->))
import           Test.Walker     (TestCaseData (..), describeDir, file, readWithExtension)
import           Toy.Execution   (ExecWay (..), asIs, defCompileX86, translateLang,
                                  (<~~>))
import           Toy.Exp
import qualified Toy.Lang        as L
import           Toy.Util        (OutputValues (..))


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
    tryGetTestCaseData = readWithExtension $ do
        ctdProgram <- file ".expr"
        ctdInput   <- file ".input"
        ctdOutput  <- file ".log"
        return ControlTestData{..}

controlTest :: ExecWay L.Program -> Either String ControlTestData -> Property
controlTest _ (Left err) =
    counterexample ("Parse failed: " ++ err) False
controlTest way (Right ControlTestData{..}) =
    (ctdInput >-*-> TestRes (getOutputValues ctdOutput)) ctdProgram way
