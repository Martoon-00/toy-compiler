{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Test.Examples.LangSpec
    ( spec
    ) where

import           Control.Lens    ((&), (<&>))
import           Test.Hspec      (Spec, describe, it)
import           Test.QuickCheck (Discard (..), NonNegative (..), Property, conjoin,
                                  counterexample, property, within, (===), (==>))

import           Test.Arbitrary  ()
import           Test.Execution  (TestRes (..), describeExecWays, (>-*->), (>-->), (~*~))
import           Test.Walker     (FullTestData (..), describeDir)
import           Toy.Execution   (ExecWay (..), asIs, defCompileX86, translateLang,
                                  (<~~>))
import           Toy.Exp
import           Toy.Lang        (ExecState (..), Stmt (..), execute, simpleExecState)
import qualified Toy.Lang        as L


spec :: Spec
spec = do
    describeExecWays [Ex @Stmt asIs] $ \_ -> do
        describe "examples" $ do
            it "Skip" $
                initSkipTest
            it "variables simple" $
                varsTest
            it "different erroneous scenarios" $
                errorsTest

        it "`execute` always ends with Skip" $
            property executeAlwaysEndsWithSkip

        describeDir "./test/cases/exec"
            fileTest

    let ways =
            [ Ex asIs
            , Ex translateLang
            , Ex $ translateLang <~~> defCompileX86
            ]
    describeExecWays ways $ \way -> do
        describe "examples" $ do
            it "no actions" $
                noActions way
            it "io simple" $
                ioTest way
            it "If true" $
                ifTrueTest way
            it "If false" $
                ifFalseTest way
            it "While simple" $
                whileTest way
            it "If simple" $
                property $ minTest way
            describe "complex" $ do
                it "fib" $
                    property $ fibTest way
                it "gcd" $
                    property $ gcdTest way


executeAlwaysEndsWithSkip :: ExecState -> Property
executeAlwaysEndsWithSkip initExecState@(ExecState _ _ _ initStmt) =
    within 1000000 $
    -- propability of initial action to be SkipS is too high
    initStmt /= Skip ==>
        case execute initExecState of
            Left _                      -> property Discard
            Right (ExecState _ _ _ end) -> end === Skip

noActions :: ExecWay Stmt -> Property
noActions = mempty & [] >-*-> []

initSkipTest :: Property
initSkipTest = execute sample === Right expected
  where
    sample   = simpleExecState Skip
    expected = simpleExecState Skip

ifTrueTest :: ExecWay Stmt -> Property
ifTrueTest = sample & [] >-*-> [0]
  where
    sample = If 1 (Write 0) (Write 1)

ifFalseTest :: ExecWay Stmt -> Property
ifFalseTest = sample & [] >-*-> [1]
  where
    sample = If 0 (Write 0) (Write 1)

varsTest :: Property
varsTest = execute sample === Right expected
  where
    sample = simpleExecState $ mconcat
        [ "a" := 1
        , "b" := 2
        , "a" := 3
        , L.while ("a" ==: 0) Skip  -- test variable access
        ]
    expected =
        let expectedVars =
                [ ("a", 3)
                , ("b", 2)
                ]
        in  ExecState [] [] expectedVars Skip

ioTest :: ExecWay Stmt -> Property
ioTest = sample ~*~ id @Value
  where
    sample = mconcat
        [ Read "a"
        , Write "a"
        ]

whileTest :: ExecWay Stmt -> Property
whileTest = sample & [] >-*-> [0 .. 4]
  where
    sample = mconcat
        [ "i" := 0
        , L.while ("i" <: 5) $ mconcat
            [ Write "i"
            , "i" := "i" +: 1
            ]
        ]

errorsTest :: Property
errorsTest = conjoin $
    [ Write (5 /: 0)
    , Read "x"
    , Write "x"
    ] <&> [] >--> X

fibTest :: ExecWay Stmt -> Property
fibTest = sample ~*~ fib . getNonNegative
  where
    sample = mconcat
        [ "a" := 0
        , "b" := 1
        , Read "i"
        , L.while ("i" >: 0) $ mconcat
            [ "c" := "b"
            , "b" := "a" +: "b"
            , "a" := "c"
            , "i" := "i" -: 1
            ]
        , Write "a"
        ]
    fibs :: [Value]
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
    fib :: Value -> Value
    fib = (fibs !!) . fromIntegral

gcdTest :: ExecWay Stmt -> Property
gcdTest = sample ~*~ gcd'
  where
    sample = mconcat
        [ Read "a"
        , Read "b"
        , L.while ("b" >: 0) $ mconcat
            [ "r" := "a" %: "b"
            , "a" := "b"
            , "b" := "r"
            ]
        , Write "a"
        ]
    gcd' :: NonNegative Value -> NonNegative Value -> Value
    gcd' (NonNegative a) (NonNegative b) = gcd a b

minTest :: ExecWay Stmt -> Property
minTest = sample ~*~ min @Value
  where
    sample = mconcat
        [ Read "a"
        , Read "b"
        , If ("a" <: "b")
            ("c" := "a")
            ("c" := "b")
        , Write "c"
        ]

fileTest :: Either String FullTestData -> Property
fileTest (Left err) =
    counterexample ("Parse failed: " ++ err) False
fileTest (Right FullTestData{..}) =
    ftdProgram & ftdInput >--> TestRes ftdOutput
