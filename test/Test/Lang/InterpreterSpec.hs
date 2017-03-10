{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Test.Lang.InterpreterSpec
    ( spec
    ) where

import           Control.Lens    ((&), (<&>))
import           Test.Hspec      (Spec, describe, it)
import           Test.QuickCheck (Discard (..), NonNegative (..), Property, conjoin,
                                  counterexample, property, within, (===), (==>))

import           Test.Arbitrary  ()
import           Test.Util       (ExecWay (..), TestRes (..), describeExecWays, (>-->),
                                  (~*~), (~~))
import           Test.Walker     (FullTestData (..), describeDir)
import           Toy.Exp
import           Toy.Lang        (ExecState (..), Stmt (..), execute, simpleExecState)


spec :: Spec
spec = do
    describeExecWays [Interpret] $ \_ -> do
        describe "examples" $ do
            it "Skip" $
                initSkipTest
            it "If true" $
                ifTrueTest
            it "If false" $
                ifFalseTest
            it "variables simple" $
                varsTest
            it "While simple" $
                whileTest
            it "If simple" $
                property minTest
            it "different erroneous scenarios" $
                errorsTest
            describe "complex" $ do
                it "fib" $
                    property fibTest
                it "gcd" $
                    property gcdTest

        it "`execute` always ends with Skip" $
            property executeAlwaysEndsWithSkip

    describeExecWays [Interpret, Translate] $ \way -> do
        describe "examples" $ do
            it "io simple" $
                ioTest way

    describeExecWays [Interpret] $ \_ -> do
        describeDir "./test/cases/exec"
            fileTest


executeAlwaysEndsWithSkip :: ExecState -> Property
executeAlwaysEndsWithSkip initExecState@(ExecState _ _ _ initStmt) =
    within 1000000 $
    -- propability of initial action to be SkipS is too high
    initStmt /= Skip ==>
        case execute initExecState of
            Left _                      -> property Discard
            Right (ExecState _ _ _ end) -> end === Skip


initSkipTest :: Property
initSkipTest = execute sample === Right expected
  where
    sample   = simpleExecState Skip
    expected = simpleExecState Skip

ifTrueTest :: Property
ifTrueTest = sample & [] >--> [0]
  where
    sample = If 1 (Write 0) (Write 1)

ifFalseTest :: Property
ifFalseTest = sample & [] >--> [1]
  where
    sample = If 0 (Write 0) (Write 1)

varsTest :: Property
varsTest = execute sample === Right expected
  where
    sample = simpleExecState $ mconcat
        [ "a" := 1
        , "b" := 2
        , "a" := 3
        , While ("a" ==: 0) Skip  -- test variable access
        ]
    expected =
        let expectedVars =
                [ ("a", 3)
                , ("b", 2)
                ]
        in  ExecState [] [] expectedVars Skip

ioTest :: ExecWay -> Property
ioTest = id @Value ~*~ sample
  where
    sample = mconcat
        [ Read "a"
        , Write "a"
        ]

whileTest :: Property
whileTest = sample & [] >--> [4, 3 .. 0]
  where
    sample = mconcat
        [ "i" := 0
        , While ("i" <: 5) $ mconcat
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

fibTest :: Property
fibTest = (fib !!) . getNonNegative ~~ sample
  where
    sample = mconcat
        [ "a" := 0
        , "b" := 1
        , Read "i"
        , While ("i" >: 0) $ mconcat
            [ "c" := "b"
            , "b" := "a" +: "b"
            , "a" := "c"
            , "i" := "i" -: 1
            ]
        , Write "a"
        ]
    fib :: [Value]
    fib = 0 : 1 : zipWith (+) fib (tail fib)

gcdTest :: Property
gcdTest = gcd' ~~ sample
  where
    sample = mconcat
        [ Read "a"
        , Read "b"
        , While ("b" >: 0) $ mconcat
            [ "r" := "a" %: "b"
            , "a" := "b"
            , "b" := "r"
            ]
        , Write "a"
        ]
    gcd' :: NonNegative Value -> NonNegative Value -> Value
    gcd' (NonNegative a) (NonNegative b) = gcd a b

minTest :: Property
minTest = min @Value ~~ sample
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
