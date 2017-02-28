{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Test.InterpreterSpec
    ( spec
    ) where

import qualified Data.Map             as M
import           Test.Hspec           (Spec, describe, it)
import           Test.QuickCheck      (Discard (..), NonNegative (..), Property, conjoin,
                                       once, property, within, (===), (==>))

import           Test.Arbitrary       ()
import           Test.Util            (TestInOut (..), running)
import           Toy.Data
import           Toy.Lang.Data        (ExecState (..), Stmt (..), simpleExecState)
import           Toy.Lang.Interpreter (execute)


spec :: Spec
spec =
    describe "interpreter" $ do
        describe "examples" $ do
            it "Skip" $
                initSkipTest
            it "If true" $
                ifTrueTest
            it "If false" $
                ifFalseTest
            it "variables simple" $
                varsTest
            it "io simple" $
                ioTest
            it "While simple" $
                whileTest
            it "error simple" $
                errorTest
            it "different erroneous scenarios" $
                errorsTest
            describe "complex" $ do
                it "fib" $
                    property fibTest
                it "gcd" $
                    property gcdTest

        it "`execute` always ends with Skip" $
            property executeAlwaysEndsWithSkip

executeAlwaysEndsWithSkip :: ExecState -> Property
executeAlwaysEndsWithSkip initExecState@(ExecState _ _ _ initStmt) =
    within 1000000 $
    -- propability of initial action to be SkipS is too high
    initStmt /= Skip ==>
        case execute initExecState of
            Left _                      -> property Discard
            Right (ExecState _ _ _ end) -> end === Skip


initSkipTest :: Property
initSkipTest = once $ execute sample === Right expected
  where
    sample   = simpleExecState Skip
    expected = simpleExecState Skip

ifTrueTest :: Property
ifTrueTest = once $ running sample $ [] :~~> [0]
  where
    sample = If (1 +: 2) (Write 0) (Write 1)

ifFalseTest :: Property
ifFalseTest = once $ running sample $ [] :~~> [1]
  where
    sample = If (1 -: 1) (Write 0) (Write 1)

varsTest :: Property
varsTest = once $ execute sample === Right expected
  where
    sample = simpleExecState $ mconcat
        [ "a" := 1
        , "b" := 2
        , "a" := 3
        , While ("a" ==: 0) Skip  -- test variable access
        ]
    expected =
        let expectedVars = M.fromList
                [ ("a", 3)
                , ("b", 2)
                ]
        in  ExecState [] [] expectedVars Skip

ioTest :: Property
ioTest = once $ running sample $ [5] :~~> [7]
  where
    sample = mconcat
        [ Read "a"
        , Write ("a" +: 2)
        ]

whileTest :: Property
whileTest = once $ running sample $ [] :~~> [4, 3 .. 0]
  where
    sample = mconcat
        [ "i" := 0
        , While ("i" <: 5) $ mconcat
            [ Write "i"
            , "i" := "i" +: 1
            ]
        ]

errorTest :: Property
errorTest = once $ running sample $ [] :~~% ()
  where
    sample = Write (5 /: 0)

errorsTest :: Property
errorsTest = property $
    conjoin $ (\sample -> running sample $ [] :~~% ()) <$>
        [ Write (5 /: 0)
        , Read "x"
        , Write "x"
        ]

fibTest :: NonNegative Value -> Property
fibTest (NonNegative i) = running sample $ [i] :~~> [fib !! i]
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
    fib = 0 : 1 : zipWith (+) fib (tail fib)

gcdTest :: NonNegative Value -> NonNegative Value -> Property
gcdTest (NonNegative a) (NonNegative b) =
    running sample $ [a, b] :~~> [gcd a b]
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
