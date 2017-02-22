{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Test.InterpreterSpec
    ( spec
    ) where

import           Control.Lens         (has, (^?), _1, _Left)
import qualified Data.Map             as M
import           Test.Hspec           (Spec, describe, it)
import           Test.QuickCheck      (Discard (..), NonNegative (..), Property, once,
                                       property, within, (===), (==>))

import           Test.Arbitrary       ()
import           Toy.Data             (Exp (..), Value)
import           Toy.Lang.Data        (ExecState (..), Stmt (..), anExecState, getIO, int,
                                       simpleExecState)
import           Toy.Lang.Interpreter (execute, executeDebug)


spec :: Spec
spec =
    describe "interpreter" $ do
        describe "examples" $ do
            it "Skip" $
                property initSkipTest
            it "If true" $
                property ifTrueTest
            it "If false" $
                property ifFalseTest
            it "variables simple" $
                property varsTest
            it "io simple" $
                property ioTest
            it "While simple" $
                property whileTest
            it "error simple" $
                property errorTest
            it "different erroneous scenarios" $
                property errorsTest
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
initSkipTest = once $ executeDebug sample === Right expected
  where
    sample   = simpleExecState Skip
    expected = simpleExecState Skip

ifTrueTest :: Property
ifTrueTest = once $ executeDebug sample === Right expected
  where
    sample   = simpleExecState $ If (1 :+ 2) (int 0) (int 1)
    expected = simpleExecState $ int 0

ifFalseTest :: Property
ifFalseTest = once $ executeDebug sample === Right expected
  where
    sample   = simpleExecState $ If (1 :- 1) (int 1) (int 0)
    expected = simpleExecState $ int 0

varsTest :: Property
varsTest = once $ executeDebug sample === Right expected
  where
    sample   = simpleExecState $ mconcat
        [ "a" := 1
        , "b" := 2
        , "a" := 3
        , While ("a" :== 0) Skip  -- test variable access
        ]
    expected =
        let expectedVars = M.fromList
                [ ("a", 3)
                , ("b", 2)
                ]
        in  ExecState [] [] expectedVars Skip

ioTest :: Property
ioTest = once $ (getIO <$> executeDebug sample) === Right expected
  where
    sample   = ExecState [5] [] M.empty $ mconcat
        [ Read "a"
        , Write ("a" :+ 2)
        ]
    expected = ([], [7])

whileTest :: Property
whileTest = once $ (getIO <$> executeDebug sample) === Right expected
  where
    sample   = simpleExecState $ mconcat
        [ "i" := 0
        , While ("i" :< 5) $ mconcat
            [ Write "i"
            , "i" := "i" :+ 1
            ]
        ]
    expected = ([], [4, 3 .. 0])

errorTest :: Property
errorTest = once $
    executeDebug (simpleExecState sample) ^? _Left . _1 === Just sample
  where
    sample = Write (5 :/ 0)

errorsTest :: Property
errorsTest = once $
    all (\sample -> _Left `has` executeDebug (simpleExecState sample))
        [ Write (5 :/ 0)
        , Read "x"
        , Write "x"
        ]

fibTest :: NonNegative Value -> Property
fibTest (NonNegative i) =
    once $ (getIO <$> executeDebug sample) === Right expected
  where
    sample   = anExecState [i] $ mconcat
        [ "a" := 0
        , "b" := 1
        , Read "i"
        , While ("i" :> 0) $ mconcat
            [ "c" := "b"
            , "b" := "a" :+ "b"
            , "a" := "c"
            , "i" := "i" :- 1
            ]
        , Write "a"
        ]
    expected = ([], [fib !! i])
    fib = 0 : 1 : zipWith (+) fib (tail fib)

gcdTest :: NonNegative Value -> NonNegative Value -> Property
gcdTest (NonNegative a) (NonNegative b) =
    once $ (getIO <$> executeDebug sample) === Right expected
  where
    sample   = anExecState [a, b] $ mconcat
        [ Read "a"
        , Read "b"
        , While ("b" :> 0) $ mconcat
            [ "r" := "a" :% "b"
            , "a" := "b"
            , "b" := "r"
            ]
        , Write "a"
        ]
    expected = ([], [gcd a b])
