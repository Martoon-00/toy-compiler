{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Test.InterpreterSpec
    ( spec
    ) where

import           Control.Lens         (has, (^?), _1, _Left)
import qualified Data.Map             as M
import           Test.Hspec           (Spec, describe, it)
import           Test.QuickCheck      (Discard (..), Property, once, property,
                                       within, (===), (==>))

import           Compiler.Data        (ExecState (..), Exp (..), Stmt (..),
                                       getIO, int, simpleExecState)
import           Compiler.Interpreter (execute, executeDebug)
import           Test.Arbitrary       ()


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

        it "`execute` always ends with Skip" $
            property executeAlwaysEndsWithSkip

executeAlwaysEndsWithSkip :: ExecState -> Property
executeAlwaysEndsWithSkip initExecState@(ExecState _ _ _ initStmt) =
    within 1000000 $
    -- propability of initial action to be SkipS is too high
    initStmt /= SkipS ==>
        case execute initExecState of
            Left _                      -> property Discard
            Right (ExecState _ _ _ end) -> end === SkipS


initSkipTest :: Property
initSkipTest = once $ executeDebug sample === Right expected
  where
    sample   = simpleExecState SkipS
    expected = simpleExecState SkipS

ifTrueTest :: Property
ifTrueTest = once $ executeDebug sample === Right expected
  where
    sample   = simpleExecState $ IfS (1 :+ 2) (int 0) (int 1)
    expected = simpleExecState $ int 0

ifFalseTest :: Property
ifFalseTest = once $ executeDebug sample === Right expected
  where
    sample   = simpleExecState $ IfS (1 :- 1) (int 1) (int 0)
    expected = simpleExecState $ int 0

varsTest :: Property
varsTest = once $ executeDebug sample === Right expected
  where
    sample   = simpleExecState $ mconcat
        [ "a" := 1
        , "b" := 2
        , "a" := 3
        , WhileS ("a" :== 0) SkipS  -- test variable access
        ]
    expected =
        let expectedVars = M.fromList
                [ ("a", 3)
                , ("b", 2)
                ]
        in  ExecState [] [] expectedVars SkipS

ioTest :: Property
ioTest = once $ (getIO <$> executeDebug sample) === Right expected
  where
    sample   = ExecState [5] [] M.empty $ mconcat
        [ ReadS "a"
        , WriteS ("a" :+ 2)
        ]
    expected = ([], [7])

whileTest :: Property
whileTest = once $ (getIO <$> executeDebug sample) === Right expected
  where
    sample   = simpleExecState $ mconcat
        [ "i" := 0
        , WhileS ("i" :< 5) $ mconcat
            [ WriteS "i"
            , "i" := "i" :+ 1
            ]
        ]
    expected = ([], [4, 3 .. 0])

errorTest :: Property
errorTest = once $
    executeDebug (simpleExecState sample) ^? _Left . _1 === Just sample
  where
    sample = WriteS (5 :/ 0)

errorsTest :: Property
errorsTest = once $
    all (\sample -> _Left `has` executeDebug (simpleExecState sample))
        [ WriteS (5 :/ 0)
        , ReadS "x"
        , WriteS "x"
        ]

fibTest :: Property
fibTest = once $ (getIO <$> executeDebug sample) === Right expected
  where
    sample   = ExecState [12] [] M.empty $ mconcat
        [ "a" := 0
        , "b" := 1
        , ReadS "i"
        , WhileS ("i" :> 0) $ mconcat
            [ "c" := "b"
            , "b" := "a" :+ "b"
            , "a" := "c"
            , "i" := "i" :- 1
            ]
        , WriteS "a"
        ]
    expected = ([], [144])
