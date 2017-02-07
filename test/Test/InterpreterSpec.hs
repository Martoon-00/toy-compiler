{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Test.InterpreterSpec
    ( spec
    ) where

import           Test.Hspec           (Spec, describe, it)
import           Test.QuickCheck      (Discard (..), Property, once, property,
                                       within, (===), (==>))

import           Compiler.Data        (ExecState (..), Exp (..), Stmt (..), int,
                                       simpleExecState)
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
initSkipTest = once $ executeDebug sample == Right expected
  where
    sample   = simpleExecState SkipS
    expected = simpleExecState SkipS


ifTrueTest :: Property
ifTrueTest = once $ executeDebug sample == Right expected
  where
    sample   = simpleExecState $ IfS (1 :+ 2) (int 0) (int 1)
    expected = simpleExecState $ int 0

ifFalseTest :: Property
ifFalseTest = once $ executeDebug sample == Right expected
  where
    sample   = simpleExecState $ IfS (1 :- 1) (int 1) (int 0)
    expected = simpleExecState $ int 0
