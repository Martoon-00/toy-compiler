{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Test.Examples.ExpSpec
    ( spec
    ) where

import           Control.Category (id, (.))
import           Control.Lens     ((&))
import           Data.Bits        (xor, (.&.), (.|.))
import           Data.Monoid      ((<>))
import           Prelude          hiding (id, (.))
import           Test.Hspec       (Spec, describe, it)
import           Test.QuickCheck  (Property)

import           Test.Arbitrary   ()
import           Test.Execution   (ExecWay (..), defCompileX86, describeExecWays,
                                   translateLang, (~*~))
import           Toy.Exp
import           Toy.Lang         (Stmt (..))


spec :: Spec
spec = do
    let ways =
            [ Ex id
            , Ex translateLang
            , Ex $ defCompileX86 . translateLang
            ]
    describeExecWays ways $ \way -> do
        describe "expressions" $ do
            describe "arithmetic" $ do
                it "plus" $
                    uniopTest way (+ 5) (+ 5)
                it "minus" $
                    uniopTest way (subtract 1) (subtract 1)
                it "div" $
                    uniopTest way (quot 6) (6 /:)
                it "two variables" $
                    binopTest way (+) (+)
                it "complex" $
                    complexArithTest way
            describe "boolean" $ do
                it "and" $
                    binopTest way (asToBool (&&)) (&&:)
                it "or" $
                    binopTest way (asToBool (||)) (||:)
                it "xor" $
                    binopTest way xor (^:)
                it "bitwise and" $
                    binopTest way (.&.) (&:)
                it "bitwise or" $
                    binopTest way (.|.) (|:)
            describe "comparisons" $ do
                it "<" $
                    binopTest way (binResToBool (<)) (<:)
                it "==" $
                    binopTest way (binResToBool (==)) (==:)
                it "complex" $
                    boolTest way


uniopTest
    :: ExecWay Stmt
    -> (Value -> Value)
    -> (Exp -> Exp)
    -> Property
uniopTest way f1 f2 = way & f1 ~*~ Read "a" <> Write (f2 "a")

binopTest
    :: ExecWay Stmt
    -> (Value -> Value -> Value)
    -> (Exp -> Exp -> Exp)
    -> Property
binopTest way f1 f2 =
    way & f1 ~*~ (Read "a" <> Read "b" <> Write ("a" `f2` "b"))

complexArithTest :: ExecWay Stmt -> Property
complexArithTest = fun ~*~ sample
  where
    sample = mconcat
        [ Read "a"
        , Read "b"
        , Read "c"
        , Write $ "a" +: "b" *: 10 -: "c" %: 2
        ]
    fun :: Value -> Value -> Value -> Value
    fun a b c = a + b * 10 - (c `rem` 2)

boolTest :: ExecWay Stmt -> Property
boolTest = fun ~*~ sample
  where
    sample = mconcat
        [ Read "a"
        , Read "b"
        , Read "c"
        , Read "d"
        , Read "e"
        , Write $ "a" ==: "b" &&: "c" <=: "d" ^: "e"
        ]
    fun :: Value -> Value -> Value -> Value -> Value -> Value
    fun a b c d e = if (a == b) && (c <= xor d e) then 1 else 0
