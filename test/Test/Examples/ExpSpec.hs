{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Examples.ExpSpec
    ( spec
    ) where

import           Control.Category (id, (.))
import           Control.Lens     ((&))
import           Data.Bits        (xor, (.&.), (.|.))
import           Data.Monoid      ((<>))
import           Prelude          hiding (id, (.))
import           Test.Hspec       (Spec, describe, it)
import           Test.QuickCheck  (Large (..), Property, counterexample)

import           Test.Arbitrary   ()
import           Test.Execution   (describeExecWays, (>-*->), (~*~))
import           Toy.Base
import           Toy.Execution    (ExecWay (..), defCompileX86, translateLang)
import           Toy.Exp
import qualified Toy.Lang         as L


spec :: Spec
spec = do
    let ways =
            [ Ex id
            , Ex translateLang
            , Ex $ defCompileX86 . translateLang
            ]
    describe "expressions" $ do
        describeExecWays ways $ \way -> do
            describe "arithmetic" $ do
                it "plus (uni)" $
                    uniopTest way (+ 5) (+ 5)
                it "minus (uni)" $
                    uniopTest way (subtract 1) (subtract 1)
                it "div (uni)" $
                    uniopTest way (quot 6) (6 /:)
                it "two variables" $
                    binopTest way const const
                it "plus" $
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
            describe "misc" $ do
                it "large" $
                    largeTest way


uniopTest
    :: ExecWay L.Stmt
    -> (Value -> Value)
    -> (Exp -> Exp)
    -> Property
uniopTest way f1 f2 =
    let sample = L.read "a" <> L.write (f2 "a")
    in  way & sample ~*~ f1

binopTest
    :: ExecWay L.Stmt
    -> (Value -> Value -> Value)
    -> (Exp -> Exp -> Exp)
    -> Property
binopTest way f1 f2 = head
    [ counterexample "plain" $
        way & sample ~*~ f1
    , counterexample "large" $
        way & sample ~*~ \(Large a) (Large b) -> f1 a b
    ]
  where
    sample = L.read "a" <> L.read "b" <> L.write ("a" `f2` "b")

complexArithTest :: ExecWay L.Stmt -> Property
complexArithTest = sample ~*~ fun
  where
    sample = mconcat
        [ L.read "a"
        , L.read "b"
        , L.read "c"
        , L.write $ "a" +: "b" *: 10 -: "c" %: 2
        ]
    fun :: Value -> Value -> Value -> Value
    fun a b c = a + b * 10 - (c `rem` 2)

boolTest :: ExecWay L.Stmt -> Property
boolTest = sample ~*~ fun
  where
    sample = mconcat
        [ L.read "a"
        , L.read "b"
        , L.read "c"
        , L.read "d"
        , L.read "e"
        , L.write $ "a" ==: "b" &&: "c" <=: "d" ^: "e"
        ]
    fun :: Value -> Value -> Value -> Value -> Value -> Value
    fun a b c d e = if (a == b) && (c <= xor d e) then 1 else 0

largeTest :: ExecWay L.Stmt -> Property
largeTest = sample & [] >-*-> [55]
  where
    sample = L.write $ foldr (+) 0 (ValueE <$> [1..10] :: [Exp])
