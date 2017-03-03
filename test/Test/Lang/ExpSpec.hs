{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Test.Lang.ExpSpec
    ( spec
    ) where

import           Test.Hspec      (Spec, describe, it)
import           Test.QuickCheck (Property)

import           Test.Arbitrary  ()
import           Test.Util       (ExecWay (..), describeExecWays, (~*~))
import           Toy.Data
import           Toy.Lang.Data   (Stmt (..))


spec :: Spec
spec =
    describeExecWays [Interpret, Translate] $ \way -> do
        describe "expressions" $ do
            describe "arithmetic" $ do
                it "plus" $
                    plusTest way
                it "div" $
                    divTest way
                it "complex" $
                    complexArithTest way


plusTest :: ExecWay -> Property
plusTest = (+) @Value 5 ~*~ sample
  where
    sample = mconcat
        [ Read "a"
        , Write $ "a" +: 5
        ]

divTest :: ExecWay -> Property
divTest = div @Value 2 ~*~ sample
  where
    sample = mconcat
        [ Read "a"
        , Write $ 2 /: "a"
        ]

complexArithTest :: ExecWay -> Property
complexArithTest = fun ~*~ sample
  where
    sample = mconcat
        [ Read "a"
        , Read "b"
        , Read "c"
        , Write $ "a" +: "b" *: 10 -: "c" /: 2
        ]
    fun :: Value -> Value -> Value -> Value
    fun a b c = a + b * 10 - (c `div` 2)
