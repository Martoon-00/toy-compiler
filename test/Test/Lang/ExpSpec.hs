{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Test.Lang.ExpSpec
    ( spec
    ) where

import           Test.Hspec      (Spec, describe, it)
import           Test.QuickCheck (Property)

import           Test.Arbitrary  ()
import           Test.Util       (alsoSM, (~~))
import           Toy.Data
import           Toy.Lang.Data   (Stmt (..))


spec :: Spec
spec =
    describe "lang" $ do
        describe "expressions" $ do
            describe "arithmetic" $ do
                it "plus"
                    plusTest
                it "div"
                    divTest
                it "complex"
                    complexArithTest


plusTest :: Property
plusTest = (+) @Value 5 ~~ alsoSM sample
  where
    sample = mconcat
        [ Read "a"
        , Write $ "a" +: 5
        ]

divTest :: Property
divTest = div @Value 2 ~~ alsoSM sample
  where
    sample = mconcat
        [ Read "a"
        , Write $ 2 /: "a"
        ]

complexArithTest :: Property
complexArithTest = fun ~~ alsoSM sample
  where
    sample = mconcat
        [ Read "a"
        , Read "b"
        , Read "c"
        , Write $ "a" +: "b" *: 10 -: "c" /: 2
        ]
    fun :: Value -> Value -> Value -> Value
    fun a b c = a + b * 10 - (c `div` 2)
