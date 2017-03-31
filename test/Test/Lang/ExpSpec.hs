{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Test.Lang.ExpSpec
    ( spec
    ) where

import           Data.Bits       (xor)
import           Test.Hspec      (Spec, describe, it)
import           Test.QuickCheck (Property)

import           Test.Arbitrary  ()
import           Test.Execution  (ExecWay (..), asIs, defCompileX86, describeExecWays,
                                  translateLang, (<~~>), (~*~))
import           Toy.Exp
import           Toy.Lang        (Stmt (..))


spec :: Spec
spec = do
    let ways =
            [ Ex asIs
            , Ex translateLang
            , Ex $ translateLang <~~> defCompileX86
            ]
    describeExecWays ways $ \way -> do
        describe "expressions" $ do
            describe "arithmetic" $ do
                it "plus" $
                    plusTest way
                it "minus" $
                    minusTest way
                it "div" $
                    divTest way
                it "two variables" $
                    twoVarsTest way
                it "complex" $
                    complexArithTest way
            describe "comparisons" $ do
                it "simple" $
                    boolTest way


plusTest :: ExecWay Stmt -> Property
plusTest = (+) @Value 5 ~*~ sample
  where
    sample = mconcat
        [ Read "a"
        , Write $ "a" + 5
        ]

minusTest :: ExecWay Stmt -> Property
minusTest = subtract @Value 1 ~*~ sample
  where
    sample = mconcat
        [ Read "a"
        , Write $ "a" - 1
        ]

divTest :: ExecWay Stmt -> Property
divTest = quot @Value 6 ~*~ sample
  where
    sample = mconcat
        [ Read "a"
        , Write $ 6 /: "a"
        ]

twoVarsTest :: ExecWay Stmt -> Property
twoVarsTest = (+) @Value ~*~ sample
  where
    sample = mconcat
        [ Read "a"
        , Read "b"
        , Write $ "a" +: "b"
        ]

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
