{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Test.Examples.BaseSpec
    ( spec
    ) where

import           Control.Lens    ((&), (<&>))
import           Test.Hspec      (Spec, describe, it)
import           Test.QuickCheck (NonNegative (..), Property, Small (..), conjoin,
                                  counterexample, property, (==>))
import           Universum       (Text, toString)

import           Test.Arbitrary  ()
import           Test.Execution  (TestRes (..), describeExecWays, (>-*->), (>-->), (~*~))
import           Test.Util       (VerySmall (..))
import           Test.Walker     (FullTestData (..), describeDir)
import           Toy.Base
import           Toy.Execution   (ExecWay (..), asIs, defCompileX86, translateLang,
                                  (<~~>))
import           Toy.Exp
import           Toy.Lang        (Stmt (..), writeS)
import qualified Toy.Lang        as L

spec :: Spec
spec = do
    describeExecWays [Ex @Stmt asIs] $ \_ -> do
        describe "examples" $ do
            it "different erroneous scenarios" $
                errorsTest

    let ways =
            [ Ex asIs
            , Ex translateLang
            , Ex $ translateLang <~~> defCompileX86
            ]
    describeExecWays ways $ \way -> do
        describe "examples" $ do
            it "no actions" $
                noActions way

            it "io simple" $
                ioTest way

            describe "if" $ do
                it "true" $
                    ifTrueTest way

                it "false" $
                    ifFalseTest way

                it "simple" $
                    property $ minTest way

            it "while simple" $
                whileTest way

            describe "arrays" $ do
                it "allocation works" $
                    property $ arrayAllocTest way

                it "simple" $
                    property $ arraySimpleTest way

                it "deep" $
                    property $ arrayDeepTest way

            describe "complex" $ do
                it "fib" $
                    property $ fibTest way

                it "gcd" $
                    property $ gcdTest way


    describeDir "./test/cases/exec"
        fileTest

noActions :: ExecWay Stmt -> Property
noActions = mempty & [] >-*-> []

ifTrueTest :: ExecWay Stmt -> Property
ifTrueTest = sample & [] >-*-> [0]
  where
    sample = If 1 (writeS 0) (writeS 1)

ifFalseTest :: ExecWay Stmt -> Property
ifFalseTest = sample & [] >-*-> [1]
  where
    sample = If 0 (writeS 0) (writeS 1)

ioTest :: ExecWay Stmt -> Property
ioTest = sample ~*~ id @Value
  where
    sample = mconcat
        [ L.readS "a"
        , writeS "a"
        ]

whileTest :: ExecWay Stmt -> Property
whileTest = sample & [] >-*-> [0 .. 4]
  where
    sample = mconcat
        [ "i" := 0
        , L.whileS ("i" <: 5) $ mconcat
            [ writeS "i"
            , "i" := "i" +: 1
            ]
        ]

arrayAllocTest :: ExecWay Stmt -> Property
arrayAllocTest = sample & [] >-*-> []
  where
    sample = "a" `L.arrayVarS` []

arraySimpleTest :: ExecWay Stmt -> (NonNegative (Small Value)) -> Property
arraySimpleTest way (NonNegative (Small k)) =
    k `elem` range ==> ([k] >-*-> [k]) sample way
  where
    sample = mconcat
        [ "a" `L.arrayVarS` (ValueE <$> range)
        , "i" := readE
        , writeS ("a" !!: "i")
        ]
    range = [0 .. 5]

arrayDeepTest :: ExecWay Stmt
              -> NonNegative (VerySmall Value)
              -> NonNegative (VerySmall Value)
              -> Property
arrayDeepTest way (NonNegative (VerySmall k1)) (NonNegative (VerySmall k2)) =
    k1 == k2 ==> ([] >-*-> [1]) sample way
  where
    sample = mconcat
        [ "a" := 1
        , mconcat . replicate (fromIntegral k1) $  -- {{{... 1 ...}}}
              ("a" :=) `L.arrayS` ["a"]
        , L.forS ("i" := 0) ("i" <: ValueE k2) ("i" := "i" + 1) $
              "a" := "a" !!: 0
        , writeS "a"
        ]

errorsTest :: Property
errorsTest = conjoin $
    [ writeS (5 /: 0)
    , L.readS "x"
    , writeS "x"
    ] <&> [] >--> X

fibTest :: ExecWay Stmt -> Property
fibTest = sample ~*~ fib . getNonNegative
  where
    sample = mconcat
        [ "a" := 0
        , "b" := 1
        , L.readS "i"
        , L.whileS ("i" >: 0) $ mconcat
            [ "c" := "b"
            , "b" := "a" +: "b"
            , "a" := "c"
            , "i" := "i" -: 1
            ]
        , writeS "a"
        ]
    fibs :: [Value]
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
    fib :: Small Value -> Value
    fib = (fibs !!) . fromIntegral . getSmall

gcdTest :: ExecWay Stmt -> Property
gcdTest = sample ~*~ gcd'
  where
    sample = mconcat
        [ L.readS "a"
        , L.readS "b"
        , L.whileS ("b" >: 0) $ mconcat
            [ "r" := "a" %: "b"
            , "a" := "b"
            , "b" := "r"
            ]
        , writeS "a"
        ]
    gcd' :: NonNegative Value -> NonNegative Value -> Value
    gcd' (NonNegative a) (NonNegative b) = gcd a b

minTest :: ExecWay Stmt -> Property
minTest = sample ~*~ min @Value
  where
    sample = mconcat
        [ L.readS "a"
        , L.readS "b"
        , If ("a" <: "b")
            ("c" := "a")
            ("c" := "b")
        , writeS "c"
        ]

fileTest :: Either Text FullTestData -> Property
fileTest (Left err) =
    counterexample ("Parse failed: " ++ toString err) False
fileTest (Right FullTestData{..}) =
    ftdProgram & ftdInput >--> TestRes ftdOutput
