{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Examples.BaseSpec
    ( spec
    ) where

import           Prelude         (tail, (!!))
import           Test.Hspec      (Spec, describe, it)
import           Test.QuickCheck (NonNegative (..), Property, Small (..), conjoin,
                                  counterexample, property, (==>))
import           Universum

import           Test.Arbitrary  ()
import           Test.Execution  (TestRes (..), describeExecWays, (>-*->), (>-->), (~*~))
import           Test.Util       (VerySmall (..))
import           Test.Walker     (FullTestData (..), describeDir)
import           Toy.Base
import           Toy.Execution   (ExecWay (..), defCompileX86, transShow, translateLang,
                                  (<~~>))
import           Toy.Exp
import           Toy.Lang        (Stmt (..))
import qualified Toy.Lang        as L

spec :: Spec
spec = do
    describeExecWays [Ex @Stmt transShow] $ \_ -> do
        describe "examples" $ do
            it "different erroneous scenarios" $
                errorsTest

    let ways =
            [ Ex transShow
            , Ex translateLang
            , Ex $ translateLang <~~> defCompileX86
            ]
    describeExecWays ways $ \way -> do

      describe "examples" $ do
            it "no actions" $
                noActions way

            describe "io" $ do
                it "write" $
                    property $ writeTest way

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
                it "allocation" $
                    property $ arrayAllocTest way

                it "allocation of two arrays subsequently" $
                    property $ arrayAlloc2Test way

                it "arrlen" $
                    property $ arrayLengthTest way

                -- it "simple" $
                    -- property $ arraySimpleTest way

                it "safe store" $
                    property $ safeStoreTest way

                it "deep" $
                    property $ arrayDeepTest way

                it "long nested" $
                    property $ arrayLongNestedTest way

                it "set gc" $
                    property $ arraySetGcTest way

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
    sample = If 1 (L.writeS 0) (L.writeS 1)

ifFalseTest :: ExecWay Stmt -> Property
ifFalseTest = sample & [] >-*-> [1]
  where
    sample = If 0 (L.writeS 0) (L.writeS 1)

ioTest :: ExecWay Stmt -> Property
ioTest = sample ~*~ identity @Value
  where
    sample = mconcat
        [ L.readS "a"
        , L.writeS "a"
        ]

writeTest :: ExecWay Stmt -> Value -> Property
writeTest way v = sample & [] >-*-> [v] $ way
  where
    sample = L.writeS (ValueE v)

whileTest :: ExecWay Stmt -> Property
whileTest = sample & [] >-*-> [0 .. 4]
  where
    sample = mconcat
        [ "i" := 0
        , L.whileS ("i" <: 5) $ mconcat
            [ L.writeS "i"
            , "i" := "i" +: 1
            ]
        ]

arrayAllocTest :: ExecWay Stmt -> Property
arrayAllocTest = sample & [] >-*-> []
  where
    sample = "a" `L.arrayVarS` []

arrayAlloc2Test :: ExecWay Stmt -> Property
arrayAlloc2Test = sample & [] >-*-> []
  where
    sample = mconcat . replicate 2 $ "a" `L.arrayVarS` []

arrayLengthTest :: ExecWay Stmt -> (NonNegative (Small Value)) -> Property
arrayLengthTest way (NonNegative (Small k)) =
    sample & [] >-*-> [k] $ way
  where
    sample = L.writeS $ FunE "arrlen" [ArrayUninitE $ fromIntegral k]

arraySimpleTest :: ExecWay Stmt -> (NonNegative (Small Value)) -> Property
arraySimpleTest way (NonNegative (Small k)) =
    k `elem` range ==> ([k] >-*-> [k]) sample way
  where
    sample = mconcat
        [ "a" `L.arrayVarS` (ValueE <$> range)
        , "i" := readE
        , L.writeS ("a" !!: "i")
        ]
    range = [0 .. 5]

safeStoreTest :: ExecWay Stmt -> Property
safeStoreTest = sample & [] >-*-> [11]
  where
    sample = mconcat
        [ "a" `L.arrayVarS` [11]
        , "a" := "a"
        , L.writeS ("a" !!: 0)
        ]

arrayDeepTest :: ExecWay Stmt
              -> NonNegative (VerySmall Value)
              -> NonNegative (VerySmall Value)
              -> Property
arrayDeepTest way (NonNegative (VerySmall k1)) (NonNegative (VerySmall k2)) =
    k1 == k2 ==> ([] >-*-> [7]) sample way
  where
    sample = mconcat
        [ "a" := 7
        , mconcat . replicate (fromIntegral k1) $  -- {{{... 1 ...}}}
              ("a" :=) `L.arrayS` ["a"]
        , L.forS ("i" := 0) ("i" <: ValueE k2) ("i" := "i" + 1) $
              "a" := "a" !!: 0
        , L.writeS "a"
        ]

arrayLongNestedTest :: ExecWay Stmt -> ([Value], [Value]) -> Property
arrayLongNestedTest way (vs0, vs1) = sample & [] >-*-> [100500] $ way
  where
    sample = mconcat
        [ "a0" `L.arrayVarS` (ValueE <$> vs0)
        , "a1" `L.arrayVarS` (ValueE <$> vs1)
        , "a" `L.arrayVarS` ["a0", "a1"]
        , L.writeS 100500
        ]

arraySetGcTest  :: ExecWay Stmt -> Property
arraySetGcTest = sample & [] >-*-> []
  where
    sample = mconcat
        [ "a0"  `L.arrayVarS` (ValueE <$> [1])
        , "a0_" `L.arrayVarS` (ValueE <$> [2])
        , "a" `L.arrayVarS` ["a0"]
        , ArrayAssign "a" 0 "a0_"
        ]

errorsTest :: Property
errorsTest = conjoin $
    [ L.writeS (5 /: 0)
    , L.readS "x"
    , L.writeS "x"
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
        , L.writeS "a"
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
        , L.writeS "a"
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
        , L.writeS "c"
        ]

fileTest :: Either Text FullTestData -> Property
fileTest (Left err) =
    counterexample ("Parse failed: " ++ toString err) False
fileTest (Right FullTestData{..}) =
    ftdProgram & ftdInput >--> TestRes ftdOutput
