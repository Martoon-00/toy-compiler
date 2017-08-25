{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Examples.IArraySpec
    ( spec
    ) where

import           Test.Hspec      (Spec, describe, it)
import           Test.QuickCheck (NonNegative (..), Property, Small (..), property, (==>))
import           Universum

import           Test.Arbitrary  ()
import           Test.Execution  (describeExecWays, (>-*->))
import           Test.Util       (VerySmall (..))
import           Toy.Base
import           Toy.Execution   (ExecWay (..), Executable, TranslateToSM, defCompileX86,
                                  transShow, translateLang, (<~~>))
import           Toy.Exp
import           Toy.Lang        (Stmt (..))
import qualified Toy.Lang        as L

spec :: Spec
spec = do
    let ways :: forall l. (Executable l, Show l, TranslateToSM l) => [ExecWay l]
        ways =
            [ Ex transShow
            , Ex translateLang
            , Ex $ translateLang <~~> defCompileX86
            ]
    describeExecWays ways $ \way -> do

      describe "examples" $ do
            describe "arrays" $ do
                it "allocation" $
                    property $ arrayAllocTest way

                it "allocation of two arrays subsequently" $
                    property $ arrayAlloc2Test way

                it "arrlen" $
                    property $ arrayLengthTest way

                it "simple" $
                    property $ arraySimpleTest way

                it "safe store" $
                    property $ safeStoreTest way

                it "deep" $
                    property $ arrayDeepTest way

                it "long nested" $
                    property $ arrayLongNestedTest way

                it "set gc" $
                    property $ arraySetGcTest way

    describeExecWays ways $ \way -> do
      describe "examples" $ do
            describe "arrays" $ do
                describe "funs" $ do
                    it "array argument" $
                        arrayArgTest way
                    it "array return" $
                        arrayReturnTest way


arrayAllocTest :: ExecWay Stmt -> Property
arrayAllocTest = sample & [] >-*-> []
  where
    sample = "a" `L.arrayVar` []

arrayAlloc2Test :: ExecWay Stmt -> Property
arrayAlloc2Test = sample & [] >-*-> []
  where
    sample = mconcat . replicate 2 $ "a" `L.arrayVar` []

arrayLengthTest :: ExecWay Stmt -> (NonNegative (Small Value)) -> Property
arrayLengthTest way (NonNegative (Small k)) =
    sample & [] >-*-> [k] $ way
  where
    sample = L.write $ FunE "arrlen" [ArrayUninitE $ fromIntegral k]

arraySimpleTest :: ExecWay Stmt -> (NonNegative (Small Value)) -> Property
arraySimpleTest way (NonNegative (Small k)) =
    k `elem` range ==> ([k] >-*-> [k]) sample way
  where
    sample = mconcat
        [ "a" `L.arrayVar` (ValueE <$> range)
        , "i" := readE
        , L.write ("a" !!: "i")
        ]
    range = [0 .. 5]

safeStoreTest :: ExecWay Stmt -> Property
safeStoreTest = sample & [] >-*-> [11]
  where
    sample = mconcat
        [ "a" `L.arrayVar` [11]
        , "a" := "a"
        , L.write ("a" !!: 0)
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
              ("a" :=) `L.array` ["a"]
        , L.for ("i" := 0) ("i" <: ValueE k2) ("i" := "i" + 1) $
              "a" := "a" !!: 0
        , L.write "a"
        ]

arrayLongNestedTest :: ExecWay Stmt -> ([Value], [Value]) -> Property
arrayLongNestedTest way (vs0, vs1) = sample & [] >-*-> [100500] $ way
  where
    sample = mconcat
        [ "a0" `L.arrayVar` (ValueE <$> vs0)
        , "a1" `L.arrayVar` (ValueE <$> vs1)
        , "a" `L.arrayVar` ["a0", "a1"]
        , L.write 100500
        ]

arraySetGcTest  :: ExecWay Stmt -> Property
arraySetGcTest = sample & [] >-*-> []
  where
    sample = mconcat
        [ "a0"  `L.arrayVar` (ValueE <$> [1])
        , "a0_" `L.arrayVar` (ValueE <$> [2])
        , "a" `L.arrayVar` ["a0"]
        , ArrayAssign "a" 0 "a0_"
        ]


arrayArgTest :: ExecWay L.Program -> Property
arrayArgTest = sample & [] >-*-> [5]
  where
    fun = L.toFunDecls "lol" ["x"] $ L.write (ArrayAccessE "x" 0)
    sample =
        L.mkProgram fun $ mconcat
            [ "a" `L.arrayVar` [5]
            , L.funCall "lol" ["a"]
            ]

arrayReturnTest :: ExecWay L.Program -> Property
arrayReturnTest = sample & [] >-*-> [7]
  where
    fun = L.toFunDecls "lol" [] $ L.Return `L.array` [7]
    sample =
        L.mkProgram fun $ mconcat
            [ L.write $ FunE "lol" [] `ArrayAccessE` 0
            ]
