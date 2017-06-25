{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Examples.YComplexSpec
    ( spec
    ) where

import           Prelude         (tail, (!!))
import           Test.Hspec      (Spec, describe, it)
import           Test.QuickCheck (NonNegative (..), Property, Small (..), counterexample,
                                  property)
import           Universum

import           Test.Arbitrary  ()
import           Test.Execution  (TestRes (..), describeExecWays, (>-->), (~*~))
import           Test.Walker     (FullTestData (..), describeDir)
import           Toy.Base
import           Toy.Execution   (ExecWay (..), defCompileX86, transShow, translateLang,
                                  (<~~>))
import           Toy.Exp
import           Toy.Lang        (Stmt (..))
import qualified Toy.Lang        as L

spec :: Spec
spec = do
    let ways =
            [ Ex transShow
            , Ex translateLang
            , Ex $ translateLang <~~> defCompileX86
            ]
    describeExecWays ways $ \way -> do

      describe "examples" $ do
            describe "complex" $ do
                it "min" $
                    property $ minTest way

                it "fib" $
                    property $ fibTest way

                it "gcd" $
                    property $ gcdTest way

      describeDir "./test/cases/exec"
          fileTest

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
