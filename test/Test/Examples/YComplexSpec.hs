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
import           Test.Execution  (TestRes (..), describeExecWays, (>-*->), (~*~))
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

      describeDir "./test/cases/exec" $
          fileTest way

fibTest :: ExecWay L.Program -> Property
fibTest = L.toProgram sample ~*~ fib . getNonNegative
  where
    sample = mconcat
        [ "a" := 0
        , "b" := 1
        , L.read "i"
        , L.while ("i" >: 0) $ mconcat
            [ "c" := "b"
            , "b" := "a" +: "b"
            , "a" := "c"
            , "i" := "i" -: 1
            ]
        , L.write "a"
        ]
    fibs :: [Value]
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
    fib :: Small Value -> Value
    fib = (fibs !!) . fromIntegral . getSmall

gcdTest :: ExecWay L.Program -> Property
gcdTest = L.toProgram sample ~*~ gcd'
  where
    sample = mconcat
        [ L.read "a"
        , L.read "b"
        , L.while ("b" >: 0) $ mconcat
            [ "r" := "a" %: "b"
            , "a" := "b"
            , "b" := "r"
            ]
        , L.write "a"
        ]
    gcd' :: NonNegative Value -> NonNegative Value -> Value
    gcd' (NonNegative a) (NonNegative b) = gcd a b

minTest :: ExecWay L.Program -> Property
minTest = L.toProgram sample ~*~ min @Value
  where
    sample = mconcat
        [ L.read "a"
        , L.read "b"
        , If ("a" <: "b")
            ("c" := "a")
            ("c" := "b")
        , L.write "c"
        ]

fileTest :: ExecWay L.Program -> Either Text FullTestData -> Property
fileTest _ (Left err) =
    counterexample ("Parse failed: " ++ toString err) False
fileTest way (Right FullTestData{..}) =
    ftdProgram & ftdInput >-*-> TestRes ftdOutput $ way
