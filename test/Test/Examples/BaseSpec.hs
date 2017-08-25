{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLists           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

module Test.Examples.BaseSpec
    ( spec
    ) where

import           Test.Hspec      (Spec, describe, it)
import           Test.QuickCheck (Large (..), Property, conjoin, counterexample, property)
import           Universum

import           Test.Arbitrary  ()
import           Test.Execution  (TestRes (..), describeExecWays, (>-*->), (>-->), (~*~))
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

            it "while simple" $
                whileTest way

    describeExecWays ways $ \way -> do
      describeDir "./test/cases/exec" $
          fileTest way


noActions :: ExecWay Stmt -> Property
noActions = mempty & [] >-*-> []

ifTrueTest :: ExecWay Stmt -> Property
ifTrueTest = sample & [] >-*-> [0]
  where
    sample = If 1 (L.write 0) (L.write 1)

ifFalseTest :: ExecWay Stmt -> Property
ifFalseTest = sample & [] >-*-> [1]
  where
    sample = If 0 (L.write 0) (L.write 1)

writeTest :: ExecWay Stmt -> Large Value -> Property
writeTest way (Large v) = sample & [] >-*-> [v] $ way
  where
    sample = L.write (ValueE v)

ioTest :: ExecWay Stmt -> Property
ioTest = sample ~*~ getLarge @Value
  where
    sample = mconcat
        [ L.read "a"
        , L.write "a"
        ]

whileTest :: ExecWay Stmt -> Property
whileTest = sample & [] >-*-> [0 .. 4]
  where
    sample = mconcat
        [ "i" := 0
        , L.while ("i" <: 5) $ mconcat
            [ L.write "i"
            , "i" := "i" +: 1
            ]
        ]

errorsTest :: Property
errorsTest = conjoin $
    [ L.write (5 /: 0)
    , L.read "x"
    , L.write "x"
    ] <&> [] >--> X

fileTest :: ExecWay L.Program -> Either Text FullTestData -> Property
fileTest _ (Left err) =
    counterexample ("Parse failed: " ++ toString err) False
fileTest way (Right FullTestData{..}) =
    ftdProgram & ftdInput >-*-> TestRes ftdOutput $ way
