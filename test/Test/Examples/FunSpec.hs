{-# LANGUAGE OverloadedLists  #-}
{-# LANGUAGE TypeApplications #-}

module Test.Examples.FunSpec
    ( spec
    ) where

import           Control.Category (id, (.))
import           Control.Lens     ((&))
import           Prelude          hiding (id, (.))
import           Test.Hspec       (Spec, describe, it)
import           Test.QuickCheck  (NonNegative (..), Property, Small (..))
import           Universum        (one)

import           Test.Arbitrary   ()
import           Test.Execution   (describeExecWays, (>-*->), (~*~))
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
    describe "functions" $ do
        describeExecWays ways $ \way -> do
            describe "basic" $ do
                it "no action" $
                    noActionTest way
                it "with body" $
                    withBodyTest way
                it "single argument" $
                    singleArgumentTest way
                it "arguments order" $
                    argumentsOrderTest way
                it "multiple arguments" $
                    multipleArgumentsTest way
                it "return" $
                    returnTest way
                it "return in the middle" $
                    returnInTheMiddleTest way
            describe "recursion" $ do
                return ()
                -- it "simple" $
                    -- recSimpleTest way
                -- it "two arguments" $
                    -- recTwoArgumentsTest way
                -- it "gcd" $
                    -- gcdTest way
                -- it "fib" $
                    -- fibTest way

singleFunProg :: [Var] -> [Exp] -> L.Stmt -> L.Program
singleFunProg argNames args body =
    let name = "testfunc"
        decl = one (name, (FunSign name argNames, body))
    in  L.ProgramG decl $ L.FunCall name args

singleRetFunProg :: [Var] -> [Exp] -> L.Stmt -> L.Program
singleRetFunProg argNames args body =
    let name = "testfunc"
        decl = one (name, (FunSign name argNames, body))
    in  L.ProgramG decl $ L.Write (FunE name args)

singleRecFunProg :: [Var] -> [Exp] -> (Var -> L.Stmt) -> L.Program
singleRecFunProg argNames args body =
    let name = "testfunc"
        decl = one (name, (FunSign name argNames, body name))
    in  L.ProgramG decl $ L.Write (FunE name args)

noActionTest :: ExecWay L.Program -> Property
noActionTest = sample & [] >-*-> []
  where
    sample = singleFunProg [] [] mempty

withBodyTest :: ExecWay L.Program -> Property
withBodyTest = sample & [] >-*-> [1]
  where
    sample = singleFunProg [] [] $ L.Write 1

singleArgumentTest :: ExecWay L.Program -> Property
singleArgumentTest = sample & [] >-*-> [5]
  where
    sample = singleFunProg ["a"] [5] $ L.Write "a"

argumentsOrderTest :: ExecWay L.Program -> Property
argumentsOrderTest = sample & [] >-*-> [1]
  where
    sample = singleFunProg ["a", "b"] (ValueE <$> [1, 0]) $
             L.Write ("a" - "b")

multipleArgumentsTest :: ExecWay L.Program -> Property
multipleArgumentsTest = sample & [] >-*-> [0]
  where
    sample = singleFunProg ["a", "b", "c", "d", "e"] (ValueE <$> [2..6]) $
             L.Write ("a" + "b" - "c" + "d" - "e")

returnTest :: ExecWay L.Program -> Property
returnTest = sample & [] >-*-> [7]
  where
    sample = singleRetFunProg [] [] $ L.Return 7

returnInTheMiddleTest :: ExecWay L.Program -> Property
returnInTheMiddleTest = sample & [] >-*-> [15]
  where
    sample = singleRetFunProg [] [] $ mconcat
        [ "i" L.:= 0
        , L.whileS (0 ==: 0) $ mconcat
            [ L.If ("i" >: 12)
                (L.Return "i")
                (L.Skip)
            , "i" L.:= "i" + 5
            ]
        ]

recSimpleTest :: ExecWay L.Program -> Property
recSimpleTest = sample ~*~ fun
  where
    sample = singleRecFunProg ["a"] [ReadE] $ \funName ->
             L.If ("a" ==: 0) (L.Return 0) $
                L.Return (1 + FunE funName ["a" - 1])
    fun :: NonNegative (Small Value) -> Value
    fun (NonNegative (Small x)) = x
