{-# LANGUAGE OverloadedLists #-}

module Test.Examples.FunSpec
    ( spec
    ) where

import           Control.Category (id, (.))
import           Control.Lens     ((&))
import           Data.Foldable    (for_)
import           Prelude          hiding (id, (.))
import           Test.Hspec       (Spec, describe, it)
import           Test.QuickCheck  (NonNegative (..), Property, property)
import           Universum        (one)

import           Test.Arbitrary   ()
import           Test.Execution   (describeExecWays, works, (>-*->), (~*~))
import           Test.Util        (VerySmall (..))
import           Toy.Base
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
                it "simple" $
                    property $ recSimpleTest way
                it "fib" $
                    fibTest way
                it "gcd" $
                    gcdTest way
            describe "standart functions presence" $ do
                for_ stdFunExamples $ \args@(show -> name, _) ->
                    it name $ stdFunCallTest args way
            describe "arrays" $ do
                it "array argument" $
                    arrayArgTest way
                it "array return" $
                    arrayReturnTest way

singleFunProg :: [Var] -> [Exp] -> L.Stmt -> L.Program
singleFunProg argNames args body =
    let name = "testfunc"
        decl = one (name, (FunSign name argNames, body))
    in  L.Program decl $ L.funCallS name args

singleRetFunProg :: [Var] -> [Exp] -> L.Stmt -> L.Program
singleRetFunProg argNames args body =
    let name = "testfunc"
        decl = one (name, (FunSign name argNames, body))
    in  L.Program decl $ L.writeS (FunE name args)

singleRecFunProg :: [Var] -> [Exp] -> (Var -> L.Stmt) -> L.Program
singleRecFunProg argNames args body =
    let name = "testfunc"
        decl = one (name, (FunSign name argNames, body name))
    in  L.Program decl $ L.writeS (FunE name args)

noActionTest :: ExecWay L.Program -> Property
noActionTest = sample & [] >-*-> []
  where
    sample = singleFunProg [] [] mempty

withBodyTest :: ExecWay L.Program -> Property
withBodyTest = sample & [] >-*-> [1]
  where
    sample = singleFunProg [] [] $ L.writeS 1

singleArgumentTest :: ExecWay L.Program -> Property
singleArgumentTest = sample & [] >-*-> [5]
  where
    sample = singleFunProg ["a"] [5] $ L.writeS "a"

argumentsOrderTest :: ExecWay L.Program -> Property
argumentsOrderTest = sample & [] >-*-> [1]
  where
    sample = singleFunProg ["a", "b"] (ValueE <$> [1, 0]) $
             L.writeS ("a" - "b")

multipleArgumentsTest :: ExecWay L.Program -> Property
multipleArgumentsTest = sample & [] >-*-> [22020]
  where
    input  = ValueE . (10 ^) <$> [4 :: Int, 3..0]
    sample = singleFunProg ["a", "b", "c", "d", "e"] input $
             L.writeS ("a" + "b" - "c" + "d" - "e" + 11111)

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
recSimpleTest way = sample ~*~ fun $ way
  where
    sample = singleRecFunProg ["a"] [readE] $ \funName ->
             L.If ("a" ==: 0) (L.Return 1) $
                L.Return (3 * FunE funName ["a" - 1] * 2)
    fun :: NonNegative (VerySmall Value) -> Value
    fun (NonNegative (VerySmall x)) = 6 ^ x

fibTest :: ExecWay L.Program -> Property
fibTest = sample ~*~ fun
  where
    sample = singleRecFunProg ["a"] [readE] $ \funName ->
             L.If ("a" <: 2) (L.Return "a") $
                L.Return (FunE funName ["a" - 1] + FunE funName ["a" - 2])
    fun :: NonNegative (VerySmall Value) -> Value
    fun (NonNegative (VerySmall x)) = fibs !! fromIntegral x
    fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

gcdTest :: ExecWay L.Program -> Property
gcdTest = sample ~*~ fun
  where
    sample = singleRecFunProg ["a", "b"] [readE, readE] $ \funName ->
             L.If ("b" ==: 0) (L.Return "a") $
                L.Return (FunE funName ["b", "a" %: "b"])
    fun :: NonNegative Value -> NonNegative Value -> Value
    fun (NonNegative x) (NonNegative y) = gcd x y

arrayArgTest :: ExecWay L.Program -> Property
arrayArgTest = sample & [] >-*-> [5]
  where
    fun = ("lol", (FunSign "lol" ["x"], L.writeS $ ArrayAccessE "x" 0))
    sample =
        L.Program [fun] $ mconcat
            [ "a" `L.arrayVarS` [5]
            , L.funCallS "lol" ["a"]
            ]

arrayReturnTest :: ExecWay L.Program -> Property
arrayReturnTest = sample & [] >-*-> [7]
  where
    fun =  ("lol", (FunSign "lol" [], L.Return `L.arrayS` [7]))
    sample =
        L.Program [fun] $ mconcat
            [ L.writeS $ FunE "lol" [] `ArrayAccessE` 0
            ]

stdFunCallTest :: (Var, [Exp]) -> ExecWay L.Program -> Property
stdFunCallTest = works . L.Program mempty . uncurry L.funCallS

