{-# LANGUAGE OverloadedLists #-}

module Test.Examples.JStringSpec
    ( spec
    ) where

import           Control.Lens          (ix, (^?!))
import           GHC.Exts              (fromList)
import           Test.Hspec            (Spec, describe, it)
import           Test.QuickCheck       (NonNegative (..), Property, Small (..), choose,
                                        forAll, property, (==>))
import           Universum

import           Test.Arbitrary        ()
import           Test.Execution        (describeExecWays, (>-*->))
import           Test.Hspec.QuickCheck (modifyMaxSuccess)
import           Toy.Execution         (ExecWay (..), Executable, TranslateToSM,
                                        defCompileX86, transShow, translateLang, (<~~>))
import           Toy.Exp
import           Toy.Lang              (Stmt (..))
import qualified Toy.Lang              as L

spec :: Spec
spec = do
    let ways :: forall l. (Executable l, Show l, TranslateToSM l) => [ExecWay l]
        ways =
            [ Ex transShow
            , Ex translateLang
            , Ex $ translateLang <~~> defCompileX86
            ]
    describeExecWays ways $ \way -> do

      describe "stdlib" . modifyMaxSuccess (`div` 10) $ do
        it "strlen" $
            property $ strlenTest way
        it "strget" $
            property $ strgetTest way
        it "strset" $
            property $ strsetTest way
        it "strsub" $
            property $ strsubTest way
        it "strcmp" $
            property $ strcmpTest way
        it "strcat" $
            property $ strcatTest way
        it "strmake" $
            property $ strmakeTest way

strlenTest :: ExecWay Stmt -> String -> Property
strlenTest way string =
    sample & [] >-*-> [fromIntegral (length string)] $ way
  where
    sample = L.stringS (\s -> L.writeS $ FunE "strlen" [s]) string

strgetTest :: ExecWay Stmt -> String -> Property
strgetTest way s =
    not (null s) ==>
    forAll (choose (0, length s - 1)) $
        \i ->
    let sample = L.stringS (\se -> L.writeS $ FunE "strget" [se, fromIntegral i]) s

    in sample & [] >-*-> [charE $ s ^?! ix i] $ way

strsetTest :: ExecWay Stmt -> String -> Char -> Property
strsetTest way s c =
    not (null s) ==>
    forAll (choose (0, length s - 1)) $
        \i ->
    let expected = fromList $ map charE (s & ix i .~ c)
        sample = flip L.stringS s $ \se -> mconcat
            [ L.funCallS "strset" [se, fromIntegral i, charE c]
            , L.funCallS "printList" [se]
            ]
    in sample & [] >-*-> expected $ way

strsubTest :: ExecWay Stmt -> String -> Property
strsubTest way s =
    not (null s) ==>
    forAll (choose (0, length s - 1)) $
        \i ->
    forAll (choose (0, length s - 1 - i)) $
        \k ->
    let expected = fromList $ map charE $ take k $ drop i s
        sample = mconcat
            [ L.storeStringS "s" s
            , "s0" := FunE "strsub" ["s", fromIntegral i, fromIntegral k]
            , L.funCallS "printList" ["s0"]
            ]
    in sample & [] >-*-> expected $ way

strcmpTest :: ExecWay Stmt -> String -> String -> Property
strcmpTest way s1 s2 =
    let expected = fromIntegral . pred . fromEnum $ s1 `compare` s2
        sample = mconcat
            [ L.storeStringS "s1" s1
            , L.storeStringS "s2" s2
            , L.writeS $ FunE "strcmp" ["s1", "s2"]
            ]
    in sample & [] >-*-> [expected] $ way

strcatTest :: ExecWay Stmt -> String -> String -> Property
strcatTest way s1 s2 =
    let expected = fromList $ map charE (s1 <> s2)
        sample = mconcat
            [ L.storeStringS "s1" s1
            , L.storeStringS "s2" s2
            , L.funCallS "printList" [FunE "strcat" ["s1", "s2"]]
            ]
    in sample & [] >-*-> expected $ way

strmakeTest :: ExecWay Stmt -> NonNegative (Small Int) -> Char -> Property
strmakeTest way (NonNegative (Small n)) c =
    let expected = fromList $ replicate n (charE c)
        sample =
            L.funCallS "printList" [FunE "strmake" [fromIntegral n, charE c]]
    in sample & [] >-*-> expected $ way

