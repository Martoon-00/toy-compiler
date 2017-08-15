{-# LANGUAGE OverloadedLists #-}

module Test.Examples.GNonlocalJmpSpec
    ( spec
    ) where

import           Control.Category ((.))
import           Control.Lens     ((&))
import           Prelude          hiding (id, (.))
import           Test.Hspec       (Spec, describe, it)
import           Test.QuickCheck  (Property)
import           Universum        hiding ((.))

import           Test.Arbitrary   ()
import           Test.Execution   (TestRes (X), describeExecWays, transFails, (>-*->))
import           Toy.Base
import           Toy.Execution    (ExecWay (..), defCompileX86, translateLang)
import           Toy.Exp
import qualified Toy.Lang         as L


spec :: Spec
spec = do
    let ways =
            [ -- Ex id
              Ex translateLang
            , Ex $ defCompileX86 . translateLang
            ]
    describe "nonlocal jumps" $ do
        describeExecWays ways $ \way -> do
            it "local jump forward" $
                localForwardTest way
            it "local jump backward" $
                localBackwardTest way
            it "outside function forward" $
                nonlocalForwardTest way
            it "outside function backward" $
                nonlocalBackwardTest way
            it "outside rec function" $
                nonlocalRecTest way

            describe "errorneous scenarios" $ do
                it "jump to non-label" $
                  nonlabelTest way
                it "duplicated local label" $
                  duplicatedLocalLabelTest way
                it "duplicated nonlocal label" $
                  duplicatedNonlocalLabelTest way
                it "jump to unavailable label" $
                  unavailableLabelTest way

function :: Var -> [Var] -> L.Stmt -> (Var, L.FunDecl)
function name argNames body = (name, (FunSign name argNames, body))

localForwardTest :: ExecWay L.Program -> Property
localForwardTest = sample & [] >-*-> [2]
  where
    fun = function "lol" [] $ mconcat
        [ L.gotoS "l"
        , L.writeS 1
        , L.Label "l"
        , L.writeS 2
        ]
    sample =
        L.Program [fun] $ L.funCallS "lol" []

localBackwardTest :: ExecWay L.Program -> Property
localBackwardTest = sample & [] >-*-> [3]
  where
    fun = function "lol" [] $ mconcat
        [ "i" L.:= 0
        , L.Label "l"
        , L.If ("i" <: 3)
              (("i" L.:= "i" + 1) <> L.gotoS "l")
              L.Skip
        , L.writeS "i"
        ]
    sample =
        L.Program [fun] $ L.funCallS "lol" []

nonlocalForwardTest :: ExecWay L.Program -> Property
nonlocalForwardTest = sample & [] >-*-> [1, 4]
  where
    fun = function "lol" [] $ mconcat
        [ L.writeS 1
        , L.gotoS "l"
        , L.writeS 2
        ]
    sample =
        L.Program [fun] $ mconcat
            [ L.funCallS "lol" []
            , L.writeS 3
            , L.Label "l"
            , L.writeS 4
            ]

nonlocalBackwardTest :: ExecWay L.Program -> Property
nonlocalBackwardTest = sample & [] >-*-> [0, 1, 0, 1, 0, 3]
  where
    fun = function "lol" [] $ mconcat
        [ L.writeS 1
        , L.gotoS "l"
        , L.writeS 2
        ]
    sample =
        L.Program [fun] $ mconcat
            [ "i" L.:= 0
            , L.Label "l"
            , L.writeS 0
            , "i" L.:= "i" + 1
            , L.If ("i" <: 3)
                  (L.funCallS "lol" [])
                  L.Skip
            , L.writeS 3
            ]

nonlocalRecTest :: ExecWay L.Program -> Property
nonlocalRecTest = sample & [] >-*-> [13, 8, 3, 2000]
  where
    fun = function "lol" ["a"] $ mconcat
        [ L.If ("a" <: 0)
            (L.gotoS "l")
            L.Skip
        , L.writeS "a"
        , L.funCallS "lol" ["a" - 5]
        ]
    sample =
        L.Program [fun] $ mconcat
            [ L.funCallS "lol" [13]
            , L.writeS 1000
            , L.Label "l"
            , L.writeS 2000
            ]

nonlabelTest :: ExecWay L.Program -> Property
nonlabelTest = transFails sample
  where
    fun = function "lol" [] $ mconcat
        [ L.gotoS "k"
        , L.Label "l"
        ]
    sample =
        L.Program [fun] $ L.funCallS "lol" []

duplicatedLocalLabelTest :: ExecWay L.Program -> Property
duplicatedLocalLabelTest = transFails sample
  where
    sample = L.Program [] $ L.Label "l" <> L.Label "l"

duplicatedNonlocalLabelTest :: ExecWay L.Program -> Property
duplicatedNonlocalLabelTest = transFails sample
  where
    fun = function "lol" [] $ L.Label "l"
    sample =
        L.Program [fun] $ L.Label "l"


unavailableLabelTest :: ExecWay L.Program -> Property
unavailableLabelTest = sample & [] >-*-> X
  where
    fun1 = function "lol" [] $ L.gotoS "l"
    fun2 = function "mem" [] $ L.Label "l"
    sample =
        L.Program [fun1, fun2] $ L.funCallS "lol" []

