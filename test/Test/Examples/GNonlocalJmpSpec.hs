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
        [ L.goto "l"
        , L.write 1
        , L.Label "l"
        , L.write 2
        ]
    sample =
        L.Program [fun] $ L.funCall "lol" []

localBackwardTest :: ExecWay L.Program -> Property
localBackwardTest = sample & [] >-*-> [3]
  where
    fun = function "lol" [] $ mconcat
        [ "i" L.:= 0
        , L.Label "l"
        , L.If ("i" <: 3)
              (("i" L.:= "i" + 1) <> L.goto "l")
              L.Skip
        , L.write "i"
        ]
    sample =
        L.Program [fun] $ L.funCall "lol" []

nonlocalForwardTest :: ExecWay L.Program -> Property
nonlocalForwardTest = sample & [] >-*-> [1, 4]
  where
    fun = function "lol" [] $ mconcat
        [ L.write 1
        , L.goto "l"
        , L.write 2
        ]
    sample =
        L.Program [fun] $ mconcat
            [ L.funCall "lol" []
            , L.write 3
            , L.Label "l"
            , L.write 4
            ]

nonlocalBackwardTest :: ExecWay L.Program -> Property
nonlocalBackwardTest = sample & [] >-*-> [0, 1, 0, 1, 0, 3]
  where
    fun = function "lol" [] $ mconcat
        [ L.write 1
        , L.goto "l"
        , L.write 2
        ]
    sample =
        L.Program [fun] $ mconcat
            [ "i" L.:= 0
            , L.Label "l"
            , L.write 0
            , "i" L.:= "i" + 1
            , L.If ("i" <: 3)
                  (L.funCall "lol" [])
                  L.Skip
            , L.write 3
            ]

nonlocalRecTest :: ExecWay L.Program -> Property
nonlocalRecTest = sample & [] >-*-> [13, 8, 3, 2000]
  where
    fun = function "lol" ["a"] $ mconcat
        [ L.If ("a" <: 0)
            (L.goto "l")
            L.Skip
        , L.write "a"
        , L.funCall "lol" ["a" - 5]
        ]
    sample =
        L.Program [fun] $ mconcat
            [ L.funCall "lol" [13]
            , L.write 1000
            , L.Label "l"
            , L.write 2000
            ]

nonlabelTest :: ExecWay L.Program -> Property
nonlabelTest = transFails sample
  where
    fun = function "lol" [] $ mconcat
        [ L.goto "k"
        , L.Label "l"
        ]
    sample =
        L.Program [fun] $ L.funCall "lol" []

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
    fun1 = function "lol" [] $ L.goto "l"
    fun2 = function "mem" [] $ L.Label "l"
    sample =
        L.Program [fun1, fun2] $ L.funCall "lol" []

