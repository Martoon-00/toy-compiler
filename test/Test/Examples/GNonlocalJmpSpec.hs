{-# LANGUAGE OverloadedLists #-}

module Test.Examples.GNonlocalJmpSpec
    ( spec
    ) where

import           Control.Category (id, (.))
import           Control.Lens     ((&))
import           Prelude          hiding (id, (.))
import           Test.Hspec       (Spec, describe, it)
import           Test.QuickCheck  (Property)
import           Universum        hiding ((.))

import           Test.Arbitrary   ()
import           Test.Execution   (TestRes (X), WayRunner (..), describeExecWays,
                                   runFails, transFails, (>-*->))
import           Toy.Execution    (ExecWay (..), defCompileX86, translateLang)
import           Toy.Exp
import qualified Toy.Lang         as L


spec :: Spec
spec = do
    let ways =
            [ (Ex id, WayRunner runFails "execution should fail")
            , (Ex translateLang, WayRunner transFails "tranlation should fail")
            , (Ex $ defCompileX86 . translateLang, WayRunner transFails "tranlation should fail")
            ]
    describe "nonlocal jumps" $ do
        describeExecWays ways $ \(way, failCheck) -> do
            it "local jump forward" $
                localForwardTest way
            it "local jump backward" $
                localBackwardTest way
            it "outside L.toFunDecls forward" $
                nonlocalForwardTest way
            it "outside L.toFunDecls backward" $
                nonlocalBackwardTest way
            it "outside rec L.toFunDecls" $
                nonlocalRecTest way

            describe "errorneous scenarios" $ do
                it "jump to non-label" $
                  nonlabelTest failCheck way
                it "duplicated local label" $
                  duplicatedLocalLabelTest failCheck way
                it "duplicated nonlocal label" $
                  duplicatedNonlocalLabelTest failCheck way
                it "jump to unavailable label" $
                  unavailableLabelTest way

localForwardTest :: ExecWay L.Program -> Property
localForwardTest = sample & [] >-*-> [2]
  where
    fun = L.toFunDecls "lol" [] $ mconcat
        [ L.goto "l"
        , L.write 1
        , L.Label "l"
        , L.write 2
        ]
    sample =
        L.mkProgram fun $ L.funCall "lol" []

localBackwardTest :: ExecWay L.Program -> Property
localBackwardTest = sample & [] >-*-> [3]
  where
    fun = L.toFunDecls "lol" [] $ mconcat
        [ "i" L.:= 0
        , L.Label "l"
        , L.If ("i" <: 3)
              (("i" L.:= "i" + 1) <> L.goto "l")
              L.Skip
        , L.write "i"
        ]
    sample =
        L.mkProgram fun $ L.funCall "lol" []

nonlocalForwardTest :: ExecWay L.Program -> Property
nonlocalForwardTest = sample & [] >-*-> [1, 4]
  where
    fun = L.toFunDecls "lol" [] $ mconcat
        [ L.write 1
        , L.goto "l"
        , L.write 2
        ]
    sample =
        L.mkProgram fun $ mconcat
            [ L.funCall "lol" []
            , L.write 3
            , L.Label "l"
            , L.write 4
            ]

nonlocalBackwardTest :: ExecWay L.Program -> Property
nonlocalBackwardTest = sample & [] >-*-> [100, 1, 101, 1, 102, 3]
  where
    fun = L.toFunDecls "lol" [] $ mconcat
        [ L.write 1
        , L.goto "l"
        , L.write 2
        ]
    sample =
        L.mkProgram fun $ mconcat
            [ "i" L.:= 0
            , L.Label "l"
            , L.write ("i" + 100)
            , "i" L.:= "i" + 1
            , L.If ("i" <: 3)
                  (L.funCall "lol" [])
                  L.Skip
            , L.write 3
            ]

nonlocalRecTest :: ExecWay L.Program -> Property
nonlocalRecTest = sample & [] >-*-> [13, 8, 3, 2000]
  where
    fun = L.toFunDecls "lol" ["a"] $ mconcat
        [ L.If ("a" <: 0)
            (L.goto "l")
            L.Skip
        , L.write "a"
        , L.funCall "lol" ["a" - 5]
        ]
    sample =
        L.mkProgram fun $ mconcat
            [ L.funCall "lol" [13]
            , L.write 1000
            , L.Label "l"
            , L.write 2000
            ]

nonlabelTest :: WayRunner L.Program -> ExecWay L.Program -> Property
nonlabelTest failCheck = runWayRunner failCheck sample
  where
    fun = L.toFunDecls "lol" [] $ mconcat
        [ L.goto "k"
        , L.Label "l"
        ]
    sample =
        L.mkProgram fun $ L.funCall "lol" []

duplicatedLocalLabelTest :: WayRunner L.Program -> ExecWay L.Program -> Property
duplicatedLocalLabelTest failCheck = runWayRunner failCheck sample
  where
    sample = L.toProgram $ L.Label "l" <> L.Label "l"

duplicatedNonlocalLabelTest :: WayRunner L.Program -> ExecWay L.Program -> Property
duplicatedNonlocalLabelTest failCheck = runWayRunner failCheck sample
  where
    fun = L.toFunDecls "lol" [] $ L.Label "l"
    sample =
        L.mkProgram fun $ L.Label "l"


unavailableLabelTest :: ExecWay L.Program -> Property
unavailableLabelTest = sample & [] >-*-> X
  where
    fun1 = L.toFunDecls "lol" [] $ L.goto "l"
    fun2 = L.toFunDecls "mem" [] $ L.Label "l"
    sample =
        L.mkProgram (fun1 <> fun2) $ L.funCall "lol" []

