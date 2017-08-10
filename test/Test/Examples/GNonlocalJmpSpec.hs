{-# LANGUAGE OverloadedLists #-}

module Test.Examples.GNonlocalJmpSpec
    ( spec
    ) where

import           Control.Category ((.))
import           Control.Lens     ((&))
import           Prelude          hiding (id, (.))
import           Test.Hspec       (Spec, describe, it)
import           Test.QuickCheck  (Property)

import           Test.Arbitrary   ()
import           Test.Execution   (describeExecWays, (>-*->))
import           Toy.Base
import           Toy.Execution    (ExecWay (..), defCompileX86, translateLang)
import qualified Toy.Lang         as L


spec :: Spec
spec = do
    let ways =
            [ -- Ex id
              Ex translateLang
            -- , Ex $ defCompileX86 . translateLang
            ]
    describe "nonlocal jumps" $ do
        describeExecWays ways $ \way -> do
            it "inside function" $
                insideFunctionTest way

function :: Var -> [Var] -> L.Stmt -> (Var, L.FunDecl)
function name argNames body = (name, (FunSign name argNames, body))

insideFunctionTest :: ExecWay L.Program -> Property
insideFunctionTest = sample & [] >-*-> [2]
  where
    fun = function "lol" [] $ mconcat
        [ L.Goto 5
        , L.writeS 1
        , L.Label 5
        , L.writeS 2
        ]
    sample =
        L.Program [fun] $ L.funCallS "lol" []
