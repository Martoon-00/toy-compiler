{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Test.Examples.SMSpec
    ( spec
    ) where

import           Test.Hspec      (Spec, describe, it)
import           Test.QuickCheck (Property, conjoin, property)

import           Test.Arbitrary  ()
import           Test.Execution  (TestRes (..), describeExecWays, (>-->), (~*~))
import           Test.Util       (instsSM)
import           Toy.Execution   (ExecWay (..), asIs, defCompileX86)
import           Toy.Exp
import           Toy.SM          (Inst (..), Insts)


spec :: Spec
spec =
    describe "SM" $ describeExecWays [Ex asIs, Ex defCompileX86] $ \way -> do
        describe "examples" $ do
            it "io simple" $
                ioTest way
            describe "complex" $ do
                return ()

        it "different erroneous scenarios" $
            errorsTest

ioTest :: ExecWay Insts -> Property
ioTest = (+) @Value 2 ~*~ sample
  where
    sample = instsSM
        [ Read
        , Push 2
        , Bin "+"
        , Write
        ]

errorsTest :: Property
errorsTest = property $
    conjoin $ [] >--> X <$>
        [ instsSM [Write]
        , [Read]
        , [Load "I see the spine\nOf the world.."]
        ]
