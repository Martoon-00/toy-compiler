{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

module Test.SM.InterpreterSpec
    ( spec
    ) where

import           Test.Hspec      (Spec, describe, it)
import           Test.QuickCheck (Property, conjoin, property)

import           Test.Arbitrary  ()
import           Test.Execution  (TestRes (..), (>-->), (~~))
import           Test.Util       (instsSM)
import           Toy.Exp
import           Toy.SM          (Inst (..))


spec :: Spec
spec =
    describe "SM" $ describe "interpreter" $ do
        describe "examples" $ do
            it "io simple" $
                ioTest
            it "different erroneous scenarios" $
                errorsTest
            describe "complex" $ do
                return ()

ioTest :: Property
ioTest = (+) @Value 2 ~~ sample
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
        , [Ld "I see the spine\nOf the world.."]
        ]
