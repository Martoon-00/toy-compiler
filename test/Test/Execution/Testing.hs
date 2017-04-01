{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Test.Execution.Testing
    ( TestRes (..)
    , (>-->)
    , (>-*->)
    , (~~)
    , (~*~)
    ) where

import           Control.Lens               ((^?), _Right)
import           Control.Monad.Trans.Either (EitherT (..))
import           Control.Spoon              (teaspoon)
import           GHC.Exts                   (IsList (..))
import           Test.QuickCheck            (Arbitrary, NonNegative (..), Property,
                                             counterexample, ioProperty, once, property,
                                             (===))

import           Test.Execution.Data        (In, InOut, Out, withEmptyInput)
import           Test.Execution.Exec        (Executable (..))
import           Test.Execution.Trans       (ExecWay (..), propTranslating)
import           Toy.Exp                    (Value)


data TestRes
    = TestRes Out  -- execution produced given output
    | X            -- execution failed

instance IsList TestRes where
    type Item TestRes = Value
    fromList = TestRes
    toList _ = error "toList: impossible for TestRes"

assess :: (Eq a, Show a) => Either String a -> Maybe a -> Property
assess result expected =
    let dispm = maybe "failure" show
        dispe = either ("failure: " ++) show
    in  counterexample
        ("Expected " ++ dispm expected ++ ", got " ++ dispe result)
        (expected == result ^? _Right)

infix 5 >-->
(>-->) :: Executable e => In -> TestRes -> e -> Property
(input >--> res) prog = ioProperty $ do
    outcome <- runEitherT $ exec prog input
    return $ outcome ^? _Right === (withEmptyInput <$> expected res)
  where
    expected (TestRes out) = Just out
    expected X             = Nothing

infix 5 >-*->
(>-*->) :: In -> TestRes -> l -> ExecWay l -> Property
(input >-*-> res) prog way =
    once $ propTranslating way prog $ \executable -> ioProperty $ do
        outcome <- runEitherT $ exec executable input
        return $ outcome `assess` (withEmptyInput <$> expected res)
  where
    expected (TestRes out) = Just out
    expected X             = Nothing

class Extract a p where
    extract :: p -> a

instance Extract a a where
    extract = id

instance Extract a (NonNegative a) where
    extract = getNonNegative

class Equivalence f where
    equivalent :: f -> ([Value] -> EitherT String IO Value) -> [Value] -> Property

instance Equivalence Value where
    equivalent r f0 args = ioProperty $ do
        result <- runEitherT $ f0 (reverse args)
        let expected = teaspoon r
        return $ result `assess` expected

instance (Equivalence f, Extract Value v, Show v, Arbitrary v)
       => Equivalence (v -> f) where
    equivalent f f0 args =
        property $ \arg -> equivalent (f arg) f0 (extract arg : args)


singleOutput :: InOut -> Value
singleOutput ([] , [x]) = x
singleOutput (_:_, _  ) = error "Non empty input remained!"
singleOutput (_,   xs)  = error $ "Non single value in output!: "
                                  ++ show (reverse xs)

-- | Interprets given program in one of our languages as a function, and
-- checks that it's equivalent to another function.
-- Program have to print a single value.
infix 3 ~~
(~~) :: (Equivalence f, Executable e) => f -> e -> Property
f ~~ prog = equivalent f (fmap singleOutput . exec prog) []

-- | Executes given program in our language as a function in given way, and
-- checks that it's equivalent to another function.
-- Program have to print a single value.
infix 3 ~*~
(~*~) :: Equivalence f => f -> l -> ExecWay l -> Property
(f ~*~ prog) way =
    propTranslating way prog $ \executable ->
    equivalent f (fmap singleOutput . exec executable) []
