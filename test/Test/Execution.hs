{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Test.Execution
    ( TestRes (..)
    , (>-->)
    , (>-*->)
    , (~~)
    , (~*~)

    , describeExecWays
    ) where

import           Control.Lens               ((^?), _Right)
import           Control.Monad              (forM_)
import           Control.Monad.Trans.Either (EitherT (..))
import           Control.Monad.Writer       (runWriter)
import           Control.Spoon              (teaspoon)
import qualified Formatting                 as F
import           GHC.Exts                   (IsList (..))
import           Test.Hspec.Core.Spec       (SpecWith, describe)
import           Test.QuickCheck            (Arbitrary, Property, counterexample,
                                             ioProperty, once, property, within, (===))
import           Test.QuickCheck.Property   (failed, reason)

import           Test.Util                  (Extract (..))
import           Toy.Execution              (ExecWay (..), Executable (..), In, InOut,
                                             Meta, Out, translatingIn, withEmptyInput)
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

withTimeout :: Property -> Property
withTimeout = within 10000000

infix 5 >-->
(>-->) :: Executable e => In -> TestRes -> e -> Property
(input >--> res) prog = withTimeout . ioProperty $ do
    outcome <- runEitherT $ exec prog input
    return $ outcome ^? _Right === (withEmptyInput <$> expected res)
  where
    expected (TestRes out) = Just out
    expected X             = Nothing

infix 5 >-*->
(>-*->) :: In -> TestRes -> l -> ExecWay l -> Property
(input >-*-> res) prog way =
    once $ propTranslating way prog $ \executable ->
        withTimeout . ioProperty $ do
            outcome <- runEitherT $ exec executable input
            return $ outcome `assess` (withEmptyInput <$> expected res)
  where
    expected (TestRes out) = Just out
    expected X             = Nothing

class Equivalence f where
    equivalent :: f -> ([Value] -> EitherT String IO Value) -> [Value] -> Property

instance Equivalence Value where
    equivalent r f0 args = withTimeout . ioProperty $ do
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
(~~) :: (Executable e, Equivalence f) => e -> f -> Property
prog ~~ f = equivalent f (fmap singleOutput . exec prog) []

-- | Executes given program in our language as a function in given way, and
-- checks that it's equivalent to another function.
-- Program have to print a single value.
infix 3 ~*~
(~*~) :: Equivalence f => l -> f -> ExecWay l -> Property
(prog ~*~ f) way =
    propTranslating way prog $ \executable ->
    equivalent f (fmap singleOutput . exec executable) []


-- * Execution utils

metaCounterexample :: [Meta] -> Property -> Property
metaCounterexample = flip . foldr $ counterexample . F.formatToString F.build

describeExecWays :: [ExecWay l] -> (ExecWay l -> SpecWith a) -> SpecWith a
describeExecWays ways specs = forM_ ways $ describe <$> show <*> specs

propTranslating
    :: ExecWay l
    -> l
    -> (forall e . Executable e => e -> Property)
    -> Property
propTranslating (Ex way) prog testExec =
    let (eExec, metas) = runWriter . runEitherT $ translatingIn way prog
    in  metaCounterexample metas $
        case eExec of
            Left err -> property
                        failed { reason = "Translation failed: " ++ err }
            Right e  -> testExec e
