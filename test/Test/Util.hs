{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}

module Test.Util
    ( TestRes (..)
    , ExecWay (..)
    , describeExecWays
    , (>-->)
    , (>-*->)
    , (~~)
    , (~*~)
    , instsSM
    ) where

import           Control.Lens         ((^?), _Right)
import           Control.Monad        (forM_)
import           Control.Spoon        (teaspoon)
import qualified Data.Map             as M
import           GHC.Exts             (IsList (..))
import           Test.Hspec           (describe)
import           Test.Hspec.Core.Spec (SpecWith)
import           Test.QuickCheck      (Arbitrary, NonNegative (..), Property, conjoin,
                                       counterexample, property, (.&&.), (===))

import           Toy.Exp              (Value)
import qualified Toy.Lang             as L
import qualified Toy.SM               as SM

type In = [Value]
type Out = [Value]
type InOut = ([Value], [Value])

class Interpretable e where
    exec :: e -> In -> Maybe InOut

instance Interpretable L.Stmt where
    exec stmt is =
        L.getIO <$> L.execute (L.ExecState is [] M.empty stmt) ^? _Right

instance Interpretable SM.Insts where
    exec insts is =
        SM.getIO <$> SM.execute (SM.ExecState is [] M.empty [] insts 0) ^? _Right


data ExecWay
    = Interpret  -- ^ Interpret language directly
    | Translate  -- ^ Translate to SM and interpret
    deriving (Eq)

instance Show ExecWay where
    show Interpret = "Lang interpreter"
    show Translate = "Translator + SM interpreter"

inExecWay :: ExecWay -> L.Stmt -> In -> Maybe InOut
inExecWay Interpret = exec
inExecWay Translate = exec . L.toIntermediate

describeExecWays :: [ExecWay] -> (ExecWay -> SpecWith a) -> SpecWith a
describeExecWays ways specs =
    forM_ ways $ describe <$> show <*> specs


data TestRes
    = TestRes Out  -- execution produced given output for given input
    | X            -- execution failed for given input

instance IsList TestRes where
    type Item TestRes = Value
    fromList = TestRes
    toList _ = error "toList: impossible for TestRes"

infix 5 >-->

(>-->) :: Interpretable e => In -> TestRes -> e -> Property
(input >--> res) prog = exec prog input === (([], ) <$> expected res)
  where
    expected (TestRes out) = Just out
    expected X             = Nothing

(>-*->) :: In -> TestRes -> L.Stmt -> ExecWay -> Property
(input >-*-> res) prog way =
    inExecWay way prog input === (([], ) <$> expected res)
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
    equivalent :: f -> ([Value] -> Maybe Value) -> [Value] -> Property

instance Equivalence Value where
    equivalent r f0 args =
        let result   = f0 (reverse args)
            expected = teaspoon r
            disp     = maybe "failure" show
        in  counterexample
            ("Expected " ++ disp expected ++ ", got " ++ disp result)
            (expected == result)

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
(~~) :: (Equivalence f, Interpretable e) => f -> e -> Property
f ~~ prog = equivalent f (fmap singleOutput . exec prog) []

-- | Executes given program in our language as a function in given way, and
-- checks that it's equivalent to another function.
-- Program have to print a single value.
infix 3 ~*~
(~*~) :: Equivalence f => f -> L.Stmt -> ExecWay -> Property
(f ~*~ prog) way = equivalent f (fmap singleOutput . inExecWay way prog) []


instsSM :: SM.Insts -> SM.Insts
instsSM = id

instance Monoid Property where
    mempty = property True
    mappend = (.&&.)
    mconcat = conjoin
