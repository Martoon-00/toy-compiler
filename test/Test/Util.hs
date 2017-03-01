{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

module Test.Util
    ( TestRes (..)
    , (>-->)
    , (~~)
    ) where

import           Control.Lens         ((^?), _Right)
import qualified Data.Map             as M
import           GHC.Exts             (IsList (..))
import           Test.QuickCheck      (NonNegative (..), Property, property, (===))

import           Toy.Data             (Value)
import           Toy.Lang.Data        (ExecState (..), Stmt, getIO)
import           Toy.Lang.Interpreter (execute)

type In = [Value]
type Out = [Value]
type InOut = ([Value], [Value])

class Interpretable e where
    exec :: e -> In -> Maybe InOut

instance Interpretable Stmt where
    exec stmt is = getIO <$> execute (ExecState is [] M.empty stmt) ^? _Right

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

class Equivalence f where
    equivalent :: f -> ([Value] -> Maybe Value) -> [Value] -> Property

instance Equivalence Value where
    equivalent r f0 args = Just r === f0 (reverse args)

instance Equivalence f => Equivalence (Value -> f) where
    equivalent f f0 args =
        property $ \(NonNegative arg) -> equivalent (f arg) f0 (arg:args)

-- | Interprets given program in our language as a function, and
-- checks that it's equivalent to another function.
-- Program have to print a single value.
(~~) :: (Equivalence f, Interpretable e) => f -> e -> Property
f ~~ prog = equivalent f (fmap getRes . exec prog) []
  where
    getRes ([] , [x]) = x
    getRes (_:_, _  ) = error "Non empty input remained!"
    getRes (_,   xs)  = error $ "Non single value in output!: "
                                ++ show (reverse xs)
