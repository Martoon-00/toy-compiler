{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Test.Util
    ( running
    , TestInOut (..)
    , (~~)
    , usingValue
    ) where

import           Control.Lens         ((^?), _Right)
import qualified Data.Map             as M
import           Test.QuickCheck      (NonNegative (..), Property, property, (===))

import           Toy.Data             (Value)
import           Toy.Lang.Data        (ExecState (..), Stmt, getIO)
import           Toy.Lang.Interpreter (execute)

type In = [Value]
type Out = [Value]
type InOut = ([Value], [Value])

class Executable e where
    exec :: e -> In -> Maybe InOut

instance Executable Stmt where
    exec stmt is = getIO <$> execute (ExecState is [] M.empty stmt) ^? _Right

data TestInOut
    = In :==> Out
    -- ^ execution produced given output for given input
    | In :==% ()
    -- ^ execution failed for given input

infix 1 :==>
infix 1 :==%

running :: Executable e => e -> TestInOut -> Property
running prog (is :==> os) = exec prog is === Just ([], os)
running prog (is :==% _ ) = exec prog is === Nothing


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
(~~) :: (Equivalence f, Executable e) => f -> e -> Property
f ~~ prog = equivalent f (fmap getRes . exec prog) []
  where
    getRes ([] , [x]) = x
    getRes (_:_, _  ) = error "Non empty input remained!"
    getRes (_,   xs)  = error $ "Non single value in output!: "
                                ++ show (reverse xs)

usingValue :: (Value -> f) -> (Value -> f)
usingValue = id
