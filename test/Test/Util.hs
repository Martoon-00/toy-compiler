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
    = In :~~> Out
    | In :~~% ()

infix 1 :~~>
infix 1 :~~%

running :: Executable e => e -> TestInOut -> Property
running prog (is :~~> os) = exec prog is === Just ([], os)
running prog (is :~~% _ ) = exec prog is === Nothing


class Equivalence f where
    equivalent :: f -> ([Value] -> Maybe Value) -> [Value] -> Property

instance Equivalence Value where
    equivalent r f0 args = Just r === f0 (reverse args)

instance Equivalence f => Equivalence (Value -> f) where
    equivalent f f0 args =
        property $ \(NonNegative arg) -> equivalent (f arg) f0 (arg:args)

-- | Interprets given program in our language
-- Program's input is considered to be function's arguments, while program's
-- output should be single value which is
(~~) :: (Equivalence f, Executable e) => f -> e -> Property
f ~~ prog = equivalent f (fmap getRes . exec prog) []
  where
    getRes ([] , [x]) = x
    getRes (_:_, _  ) = error "Non empty input remained!"
    getRes _          = error "Non single value in output!"

usingValue :: (Value -> f) -> (Value -> f)
usingValue = id
