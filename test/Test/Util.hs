{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeFamilies              #-}

module Test.Util
    ( TestRes (..)
    , (>-->)
    , (~~)
    , alsoSM
    ) where

import           Control.Lens         ((^?), _Right)
import qualified Data.Map             as M
import           GHC.Exts             (IsList (..))
import           Test.QuickCheck      (NonNegative (..), Property, property, (===))

import           Toy.Data             (Value)
import qualified Toy.Lang.Data        as L
import qualified Toy.Lang.Interpreter as L
import qualified Toy.Lang.Translator  as L
import qualified Toy.SM.Data          as SM
import qualified Toy.SM.Interpreter   as SM

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

instance (Interpretable e1, Interpretable e2) => Interpretable (e1, e2) where
    exec (e1, e2) input =
        let r1 = exec e1 input
            r2 = exec e2 input
        in  if r1 /= r2
            then error $ "Got different results: " ++
                         show r1 ++ " != " ++ show r2
            else r1

alsoSM :: L.Stmt -> (L.Stmt, SM.Insts)
alsoSM = (,) <$> id <*> L.toIntermediate


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
