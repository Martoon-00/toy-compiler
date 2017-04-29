{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE TemplateHaskell            #-}

module Toy.X86.SymStack
    ( symStack
    , atSymStack

    , SymStackSize
    , SymStackHolder
    , runSymStackHolder
    , allocSymStackOp
    , popSymStackOp
    ) where

import           Control.Lens        (ix, makeLenses, (%=), (<&>), (<-=), (<<+=), (^?))
import           Control.Monad.State (State, runState)
import           Data.Default        (Default (..))
import qualified Data.Vector         as V

import           Toy.X86.Data        (Operand (..))

symStack :: V.Vector Operand
symStack = Reg <$> ["ecx", "ebx", "esi", "edi"]

atSymStack :: Int -> Operand
atSymStack k =
    case symStack ^? ix k of
        Just x  -> x
        Nothing -> Stack (k - V.length symStack)

data SymStackState = SymStackState
    { _symSize    :: Int
    , _symMaxSize :: Int
    }
makeLenses ''SymStackState

instance Default SymStackState where
    def = SymStackState 0 0

newtype SymStackHolder a = SymStackHolder (State SymStackState a)
    deriving (Functor, Applicative, Monad)

newtype SymStackSize = SymStackSize Int
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

runSymStackHolder :: SymStackHolder a -> (SymStackSize, a)
runSymStackHolder (SymStackHolder a) =
    case runState a def of
        (res, SymStackState 0 totalSize) -> (SymStackSize totalSize, res)
        (_  , SymStackState k _        ) ->
            error $ "Wrong symbolic stack size at the end: " ++ show k

allocSymStackOp :: SymStackHolder Operand
allocSymStackOp = SymStackHolder $ do
    pos <- symSize <<+= 1
    symMaxSize %= max (pos + 1)
    return $ atSymStack pos

popSymStackOp :: SymStackHolder Operand
popSymStackOp = SymStackHolder $ (symSize <-= 1) <&> atSymStack
