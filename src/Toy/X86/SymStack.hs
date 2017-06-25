{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE TemplateHaskell            #-}

-- | This module contains operations related to symbolic stack.

module Toy.X86.SymStack
    ( regSymStack
    , atSymStack

    , SymStackSpace
    , SymStackHolder
    , runSymStackHolder
    , symStackSize
    , allocSymStackOp
    , popSymStackOp
    , peekSymStackOp
    , occupiedRegs
    , accountHardMem
    ) where

import           Control.Lens         (ix, makeLenses, (%=), (<-=), (<<+=))
import           Control.Monad.Fix
import           Control.Monad.State  (StateT, runStateT)
import           Control.Monad.Writer (MonadWriter)
import           Data.Default         (Default (..))
import qualified Data.Vector          as V
import           Universum

import           Toy.X86.Data         (Operand (..))


regSymStack :: V.Vector Operand
regSymStack = Reg <$> ["ecx", "ebx", "esi", "edi"]

atSymStack :: Int -> Operand
atSymStack k =
    case regSymStack ^? ix k of
        Just x  -> x
        Nothing -> Stack (k - V.length regSymStack)

data SymStackState = SymStackState
    { _symSize    :: Int
    , _symMaxSize :: Int
    }
makeLenses ''SymStackState

instance Default SymStackState where
    def = SymStackState 0 0

newtype SymStackHolder m a = SymStackHolder (StateT SymStackState m a)
    deriving (Functor, Applicative, Monad, MonadWriter __, MonadFix)

-- | How much space sym stack requires on real stack.
newtype SymStackSpace = SymStackSpace Int
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

runSymStackHolder :: Monad m => SymStackHolder m a -> m ((SymStackSpace, Int), a)
runSymStackHolder (SymStackHolder a) = do
    (res, SymStackState k totalSize) <- runStateT a def
    let space = max 0 $ totalSize - V.length regSymStack
    return ((SymStackSpace space, k), res)

updateSymMaxSize :: Monad m => Int -> SymStackHolder m ()
updateSymMaxSize k = SymStackHolder $ symMaxSize %= max k

symStackSize :: Monad m => SymStackHolder m Int
symStackSize = SymStackHolder $ use symSize

allocSymStackOp :: Monad m => SymStackHolder m Operand
allocSymStackOp = do
    pos <- SymStackHolder $ symSize <<+= 1
    updateSymMaxSize (pos + 1)
    return $ atSymStack pos

popSymStackOp :: Monad m => SymStackHolder m Operand
popSymStackOp = SymStackHolder $ (symSize <-= 1) <&> atSymStack

peekSymStackOp :: Monad m => SymStackHolder m Operand
peekSymStackOp = SymStackHolder $ use symSize <&> atSymStack . pred

occupiedRegs :: Monad m => SymStackHolder m [Operand]
occupiedRegs = symStackSize <&> flip take (V.toList regSymStack)


-- | @HardMem@s occupy same space as sym stack
accountHardMem :: Monad m => Int -> SymStackHolder m ()
accountHardMem = updateSymMaxSize . (+ V.length regSymStack)  -- TODO: dirty hack
