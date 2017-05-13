{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE TemplateHaskell            #-}

module Toy.X86.SymStack
    ( regSymStack
    , atSymStack

    , SymStackSpace
    , SymStackHolder
    , runSymStackHolder
    , symStackSize
    , allocSymStackOp
    , popSymStackOp
    , occupiedRegs
    ) where

import           Control.Lens        (ix, makeLenses, use, (%=), (<&>), (<-=), (<<+=),
                                      (^?))
import           Control.Monad.State (State, runState)
import           Data.Default        (Default (..))
import qualified Data.Vector         as V

import           Toy.X86.Data        (Operand (..))

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

newtype SymStackHolder a = SymStackHolder (State SymStackState a)
    deriving (Functor, Applicative, Monad)

-- | How much space sym stack requires on real stack.
newtype SymStackSpace = SymStackSpace Int
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

runSymStackHolder :: SymStackHolder a -> ((SymStackSpace, Int), a)
runSymStackHolder (SymStackHolder a) =
    let (res, SymStackState k totalSize) = runState a def
        space = max 0 $ totalSize - V.length regSymStack
    in ((SymStackSpace space, k), res)

updateSymMaxSize :: Int -> SymStackHolder ()
updateSymMaxSize k = SymStackHolder $ symMaxSize %= max k

symStackSize :: SymStackHolder Int
symStackSize = SymStackHolder $ use symSize

allocSymStackOp :: SymStackHolder Operand
allocSymStackOp = do
    pos <- SymStackHolder $ symSize <<+= 1
    updateSymMaxSize (pos + 1)
    return $ atSymStack pos

popSymStackOp :: SymStackHolder Operand
popSymStackOp = SymStackHolder $ (symSize <-= 1) <&> atSymStack

occupiedRegs :: SymStackHolder [Operand]
occupiedRegs = symStackSize <&> flip take (V.toList regSymStack)
