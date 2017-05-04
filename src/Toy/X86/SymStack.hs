{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLists            #-}
{-# LANGUAGE TemplateHaskell            #-}

module Toy.X86.SymStack
    ( symStack
    , atSymStack

    , SymStackSpace
    , SymStackHolder
    , runSymStackHolder
    , symStackSize
    , allocSymStackOp
    , popSymStackOp
    , wipeSymStackAfterRollout
    ) where

import           Control.Lens        (ix, makeLenses, use, (%=), (<&>), (<-=), (<<+=),
                                      (<<.=), (^?))
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

-- | How much space sym stack requires on real stack.
newtype SymStackSpace = SymStackSpace Int
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral)

runSymStackHolder :: SymStackHolder a -> (SymStackSpace, a)
runSymStackHolder (SymStackHolder a) =
    case runState a def of
        (res, SymStackState 0 totalSize) ->
            (SymStackSpace . max 0 $ totalSize - V.length symStack, res)
        (_  , SymStackState k _        ) ->
            error $ "Wrong symbolic stack size at the end: " ++ show k

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

wipeSymStackAfterRollout :: SymStackHolder ()
wipeSymStackAfterRollout = do
    size <- SymStackHolder $ symSize <<.= 0
    updateSymMaxSize $ min size (length symStack)
