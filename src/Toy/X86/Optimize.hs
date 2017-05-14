{-# LANGUAGE TupleSections #-}

-- | The module contains some optimization rules which tend to decrease
-- number of instructions to reach better readability.

module Toy.X86.Optimize
    ( optimize
    ) where

import           Control.Monad (guard)
import           Data.Functor  (($>))
import           GHC.Exts      (fromList, toList)

import           Toy.X86.Data  (Inst (..), Insts, Operand (..))

optimize :: Insts -> Insts
optimize = fromList . optimizeTillCan . toList
  where
    optimizeTillCan :: [Inst] -> [Inst]
    optimizeTillCan insts = maybe insts optimizeTillCan $ tryOptimize insts

    tryOptimize :: [Inst] -> Maybe [Inst]
    tryOptimize insts =
        let insts' = foldr ($) insts rules
        in  guard (insts /= insts') $> insts'

    rules :: [TotalRule]
    rules =
        [ localRule pushPop
        , localRule movRevMov
        , localRule noStackResize
        ]

-- | Some rule which /perhaps/ optimizes given program
type TotalRule = [Inst] -> [Inst]


-- * Local rules

-- | Local rule is rule which looks at some suffix of instructions list
-- and tries to optimize it.
-- This may be more convinient than defining 'TotalRule' from scratch.
type LocalRule = [Inst] -> Maybe [Inst]

localRule
    :: LocalRule -> TotalRule
localRule _          []           = []
localRule tryLocally insts@(i:is) =
    let cont = localRule tryLocally
    in maybe (i : cont is) cont $ tryLocally insts

pushPop :: LocalRule
pushPop insts
    | Push a : Pop b : is <- insts
    , a == b
        = Just is
    | otherwise
        = Nothing

movRevMov :: LocalRule
movRevMov insts
    | Mov a b : Mov c d : is <- insts
    , a == d && b == c
        = Just $ Mov a b : is
    | otherwise
        = Nothing

noStackResize :: LocalRule
noStackResize insts
    | BinOp op (Const 0) (Reg "esp") : is <- insts
    , op == "subl" || op == "addl"
        = Just is
    | otherwise
        = Nothing
