{-# LANGUAGE TupleSections #-}

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

    localRule
        :: ([Inst] -> Maybe [Inst])  -- ^ rule to apply to suffix
        -> [Inst] -> [Inst]          -- ^ total rule
    localRule _          []           = []
    localRule tryLocally insts@(i:is) =
        let cont = localRule tryLocally
        in maybe (i : cont is) cont $ tryLocally insts

    rules =
        [ localRule pushPop
        , localRule movRevMov
        , localRule noStackResize
        ]

    pushPop insts
        | Push a : Pop b : is <- insts
        , a == b
            = Just is
        | otherwise
            = Nothing

    movRevMov insts
        | Mov a b : Mov c d : is <- insts
        , a == d && b == c
            = Just $ Mov a b : is
        | otherwise
            = Nothing

    noStackResize insts
        | BinOp op (Const 0) (Reg "esp") : is <- insts
        , op == "subl" || op == "addl"
            = Just is
        | otherwise
            = Nothing
