{-# LANGUAGE TupleSections #-}

module Toy.X86.Optimize
    ( optimize
    ) where

import           Control.Applicative ((<|>))
import           Data.Monoid         (Any (..))
import           GHC.Exts            (fromList, toList)

import           Toy.X86.Data        (Inst (..), Insts, Operand (..))

optimize :: Insts -> Insts
optimize = fromList . optimizeTillCan . toList
  where
    optimizeTillCan :: [Inst] -> [Inst]
    optimizeTillCan insts =
        let (cont, insts') = tryOptimize insts
        in  if getAny cont then optimizeTillCan insts' else insts

    tryOptimize :: [Inst] -> (Any, [Inst])
    tryOptimize []           = return []
    tryOptimize insts@(i:is) =
        let matchedRule = foldr (<|>) Nothing $ ($ insts) <$> rules
        in  maybe ((i:) <$> tryOptimize is) (Any True, ) matchedRule

    -- TODO: local rules
    rules =
        [ pushPop
        , movRevMov
        , noStackResize
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
