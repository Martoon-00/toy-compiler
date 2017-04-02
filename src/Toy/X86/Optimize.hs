{-# LANGUAGE TupleSections #-}

module Toy.X86.Optimize
    ( optimize
    ) where

import           Control.Applicative ((<|>))
import           Data.Monoid         (Any (..))
import           GHC.Exts            (fromList, toList)

import           Toy.X86.Data        (Inst (..), Insts)

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

    rules =
        [ pushPop
        ]

    pushPop (Push a : Pop b : is)
        | a == b    = Just is
        | otherwise = Nothing
    pushPop _       = Nothing
