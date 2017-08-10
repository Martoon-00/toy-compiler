-- | Constants, including once provided in environment or as program arguments

module Toy.Constants
    ( runtimePath
    , useGC
    , use31Arith
    ) where

import           System.Environment (lookupEnv)
import           System.IO.Unsafe   (unsafePerformIO)
import           Universum

env :: String -> Maybe String
env = unsafePerformIO . lookupEnv

flag :: String -> Bool
flag = maybe False (const True) . env


-- | Path where runtime library is expected to be
runtimePath :: FilePath
runtimePath = fromMaybe "./runtime" $ env "RC_RUNTIME"
{-# NOINLINE runtimePath #-}

-- | Whether to use GC (and check memory cleaned at the program end).
-- Requires 'use31Arith' flag to be enabled
useGC :: Bool
useGC = not $ flag "DISABLE_GC"
{-# NOINLINE useGC #-}

-- | Whether to use 31-byte arithmetics to keep numbers
use31Arith :: Bool
use31Arith = useGC  -- exactly follows this constant for now
