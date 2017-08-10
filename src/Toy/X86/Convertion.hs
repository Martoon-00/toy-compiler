-- | Convert nums to and from 31-byte arithmetic

module Toy.X86.Convertion where

import           Universum

import           Toy.Base  (Value)

-- Convertions between 31-bit (with lowest bit = 1) numbers and normal numbers.
-- Normal number: sign  sign  value
--                  1     1     30
-- 31-bit number: sign  value   1
--                  1     30    1
nTo31 :: Value -> Int32
nTo31 x = fromIntegral x * 2 + 1




