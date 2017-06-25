-- | Util to work with bit representation of number

module Toy.Util.Bits where

import           Data.Bits (Bits (..), FiniteBits (..))
import           Universum

setHBit :: FiniteBits x => x -> x
setHBit = flip setBit =<< subtract 1 . finiteBitSize

clearHBit :: FiniteBits x => x -> x
clearHBit = flip clearBit =<< subtract 1 . finiteBitSize

setPHBit :: FiniteBits x => x -> x
setPHBit = flip setBit =<< subtract 2 . finiteBitSize

clearPHBit :: FiniteBits x => x -> x
clearPHBit = flip clearBit =<< subtract 2 . finiteBitSize

