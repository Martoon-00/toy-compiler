-- | Keeps some general functions, not related to compilers

module Toy.Util
       ( module M
       , (>:)
       ) where

import           Toy.Util.Bits      as M
import           Toy.Util.Error     as M
import           Toy.Util.Instances as M ()
import           Toy.Util.Parsable  as M

-- | Convenient alias for creating tuples.
(>:) :: a -> b -> (a, b)
(>:) = (,)
infixr 0 >:
