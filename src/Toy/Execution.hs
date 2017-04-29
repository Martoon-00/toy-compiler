-- | Gathers all interpreting / translation / compilation logic,
-- providing common interface for it.

module Toy.Execution
       ( module M
       ) where

import           Toy.Execution.Data  as M
import           Toy.Execution.Exec  as M
import           Toy.Execution.Trans as M
