-- | Error utils

module Toy.Util.Error
    ( mapError
    ) where

import           Control.Monad.Error.Class (MonadError (..))
import           Universum

mapError :: MonadError e m => (e -> e) -> m a -> m a
mapError mapErr = (`catchError` throwError . mapErr)
