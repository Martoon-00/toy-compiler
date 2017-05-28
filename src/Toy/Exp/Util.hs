{-# LANGUAGE TypeApplications #-}

module Toy.Exp.Util
    ( arithspoon
    , bool
    , asToBool
    , binResToBool
    ) where

import           Control.DeepSeq           (NFData)
import           Control.Exception         (ArithException, Handler (..))
import           Control.Lens              (Iso', from, iso, (^.))
import           Control.Monad.Error.Class (MonadError (..))
import           Control.Spoon             (spoonWithHandles)
import           Data.Maybe                (fromJust)
import           Data.String               (IsString (..))

import           Toy.Base                  (Value)


bool :: Iso' Value Bool
bool = iso (/= 0) (fromIntegral . fromEnum)

asToBool :: (Bool -> Bool -> Bool) -> Value -> Value -> Value
asToBool f a b = f (a ^. bool) (b ^. bool) ^. from bool

binResToBool :: (Value -> Value -> Bool) -> Value -> Value -> Value
binResToBool f a b = f a b ^. from bool

-- | Like `teaspoon`, but for `ArithException` only and reports details
-- in case of error
arithspoon :: (MonadError e m, IsString e, NFData a) => a -> m a
arithspoon = either (throwError . fromString) return
           . fromJust . spoonWithHandles [Handler handler] . Right
  where
    handler = return . Just . Left . show @ArithException
