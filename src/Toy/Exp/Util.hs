module Toy.Exp.Util
    ( arithspoon
    , boolL
    , asToBool
    , binResToBool
    ) where

import           Control.DeepSeq           (NFData)
import           Control.Exception         (ArithException, Handler (..))
import           Control.Lens              (Iso', from, iso)
import           Control.Monad.Error.Class (MonadError (..))
import           Control.Spoon             (spoonWithHandles)
import           Data.Maybe                (fromJust)
import           Data.String               (IsString (..))
import           Universum

import           Toy.Base                  (Value)


boolL :: Iso' Value Bool
boolL = iso (/= 0) (fromIntegral . fromEnum)

asToBool :: (Bool -> Bool -> Bool) -> Value -> Value -> Value
asToBool f a b = f (a ^. boolL) (b ^. boolL) ^. from boolL

binResToBool :: (Value -> Value -> Bool) -> Value -> Value -> Value
binResToBool f a b = f a b ^. from boolL

-- | Like `teaspoon`, but for `ArithException` only and reports details
-- in case of error
arithspoon :: (MonadError e m, IsString e, NFData a) => a -> m a
arithspoon = either (throwError . fromString) return
           . fromJust . spoonWithHandles [Handler handler] . Right
  where
    handler = return . Just . Left . show @ArithException
