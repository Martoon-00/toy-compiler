module Toy.Exp.Util
    ( arithspoon
    , bool
    , asToBool
    , binResToBool
    ) where

import           Control.DeepSeq   (NFData)
import           Control.Exception (ArithException, Handler (..))
import           Control.Lens      (Iso', from, iso, (^.))
import           Control.Spoon     (spoonWithHandles)
import           Data.Maybe        (fromJust)


bool :: Iso' Int Bool
bool = iso (/= 0) fromEnum

asToBool :: (Bool -> Bool -> Bool) -> Int -> Int -> Int
asToBool f a b = f (a ^. bool) (b ^. bool) ^. from bool

binResToBool :: (Int -> Int -> Bool) -> Int -> Int -> Int
binResToBool f a b = f a b ^. from bool

-- | Like `teaspoon`, but for `ArithException` only and reports details
-- in case of error
arithspoon :: NFData a => a -> Either String a
arithspoon = fromJust . spoonWithHandles [Handler handler] . Right
  where
    handler = return . Just . Left . (show :: ArithException -> String)
