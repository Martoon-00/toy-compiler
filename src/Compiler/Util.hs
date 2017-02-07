module Compiler.Util
    ( arithspoon
    , bool
    , asToBool
    , binResToBool
    ) where

import           Control.Exception (ArithException, Handler (..))
import           Control.Lens      (Iso', from, iso, (^.))
import           Control.Spoon     (teaspoonWithHandles)
import           Data.Maybe        (fromJust)


bool :: Iso' Int Bool
bool = iso (/= 0) fromEnum

asToBool :: (Bool -> Bool -> Bool) -> Int -> Int -> Int
asToBool f a b = f (a ^. bool) (b ^. bool) ^. from bool

binResToBool :: (Int -> Int -> Bool) -> Int -> Int -> Int
binResToBool f a b = f a b ^. from bool

arithspoon :: a -> Either String a
arithspoon = fromJust . teaspoonWithHandles [Handler handler] . Right
  where
    handler = return . Just . Left . (show :: ArithException -> String)
