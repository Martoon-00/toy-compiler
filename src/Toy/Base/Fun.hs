module Toy.Base.Fun
    ( FunName (..)
    , FunSign (..)
    ) where

import           Data.Default        (Default (..))
import           Data.String         (IsString (..))
import qualified Data.Text.Buildable
import           Formatting          (bprint, build)
import           Universum

import           Toy.Base.Data       (Var)

data FunName
    = FunName Var
    | MainFunName
    deriving (Eq, Ord, Show)

instance IsString FunName where
    fromString = FunName . fromString

instance Default FunName where
    def = MainFunName

instance Buildable FunName where
    build (FunName v) = bprint build v
    build MainFunName = "main"

-- | Function signature
data FunSign = FunSign FunName [Var]
    deriving (Show, Eq)


