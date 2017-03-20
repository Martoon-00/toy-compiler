{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}

module Test.Util
    ( Parsable (..)
    , OutputValues (..)
    , parseDataOrFail
    , instsSM
    ) where

import           Control.Applicative  (many, (<|>))
import           Control.Monad.Trans  (MonadIO (..))
import           Data.Attoparsec.Text (char, decimal, endOfInput, parseOnly, signed,
                                       space)
import           Data.Text            (Text)
import           Test.Hspec.Core.Spec (SpecM (..))
import           Test.QuickCheck      (Property, conjoin, property, (.&&.))

import           Toy.Exp              (Value)
import qualified Toy.SM               as SM

class Parsable e where
    parseData :: Text -> Either String e

parseDataOrFail :: Parsable p => Text -> p
parseDataOrFail =
    either (\err -> error $ "parse failed: " ++ err) id . parseData

instance Parsable [Value] where
    parseData = do
        let spaces = many space
        parseOnly $ many (spaces *> signed decimal) <* spaces <* endOfInput

newtype OutputValues = OutputValues
    { getOutputValues :: [Value]
    } deriving (Show, Eq)

instance Parsable OutputValues where
    parseData = do
        let spaces = many (space <|> char '>')
            value  = spaces *> signed decimal
        parseOnly $ OutputValues <$> many value <* spaces <* endOfInput


instsSM :: SM.Insts -> SM.Insts
instsSM = id

instance Monoid Property where
    mempty = property True
    mappend = (.&&.)
    mconcat = conjoin

instance MonadIO (SpecM a) where
    liftIO = SpecM . liftIO
