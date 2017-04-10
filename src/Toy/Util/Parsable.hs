module Toy.Util.Parsable
    ( Parsable (..)
    , OutputValues (..)
    , parseDataOrFail
    ) where

import           Control.Applicative  (many, (<|>))
import           Data.Attoparsec.Text (char, decimal, endOfInput, parseOnly, signed,
                                       space)
import           Data.Text            (Text)

import           Toy.Exp              (Value)

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
