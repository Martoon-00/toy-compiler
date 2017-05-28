{-# OPTIONS_GHC -fno-warn-orphans   #-}

module Toy.Base.Parsable
    ( OutputValues (..)
    ) where

import           Control.Applicative   (many, (<|>))
import           Text.Megaparsec       (char, eof, label, space, spaceChar)
import           Text.Megaparsec.Lexer (integer, signed)
import           Universum             (pass, void, ($>))

import           Toy.Base.Data         (Value)
import           Toy.Util              (Parsable (..))

instance Parsable Value where
    parserName _ = "Value"
    mkParser = label "Value" $ fmap fromIntegral $ space *> signed pass integer

instance Parsable [Value] where
    parserName _ = "Values list"
    mkParser = many (mkParser <* space) <* eof

newtype OutputValues = OutputValues
    { getOutputValues :: [Value]
    } deriving (Show, Eq)

instance Parsable OutputValues where
    parserName _ = "Output values"
    mkParser = do
        let spaces = many (void spaceChar <|> char '>' $> ())
        OutputValues <$> (spaces *> many (mkParser <* spaces)) <* eof
