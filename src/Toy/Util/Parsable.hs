{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Toy.Util.Parsable
    ( Parser
    , Parsable (..)
    , OutputValues (..)
    , parseData
    , parseDataWith
    ) where

import           Control.Applicative   (many, (<|>))
import           Control.Monad         (void)
import           Data.Proxy            (Proxy (..))
import           Data.Text             (Text)
import           Text.Megaparsec       (Dec, Parsec, char, eof, label, parse,
                                        parseErrorPretty, space, spaceChar)
import           Text.Megaparsec.Lexer (integer, signed)
import           Universum             (first, pass, toText, ($>))

import           Toy.Base              (Value)

type Parser = Parsec Dec Text

class Parsable a where
    mkParser :: Parser a

    parserName :: Proxy a -> String

parseDataWith :: String -> Parser a -> Text -> Either Text a
parseDataWith name parser =
    first (toText . parseErrorPretty) . parse parser name

parseData :: forall a. Parsable a => Text -> Either Text a
parseData = parseDataWith (parserName $ Proxy @a) mkParser

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
