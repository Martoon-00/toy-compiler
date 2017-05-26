{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Toy.Util.Parsable
    ( Parser
    , Parsable (..)
    , OutputValues (..)
    , parseData
    , parseDataOrFail
    ) where

import           Control.Applicative   (many, (<|>))
import           Data.Proxy            (Proxy (..))
import           Data.Text             (Text)
import           Formatting            (formatToString, stext, (%))
import           Text.Megaparsec       (Dec, Parsec, char, eof, parse, parseErrorPretty,
                                        space)
import           Text.Megaparsec.Lexer (integer, signed)
import           Universum             (first, toText, ($>))

import           Toy.Exp               (Value)

type Parser = Parsec Dec Text

class Parsable a where
    mkParser :: Parser a

    parserName :: Proxy a -> String
    parserName _ = ""

parseDataWith :: String -> Parser a -> Text -> Either Text a
parseDataWith name parser =
    first (toText . parseErrorPretty) . parse parser name

parseData :: forall a. Parsable a => Text -> Either Text a
parseData = parseDataWith (parserName $ Proxy @a) mkParser

parseDataOrFail :: Parsable p => Text -> p
parseDataOrFail =
    either (error . formatToString ("parse failed: "%stext)) id . parseData

instance Parsable Value where
    parserName _ = "Value"
    mkParser = fmap fromIntegral $ many space *> signed space integer

instance Parsable [Value] where
    parserName _ = "Values list"
    mkParser = many mkParser <* many space <* eof

newtype OutputValues = OutputValues
    { getOutputValues :: [Value]
    } deriving (Show, Eq)

instance Parsable OutputValues where
    mkParser = do
        let spaces = many (space <|> char '>' $> ())
        OutputValues <$> (many mkParser <* spaces <* eof)
