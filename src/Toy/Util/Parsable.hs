{-# LANGUAGE Rank2Types #-}

module Toy.Util.Parsable
    ( Parser
    , Parsable (..)
    , parseData
    , parseDataWith
    ) where

import           Data.Proxy      (Proxy (..))
import           Data.Text       (Text)
import           Text.Megaparsec (Dec, Parsec, parse, parseErrorPretty)
import           Universum       (first, toText)

type Parser = Parsec Dec Text

class Parsable a where
    mkParser :: Parser a

    parserName :: Proxy a -> String

parseDataWith :: String -> Parser a -> Text -> Either Text a
parseDataWith name parser =
    first (toText . parseErrorPretty) . parse parser name

parseData :: forall a. Parsable a => Text -> Either Text a
parseData = parseDataWith (parserName $ Proxy @a) mkParser
