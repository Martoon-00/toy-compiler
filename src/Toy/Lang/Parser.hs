module Toy.Lang.Parser
    ( parse
    ) where

import           Control.Applicative  (Alternative (..), optional, (*>), (<*))
import           Data.Attoparsec.Text (Parser, asciiCI, char, decimal, decimal,
                                       endOfInput, letter, parseOnly, satisfy, signed,
                                       space, string)
import           Data.Char            (isAlphaNum)
import           Data.Text            (Text)

import           Toy.Exp              (Exp (..), Var (..))
import           Toy.Lang.Data        (Stmt (..))

-- * Util parsers

sp :: Parser a -> Parser a
sp p = many space *> p <* many space

-- * Expression parser

-- Expression parser is split up to layers.
-- Parser at each level accepts as argument a parser for what it considers
-- to be an /atom/, in fact - parser for all lower layers.
-- E.g., parser which cares about sums accepts parser for products, numbers,
-- variables e.t.c.

-- | Parser for layer of left-associative binary operations.
binopLALayerP :: [Text] -> Parser Exp -> Parser Exp
binopLALayerP ops lp = sp $
    let opParser op = BinE op <$> (lp <* sp (string op)) <*> result
        result      = foldr (<|>) lp $ opParser <$> ops
    in  result

-- atom for this parser is expression parser itself
elemP :: Parser Exp -> Parser Exp
elemP p = sp $
        char '(' *> p <* char ')'
    <|> ValueE <$> signed decimal
    <|> VarE   <$> varP

expP :: Parser Exp
expP = foldr ($) expP $
    [ -- priority #4
      binopLALayerP ["==", "!=", "<=", ">=", "<", ">"]
      -- priority #6
    , binopLALayerP ["+", "-"]
      -- priority #7
    , binopLALayerP ["*", "/", "%"]
      -- expression atom
    , elemP
    ]

-- * Program parser

varP :: Parser Var
varP =
    Var <$> ((:) <$> letter <*> many (satisfy isAlphaNum))

keywordP :: Text -> Parser ()
keywordP t = () <$ asciiCI t <* some space

stmtP :: Parser Stmt
stmtP = sp $
        Read  <$> (keywordP "Read"  *> varP )
    <|> Write <$> (keywordP "Write" *> expP )
    <|> If    <$> (keywordP "If"    *> expP )
              <*> (keywordP "then"  *> progP)
              <*> (keywordP "else"  *> progP)
    <|> While <$> (keywordP "While" *> expP )
              <*> (keywordP "do"    *> progP)
    <|> Skip  <$   keywordP "Skip"
    <|> (:=)  <$> (varP <* sp (string "=")) <*> expP

progP :: Parser Stmt
progP = sp $
        char '{' *> progP <* char '}'
    <|> Seq <$> stmtP <* char ';' <*> progP
    <|> stmtP <* optional (char ';')

parse :: Text -> Either String Stmt
parse = parseOnly $ progP <* endOfInput
