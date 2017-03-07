module Toy.Lang.Parser
    ( parse
    ) where

import           Control.Applicative  (Alternative (..), (*>), (<*))
import           Data.Attoparsec.Text (Parser, asciiCI, char, decimal, decimal, letter,
                                       parseOnly, satisfy, signed, space, string)
import           Data.Char            (isAlphaNum)
import           Data.Text            (Text)

import           Toy.Exp              (Exp (..), Var (..))
import           Toy.Lang.Data        (Stmt (..))

-- * Util parsers

sp :: Parser a -> Parser a
sp p = many space *> p <* many space

-- * Expression parser

-- Parsers here are split up to layers, each one accepts parser for
-- all lower layers as parser for single atom.
-- Layer number = its operations priority.

-- | Parser for left-associative binary operation.
binopLParser :: Text -> Parser Exp -> Parser Exp
binopLParser op lp = BinE op <$> (lp <* sp (string op)) <*> binopLParser op lp

level6Parser :: Parser Exp -> Parser Exp
level6Parser lp = sp parser
  where
    parser = binopLParser "+" lp
         <|> binopLParser "-" lp
         <|> lp

level7Parser :: Parser Exp -> Parser Exp
level7Parser lp = sp parser
  where
    parser = binopLParser "*" lp
         <|> binopLParser "/" lp
         <|> binopLParser "%" lp
         <|> lp

level4Parser :: Parser Exp -> Parser Exp
level4Parser lp = sp parser
  where
    parser = binopLParser "==" lp
         <|> binopLParser "!=" lp
         <|> binopLParser "<=" lp
         <|> binopLParser ">=" lp
         <|> binopLParser "<"  lp
         <|> binopLParser ">"  lp
         <|> lp

elemParser :: Parser Exp -> Parser Exp
elemParser p = sp $
        char '(' *> p <* char ')'
    <|> ValueE <$> signed decimal

expParser :: Parser Exp
expParser = foldr ($) expParser $
    [ level4Parser
    , level6Parser
    , level7Parser
    , elemParser
    ]

-- * Program parser

varParser :: Parser Var
varParser =
    Var <$> ((:) <$> letter <*> many (satisfy isAlphaNum) <* some space)

keywordParser :: Text -> Parser ()
keywordParser t = () <$ asciiCI t <* some space

stmtParser :: Parser Stmt
stmtParser = sp $
        Read  <$> (keywordParser "Read"  *> varParser)
    <|> Write <$> (keywordParser "Write" *> expParser )
    <|> If    <$> (keywordParser "If"    *> expParser )
              <*> (keywordParser "then"  *> progParser)
              <*> (keywordParser "else"  *> progParser)
    <|> While <$> (keywordParser "While" *> expParser )
              <*> (keywordParser "do"    *> progParser)
    <|> Skip  <$   keywordParser "Skip"
    <|> (:=)  <$> varParser <*> expParser

progParser :: Parser Stmt
progParser = sp $
        char '{' *> progParser <* char '}'
    <|> Seq <$> (stmtParser <* char ';') <*> progParser
    <|> stmtParser

parse :: Text -> Either String Stmt
parse = parseOnly progParser
