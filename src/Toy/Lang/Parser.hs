{-# OPTIONS_GHC -fno-warn-orphans   #-}

module Toy.Lang.Parser
    (
    ) where

import           Control.Applicative   (Alternative (..), optional, (*>), (<*))
import           Control.Lens          ((&))
import           Control.Monad         (void)
import           Data.Char             (isAlphaNum)
import           Data.Functor          (($>), (<$))
import qualified Data.Map              as M
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text)
import           Text.Megaparsec       (char, eof, letterChar, notFollowedBy, satisfy,
                                        sepBy, space)
import           Text.Megaparsec.Lexer (symbol, symbol')
import           Universum             (toString)

import           Toy.Exp               (Exp (..), FunCallParams, FunSign (..), Var (..))
import           Toy.Lang.Data         (FunDecl, Program, Program (..), Stmt (..), forS,
                                        mkFunDecls, repeatS, whileS, writeS)
import           Toy.Util              (Parsable (..), Parser)

-- * Util parsers

sp :: Parser a -> Parser a
sp p = many space *> p <* many space

string :: Text -> Parser ()
string = void . symbol space . toString

stringCI :: Text -> Parser ()
stringCI = void . symbol' space . toString

-- * Expression parser

-- Expression parser is split up to layers.
-- Parser at each level accepts as argument a parser for what it considers
-- to be an /atom/, in fact - parser for all lower layers.
-- E.g., parser which cares about sums accepts parser for products, numbers,
-- variables e.t.c.

-- | Parser for layer of left-associative binary operations.
binopLALayerP :: M.Map Text Text -> [Text] -> Parser Exp -> Parser Exp
binopLALayerP replacements ops lp = sp $ do
    let replace op = fromMaybe op $ M.lookup op replacements
    let opParser op = flip (BinE (replace op)) <$> (sp (string op) *> lp)
    first <- lp
    nexts <- many $ foldr (<|>) (pure id) $ opParser <$> ops
    return $ foldl (&) first nexts

paren :: Parser a -> Parser a
paren p = sp $ char '(' *> p <* char ')'

-- atom for this parser is expression parser itself
elemP :: Parser Exp -> Parser Exp
elemP p = sp $
        paren p
    <|> ValueE <$> mkParser
    <|> FunE   <$> funCallP
    <|> VarE   <$> varP

expP :: Parser Exp
expP = foldr ($) expP $
    [ -- priority #2
      binopLALayerP (M.fromList [("!!", "||")]) ["!!"]
      -- priority #3
    , binopLALayerP mempty ["&&"]
      -- priority #4
    , binopLALayerP mempty ["==", "!=", "<=", ">=", "<", ">"]
      -- priority #6
    , binopLALayerP mempty ["+", "-"]
      -- priority #7
    , binopLALayerP mempty ["*", "/", "%"]
      -- expression atom
    , elemP
    ]

-- * Program parser

varP :: Parser Var
varP = Var <$> do
    l1 <- letterChar <|> char '_'
    ls <- many $ satisfy isAlphaNum <|> char '_'
    return (l1 : ls)

keywordP :: Text -> Parser ()
keywordP t = () <$ stringCI t <* noCont
  where noCont = notFollowedBy (satisfy isAlphaNum)

enumerationP :: Parser a -> Parser [a]
enumerationP = sp . (`sepBy` char ',') . sp

functionP :: Parser FunDecl
functionP = sp $ do
    keywordP "def" <|> keywordP "fun"
    name <- sp varP
    args <- paren $ enumerationP varP
    _    <- many space
    keywordP "begin"
    body <- stmtsP <|> skipP
    keywordP "end"
    return (FunSign name args, body)

funCallP :: Parser FunCallParams
funCallP = (,) <$> varP <* many space <*> paren (enumerationP expP)

skipP :: Parser Stmt
skipP = many space $> Skip

stmtP :: Parser Stmt
stmtP = sp $
        writeS  <$> (keywordP "Write"  *> expP  )
    <|> If      <$> (keywordP "If"     *> expP  )
                <*> (keywordP "then"   *> stmtsP)
                <*> ifContP
                <*  keywordP "fi"
    <|> whileS  <$> (keywordP "While"  *> expP  )
                <*> (keywordP "do"     *> stmtsP)
                <*   keywordP "od"
    <|> repeatS <$> (keywordP "Repeat" *> stmtsP)
                <*> (keywordP "until"  *> expP  )
    <|> forS    <$> (keywordP "for"    *> stmtP )
                <*> (char ','          *> expP  )
                <*> (char ','          *> stmtP )
                <*> (keywordP "do"     *> stmtsP)
                <*   keywordP "od"
    <|> Return  <$> (keywordP "Return" *> expP)
    <|> FunCall <$>  funCallP
    <|> Skip    <$   keywordP "Skip"
    <|> (:=)    <$> (varP <* sp (string ":=")) <*> expP
  where
    ifContP = If <$> (keywordP "elif" *> expP  )
                 <*> (keywordP "then" *> stmtsP)
                 <*> ifContP
          <|>         keywordP "else" *> stmtsP
          <|>         skipP

stmtsP :: Parser Stmt
stmtsP = sp $
        char '{' *> stmtsP <* char '}'
    <|> Seq <$> stmtP <* char ';' <*> stmtsP
    <|> stmtP <* optional (char ';')

instance Parsable Program where
    parserName _ = "Program"
    mkParser = do
        funs <- many functionP
        prog <- stmtsP <|> skipP
        eof
        return $ Program (mkFunDecls funs) prog
