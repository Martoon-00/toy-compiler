{-# OPTIONS_GHC -fno-warn-orphans #-}

module Toy.Lang.Parser
    (
    ) where

import           Control.Applicative   (Alternative (..), (*>), (<*))
import           Control.Monad         (join, void)
import           Data.Char             (isAlphaNum)
import           Data.Functor          (($>), (<$))
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text)
import           Text.Megaparsec       (char, choice, eof, label, letterChar,
                                        notFollowedBy, satisfy, sepBy, space, try, (<?>))
import           Text.Megaparsec.Expr  (Operator (..), makeExprParser)
import           Text.Megaparsec.Lexer (symbol, symbol')
import           Universum

import           Toy.Base              (FunSign (..), Var (..))
import           Toy.Exp               (Exp (..))
import           Toy.Lang.Data         (FunDecl, Program, Program (..), Stmt (..),
                                        mkFunDecls)
import qualified Toy.Lang.Primitives   as L
import           Toy.Util              (Parsable (..), Parser)


-- * Util parsers

sp :: Parser a -> Parser a
sp p = space *> p <* space

string :: Text -> Parser ()
string = void . symbol (pure ()) . toString

stringCI :: Text -> Parser ()
stringCI = void . symbol' (pure ()) . toString

-- somehow plain `<|>` doesn't work without `try` if second parser
-- doesn't consume anything, so this operator is introduced
(?>) :: Parser a -> a -> Parser a
p1 ?> p2 = try p1 <|> space $> p2
infixl 1 ?>

-- * Expression parser

-- Expression parser is split up to layers.
-- Parser at each level accepts as argument a parser for what it considers
-- to be an /atom/, in fact - parser for all lower layers.
-- E.g., parser which cares about sums accepts parser for products, numbers,
-- variables e.t.c.

paren :: Parser a -> Parser a
paren p = sp $ char '(' *> p <* char ')'

brackets :: Parser a -> Parser a
brackets p = sp $ char '[' *> p <* char ']'

-- | Expression atom
elemP :: Parser Exp
elemP = sp $ label "Expression atom" $
    arrayAccessP $
    choice
    [ paren expP
    , ArrayUninitE 0 <$ (char '{' >> space >> char '}')
    , LabelE <$> (char ':' *> varP)
    , ValueE <$> mkParser
    , do var <- varP
         FunE var <$> funCallArgsP ?> VarE var
    ]

binopLaP' :: Text -> Text -> Operator Parser Exp
binopLaP' sym op = InfixL $ sp (string sym) $> BinE op

binopLaP :: Text -> Operator Parser Exp
binopLaP = join binopLaP'

expP :: Parser Exp
expP = makeExprParser elemP $ reverse
    [ -- priority #2
     [binopLaP "||", binopLaP' "!!" "||"]
      -- priority #3
    , binopLaP <$> ["&&"]
      -- priority #4
    , binopLaP <$> ["==", "!=", "<=", ">=", "<", ">"]
      -- priority #6
    , binopLaP <$> ["+", "-"]
      -- priority #7
    , binopLaP <$> ["*", "/", "%"]
    ]

-- * Program parser

varP :: Parser Var
varP = sp $ Var <$> do
    l1 <- letterChar
    ls <- many $ satisfy isAlphaNum <|> char '_'
    return $ toText (l1 : ls)

keywordP :: Text -> Parser ()
keywordP t = () <$ stringCI t <* noCont
  where noCont = notFollowedBy (satisfy isAlphaNum)

enumerationP :: Parser a -> Parser [a]
enumerationP = sp . (`sepBy` char ',') . sp

functionP :: Parser FunDecl
functionP = sp $ do
    keywordP "def" <|> keywordP "fun"
    name <- varP
    args <- paren (enumerationP varP) <?> "Function arguments"
    _    <- space
    keywordP "begin"
    body <- stmtsP ?> Skip
    keywordP "end"
    return (FunSign name args, body)

funCallArgsP :: Parser [Exp]
funCallArgsP =
    label "Function call arguments" $
    sp $ paren (enumerationP expP)

arrayAccessP :: Parser Exp -> Parser Exp
arrayAccessP arrayP =
    label "Array access operator" $
    sp $ do
    a <- arrayP
    arrayAccessP (ArrayAccessE a <$> brackets expP) ?> a

skipP :: Parser Stmt
skipP = space $> Skip

stmtP :: Parser Stmt
stmtP = sp $
        L.write  <$> (keywordP "Write"  *> expP  )
    <|> If       <$> (keywordP "If"     *> expP  )
                 <*> (keywordP "then"   *> stmtsP)
                 <*> ifContP
                 <*  keywordP "fi"
    <|> L.while  <$> (keywordP "While"  *> expP  )
                 <*> (keywordP "do"     *> stmtsP)
                 <*   keywordP "od"
    <|> L.repeat <$> (keywordP "Repeat" *> stmtsP)
                 <*> (keywordP "until"  *> expP  )
    <|> L.for    <$> (keywordP "for"    *> stmtP )
                 <*> (char ','          *> expP  )
                 <*> (char ','          *> stmtP )
                 <*> (keywordP "do"     *> stmtsP)
                 <*   keywordP "od"
    <|> Return   <$> (keywordP "Return" *> expP  )
    <|> Label    <$> (char ':'          *> varP  )
    <|> Goto     <$> (keywordP "Goto"   *> expP  )
    <|> Skip     <$   keywordP "Skip"
    <|> withName
  where
    ifContP = If <$> (keywordP "elif" *> expP  )
                 <*> (keywordP "then" *> stmtsP)
                 <*> ifContP
          <|> keywordP "else" *> stmtsP
          <|> skipP
    withName = label "Assignment or function" $ do
        var <- varP
        choice
            [ rvalue var
            , L.funCall var <$> funCallArgsP
            ]
    rvalue var = do
        assign <- sp . choice $
            [ (var := ) <$ string ":="
            , do exprs <- unsnoc <$> some (brackets expP)
                 let (es, e) = fromMaybe (error "'some' returned 0 elems") exprs
                 string ":="
                 return $ ArrayAssign (foldr ArrayAccessE (VarE var) es) e
            ]
        choice
            [ assign <$> expP
            , L.array assign <$> brackets (enumerationP expP)
            ]



stmtsP :: Parser Stmt
stmtsP = sp $
        char '{' *> stmtsP <* char '}'
    <|> (stmtP >>= \stmt ->
             char ';' *> (Seq stmt <$> stmtsP ?> stmt)
          ?> stmt)

instance Parsable Program where
    parserName _ = "Program"
    mkParser = do
        funs <- many functionP
        prog <- stmtsP ?> Skip
        eof
        return $ Program (mkFunDecls funs) prog
