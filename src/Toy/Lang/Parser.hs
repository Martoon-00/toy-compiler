{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Toy.Lang.Parser
    (
    ) where

import           Control.Applicative   (Alternative (..), (*>), (<*))
import           Control.Monad         (join, void)
import           Control.Monad.Writer  (MonadWriter, Writer, listen, runWriter, tell)
import           Data.Char             (isAlphaNum)
import           Data.Functor          (($>), (<$))
import           Data.Maybe            (fromMaybe)
import           Data.Text             (Text)
import           Text.Megaparsec       (char, choice, eof, label, letterChar,
                                        notFollowedBy, satisfy, sepBy, space, try, (<?>))
import           Text.Megaparsec.Char  (anyChar, noneOf)
import           Text.Megaparsec.Expr  (Operator (..), makeExprParser)
import           Text.Megaparsec.Lexer (symbol, symbol')
import           Universum

import           Toy.Base              (FunSign (..), Var (..))
import           Toy.Exp               (Exp (..), charE)
import           Toy.Lang.Data         (FunDecl, Program (..), Stmt (..), forS, funCallS,
                                        mkFunDecls, repeatS, storeArrayS, storeStringS,
                                        whileS, writeS)
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

braces :: Parser a -> Parser a
braces p = sp $ char '{' *> p <* char '}'

newtype Assigns a = Assigns (Writer [Stmt] a)
    deriving (Functor, Applicative, Monad, MonadWriter [Stmt])

noAssigns :: a -> Assigns a
noAssigns = pure

-- | If making and expression is actually complex statement,
-- this function be used to deal with it.
addAssign
    :: (Var -> Stmt)  -- ^ Accepting some unique variable, fill it with required value
    -> Assigns Exp    -- ^ Reference to just filled variable
addAssign f = do
    ((), assigns) <- listen pass
    let used = length assigns
    let var = fromString $ "_" <> show used
    tell [f var]
    return (VarE var)

rememberAssign :: Assigns Stmt -> Assigns ()
rememberAssign = (>>= tell . one)

buildAssigns_ :: Assigns () -> Stmt
buildAssigns_ (Assigns asg) =
    let ((), assigns) = runWriter asg
    in  mconcat assigns

buildAssigns :: Assigns Stmt -> Stmt
buildAssigns (Assigns asg) =
    let (a, assigns) = runWriter asg
    in  mconcat assigns <> a

-- | Expression atom
elemP :: Parser (Assigns Exp)
elemP = sp $ label "Expression atom" $
    mArrayAccessP $
    choice
    [ paren expP
    , noAssigns . ValueE <$> choice
        [ mkParser
        , keywordP "True" $> 1
        , keywordP "False" $> 0
        ]
    , noAssigns . charE <$> (char '\'' *> anyChar <* char '\'')
    , do let args = enumerationP expP
         values <- brackets args <|> braces args
         return $ do
             values' <- sequence values
             addAssign (`storeArrayS` values')
    , do chars <- char '"' *> many (noneOf ['"']) <* char '"'
         return $ addAssign (`storeStringS` chars)
    , do var <- varP
         FunE var <<$>> funCallArgsP ?> noAssigns (VarE var)
    ]

binopLaP' :: Text -> Text -> Operator Parser (Assigns Exp)
binopLaP' sym op = InfixL $ sp (string sym) $> liftM2 (BinE op)

binopLaP :: Text -> Operator Parser (Assigns Exp)
binopLaP = join binopLaP'

expP :: Parser (Assigns Exp)
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

funCallArgsP :: Parser (Assigns [Exp])
funCallArgsP =
    label "Function call arguments" $
    sp $ sequence <$> paren (enumerationP expP)

mArrayAccessP :: Parser (Assigns Exp) -> Parser (Assigns Exp)
mArrayAccessP arrayP =
    label "Array access operator" $
    sp $ do
    a <- arrayP
    mArrayAccessP ((ArrayAccessE <$> a <*>) <$> brackets expP) ?> a

skipP :: Parser Stmt
skipP = space $> Skip

stmtP :: Parser Stmt
stmtP = sp $ choice
    [ do keywordP "Write"
         e <- expP
         return $ buildAssigns (writeS <$> e)
    , do keywordP "If"
         cond <- expP
         keywordP "then"
         onTrue <- stmtsP
         onFalse <- ifContP
         keywordP "fi"
         return $ buildAssigns $ cond <&>
             \cond' -> If cond' onTrue onFalse
    , do keywordP "While"
         cond <- expP
         keywordP "do"
         body <- stmtsP
         keywordP "od"
         return $ buildAssigns $ cond <&>
             \cond' -> whileS cond' body
    , do keywordP "Repeat"
         body <- stmtsP
         keywordP "until"
         cond <- expP
         return $ buildAssigns $ cond <&>
             \cond' -> repeatS body cond'
    , do keywordP "For"
         ini <- stmtP
         _ <- char ','
         cond <- expP
         _ <- char ','
         post <- stmtP
         keywordP "do"
         body <- stmtsP
         keywordP "od"
         return $ buildAssigns $ cond <&>
             \cond' -> forS (ini, cond', post) body
    , do keywordP "Return"
         value <- expP
         return $ buildAssigns (Return <$> value)
    , do keywordP "Skip"
         return Skip
    , withName
    ]
  where
    ifContP = choice
        [ do keywordP "elif"
             cond <- expP
             keywordP "then"
             onTrue <- stmtsP
             onFalse <- ifContP
             return $ buildAssigns $ cond <&>
                  \cond' -> If cond' onTrue onFalse
        , do keywordP "else"
             stmtsP
        , skipP
        ]
    withName = label "Assignment or function" $ do
        var <- varP
        choice
            [ rvalue var
            , do args <- funCallArgsP
                 return $ buildAssigns (funCallS var <$> args)
            ]
    rvalue var = do
        assign :: (Exp -> Assigns Stmt) <- sp . choice $
            [ noAssigns . (var := ) <$ string ":="
            , do exprs <- some (brackets expP)
                 string ":="
                 return $
                   \new -> do
                    exprs' <- sequence exprs
                    let (es, e) = fromMaybe (error "'some' returned 0 elems") (unsnoc exprs')
                    return $ ArrayAssign (foldr ArrayAccessE (VarE var) es) e new
            ]
        res :: Assigns () <-
            do value <- expP
               return $ rememberAssign $ value >>= assign
        return $ buildAssigns_ res

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
