{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}

module Toy.Lang.Data where

import           Control.Lens              (makePrisms, (%~))
import           Control.Monad.Error.Class (MonadError (..))
import           Control.Monad.Reader      (MonadReader)
import           Control.Monad.State       (MonadState)
import qualified Data.Map                  as M
import           Data.String               (IsString (..))
import           Formatting                (formatToString, shown, string, (%))
import           Universum                 ((<<$>>), (<>))

import           Toy.Exp.Data              (Exp (..), FunSign (..), LocalVars, Value,
                                            Var (..), readE)


-- | Statement of a program.
data Stmt
    = Var := Exp
    | If Exp Stmt Stmt
    | DoWhile Stmt Exp  -- ^ @do .. while@ is the most optimal / easy loop from
                        -- asm point of view
    | FunCall Var [Exp]
    | Return Exp
    | Seq Stmt Stmt
    | Skip
    deriving (Eq, Show)

infix 0 :=

instance Monoid Stmt where
    mempty = Skip
    mappend = Seq

type FunDeclG b = (FunSign, b)

type FunDeclsG b = M.Map Var (FunDeclG b)

type FunDecls = FunDeclsG Stmt

data ProgramG b = ProgramG
    { pFunDecls :: FunDeclsG b
    , pMain     :: b
    } deriving (Show, Eq)

type Program = ProgramG Stmt

instance Functor ProgramG where
    fmap f (ProgramG funcs main) =
        ProgramG (f <<$>> funcs) (f main)

instance Foldable ProgramG where
    foldMap f (ProgramG funcs main) =
        foldMap (foldMap f) funcs <> f main

instance Traversable ProgramG where
    traverse f (ProgramG funcs main) = undefined
        ProgramG <$> traverse (traverse f) funcs <*> f main

data ExecInterrupt
    = Error String
    | Returned Value
    deriving (Eq, Show)
makePrisms ''ExecInterrupt

instance IsString ExecInterrupt where
    fromString = Error

type MonadExec m =
    ( MonadError ExecInterrupt m
    , MonadState LocalVars m
    , MonadReader FunDecls m
    )

-- | Adds current statement info to probable evaluation error
withStmt :: MonadError ExecInterrupt m => Stmt -> m a -> m a
withStmt stmt =
    flip catchError $
    throwError . (_Error %~ formatToString (shown%": "%string) stmt)

-- | @while@ loop in terms of `Stmt`.
whileS :: Exp -> Stmt -> Stmt
whileS cond stmt = If cond (DoWhile stmt cond) Skip

-- | @read@ to a given variable.
readS :: Var -> Stmt
readS v = v := readE

-- | @write@ given expression.
writeS :: Exp -> Stmt
writeS = FunCall "write" . pure
