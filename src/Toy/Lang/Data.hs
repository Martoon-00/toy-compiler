{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}

module Toy.Lang.Data where

import           Control.Monad.Error.Class (MonadError (..))

import           Toy.Exp.Data              (Exp (..), Var)

-- | Statement of a program.
data Stmt
    = Var := Exp
    | Write Exp
    | If Exp Stmt Stmt
    | DoWhile Stmt Exp  -- ^ @do .. while@ is the most optimal / easy loop from
                        -- asm point of view
    | Seq Stmt Stmt
    | Skip
    deriving (Eq, Show)

infix 0 :=

instance Monoid Stmt where
    mempty = Skip
    mappend = Seq

-- | Adds current statement info to probable evaluation error
withStmt :: MonadError String m => Stmt -> m a -> m a
withStmt stmt = flip catchError $ throwError . \err -> show stmt ++ ": " ++ err

-- | @while@ loop in terms of `Stmt`.
whileS :: Exp -> Stmt -> Stmt
whileS cond stmt = If cond (DoWhile stmt cond) Skip

-- | @read@ to a given variable.
readS :: Var -> Stmt
readS v = v := ReadE
