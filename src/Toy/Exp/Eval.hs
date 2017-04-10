{-# LANGUAGE FlexibleContexts #-}

module Toy.Exp.Eval
    ( eval
    ) where

import           Control.Lens              (at, use)
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.Error.Class (MonadError)
import           Control.Monad.State       (MonadState)
import           Data.Conduit              (await)
import           Data.Conduit              (ConduitM)
import           Universum                 (whenNothingM)

import           Toy.Exp.Data              (Exp (..), LocalVars, Value)
import           Toy.Exp.Operations        (binOp, unaryOp)
import           Toy.Exp.Util              (arithspoon)

-- | Evaluate expression in given variables context
eval
    :: (MonadState LocalVars m, MonadError String m)
    => Exp -> ConduitM Value Value m Value
eval e = case e of
    ValueE v    -> return v
    VarE v      ->
        let err = "No variable " ++ show v ++ " defined"
        in  use (at v) `whenNothingM` throwError err
    ReadE       -> await `whenNothingM` throwError "No input"
    UnaryE op v -> arithspoon =<< (unaryOp op <$> eval v)
    BinE op a b -> arithspoon =<< (binOp op <$> eval a <*> eval b)
