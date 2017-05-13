{-# LANGUAGE LambdaCase #-}

module Toy.Lang.Eval
    ( eval
    , callFun
    ) where

import           Control.Lens              (at, use, view, (.=), (<<.=))
import           Control.Monad             (forM)
import           Control.Monad.Error.Class (throwError)
import           Data.Conduit              (await)
import qualified Data.Map                  as M
import           Universum                 (whenNothingM)

import           Toy.Exp                   (ExecInOut, Exp (..), FunSign (..), Value, Var,
                                            arithspoon, binOp, unaryOp)
import           Toy.Lang.Data             (ExecInterrupt (..), MonadExec, Stmt (..),
                                            withStmt)

type FunExecutor m = Stmt -> ExecInOut m (Maybe Value)

-- | Evaluate expression in given variables context
eval
    :: MonadExec m
    => FunExecutor m -> Exp -> ExecInOut m Value
eval executor = \case
    ValueE v       -> return v
    VarE v         ->
        let err = Error $ "No variable " ++ show v ++ " defined"
        in  use (at v) `whenNothingM` throwError err
    UnaryE op v    -> arithspoon =<< (unaryOp op <$> evalRec v)
    BinE op a b    -> arithspoon =<< (binOp op <$> evalRec a <*> evalRec b)
    FunE ("read", _) -> await `whenNothingM` throwError "No input"
    FunE (n, args) -> callFun executor n args  `whenNothingM`
                     throwError "Function didn't return anything"
  where
    evalRec = eval executor

callFun
    :: MonadExec m
    => FunExecutor m -> Var -> [Exp] -> ExecInOut m (Maybe Value)
callFun executor name args = do
    (FunSign _ argNames, body) <-
        view (at name) `whenNothingM`
        throwError (Error $ "No function " ++ show name ++ " defined")
    args' <- forM args $ withStmt (FunCall (name, args)) . eval executor
    curVars <- id <<.= M.fromList (zip argNames args')
    executor body <* (id .= curVars)
