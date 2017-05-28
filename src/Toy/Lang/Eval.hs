{-# LANGUAGE LambdaCase #-}

module Toy.Lang.Eval
    ( eval
    ) where

import           Control.Lens              (at, use, view, (.=), (<<.=))
import           Control.Monad             (forM)
import           Control.Monad.Error.Class (throwError)
import           Data.Conduit              (await, yield)
import qualified Data.Map                  as M
import           Formatting                (build, sformat, shown, (%))
import           Universum                 (fromMaybe, whenNothingM)

import           Toy.Base                  (ExecInOut, FunSign (..), Value, Var)
import           Toy.Exp                   (Exp (..), arithspoon, binOp, unaryOp)
import           Toy.Lang.Data             (ExecInterrupt (..), MonadExec, Stmt (..))

type FunExecutor m = Stmt -> ExecInOut m (Maybe Value)

-- | Evaluate expression in given variables context
eval
    :: MonadExec m
    => FunExecutor m -> Exp -> ExecInOut m Value
eval executor = \case
    ValueE v      -> return v
    VarE v        ->
        let err = Error $ sformat ("No variable "%build%" defined") v
        in  use (at v) `whenNothingM` throwError err
    UnaryE op v   -> arithspoon =<< (unaryOp op <$> evalRec v)
    BinE op a b   -> arithspoon =<< (binOp op <$> evalRec a <*> evalRec b)
    FunE "read" _ -> await `whenNothingM` throwError "No input"

    FunE n args   -> callFun executor n args
  where
    evalRec = eval executor

callFun
    :: MonadExec m
    => FunExecutor m -> Var -> [Exp] -> ExecInOut m Value
callFun executor name args = case name of
    "read" ->
        await `whenNothingM` throwError "No input"

    "write" | [x] <- args ->
        eval executor x >>= yield >> return 0
    "write" ->
        throwError "Wrong number of arguments"
    _ -> callDefinedFun executor name args

callDefinedFun
    :: MonadExec m
    => FunExecutor m -> Var -> [Exp] -> ExecInOut m Value
callDefinedFun executor name args = do
    (FunSign _ argNames, body) <- view (at name) `whenNothingM` throwError noFun
    args' <- forM args $ eval executor
    curVars <- id <<.= M.fromList (zip argNames args')
    fromMaybe 0 <$> executor body <* (id .= curVars)
  where
    noFun = Error $ sformat ("No function "%shown%" defined") name
