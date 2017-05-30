{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Toy.Lang.Eval
    ( eval
    ) where

import           Control.Lens              (at, use, view, (.=), (<<.=))
import           Control.Monad             (forM)
import           Control.Monad.Error.Class (throwError)
import           Data.Conduit              (await, yield)
import qualified Data.Map                  as M
import           Formatting                (build, sformat, shown, (%))
import           Universum                 (whenNothingM)

import           Toy.Base                  (ExecInOut, FunSign (..), Var)
import           Toy.Exp                   (Exp (..), ExpRes (..), arithspoon,
                                            arrayAccess, arrayMake, binOp, unaryOp,
                                            valueOnly)
import           Toy.Lang.Data             (ExecInterrupt (..), MonadExec, Stmt (..))

type FunExecutor m = Stmt -> ExecInOut m ExpRes

-- | Evaluate expression in given variables context
eval
    :: MonadExec m
    => FunExecutor m -> Exp -> ExecInOut m ExpRes
eval executor = \case
    ValueE v       -> return (ValueR v)
    VarE v         ->
        let err = Error $ sformat ("No variable "%build%" defined") v
        in  use (at v) `whenNothingM` throwError err
    UnaryE op v    -> fmap ValueR $ arithspoon =<< (unaryOp op <$> evalRecV v)
    BinE op a b    -> fmap ValueR $ arithspoon =<< (binOp op <$> evalRecV a <*> evalRecV b)
    FunE n args    -> callFun executor n args
    ArrayUninitE k -> arrayMake k
    ArrayAccessE a i -> do
        ar <- evalRec a
        ir <- evalRec i
        arrayAccess ar ir
  where
    evalRec = eval executor
    evalRecV e = evalRec e `valueOnly` "Arithmetic operation on reference"

callFun
    :: MonadExec m
    => FunExecutor m -> Var -> [Exp] -> ExecInOut m ExpRes
callFun executor name args = case name of
    "read" ->
        ValueR <$> await `whenNothingM` throwError "No input"

    "write" | [x] <- args -> do
        arg <- eval executor x `valueOnly` "Cannot write array"
        yield arg
        return (ValueR 0)
    "write" ->
        throwError "Wrong number of arguments"
    _ -> callDefinedFun executor name args

callDefinedFun
    :: MonadExec m
    => FunExecutor m -> Var -> [Exp] -> ExecInOut m ExpRes
callDefinedFun executor name args = do
    (FunSign _ argNames, body) <- view (at name) `whenNothingM` throwError noFun
    args' <- forM args $ eval executor
    curVars <- id <<.= M.fromList (zip argNames args')
    executor body <* (id .= curVars)
  where
    noFun = Error $ sformat ("No function "%shown%" defined") name
