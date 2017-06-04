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
import           Universum                 (whenNothingM, (<>))

import           Toy.Base                  (ExecInOut, FunSign (..), Var (..))
import           Toy.Exp                   (Exp (..), ExpRes (..), arithspoon,
                                            arrayAccess, arrayLength, arrayMake,
                                            arrayMakeU, binOp, unaryOp, valueOnly)
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
    ArrayUninitE k -> arrayMakeU k
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

    "write" -> expectArgs 1 $ \[x] -> do
        arg <- evalValue x
        yield arg
        return (ValueR 0)

    "arrlen" -> expectArgs 1 $ \[x] -> do
        arrayLength =<< eval executor x

    "arrmake" -> expectArgs 2 $ \[l, v] -> do
        l' <- eval executor l
        v' <- eval executor v
        arrayMake l' v'

    "Arrmake" -> callFun executor "arrmake" args

    _ -> callDefinedFun executor name args
  where
    funName = case name of Var n -> n
    expectArgs k f = do
        let err = Error $ funName <> ": wrong number of arguments"
        if length args == k then f args else throwError err
    evalValue expr = do
        let err = funName <> ": not a primitive value given"
        eval executor expr `valueOnly` err
    -- evalArray expr = do
    --     let err = funName <> ": not an array given"
    --     eval executor expr `arrayOnly` err

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
