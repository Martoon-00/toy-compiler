{-# LANGUAGE TemplateHaskell #-}

module Toy.Lang.Eval
    ( eval
    , findDefinedFun
    ) where

import           Control.Lens              (at, (.=), (<<.=))
import           Control.Monad             (forM)
import           Control.Monad.Error.Class (MonadError (..))
import           Data.Conduit              (await, yield)
import qualified Data.Map                  as M
import           Formatting                (build, sformat, shown, (%))
import           Universum

import           Toy.Base                  (ExecInOut, FunName (..), FunSign (..),
                                            Var (..))
import           Toy.Exp                   (Exp (..), ExpRes (..), arithspoon,
                                            arrayAccess, arrayLength, arrayMake,
                                            arrayMakeU, binOp, unaryOp, valueOnly)
import           Toy.Lang.Data             (ExecInterrupt (..), FunDecl, MonadExec,
                                            Stmt (..), evCurFun, evFunDecls)

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
    LabelE l       -> return $ LabelR l
  where
    evalRec = eval executor
    evalRecV e = evalRec e `valueOnly` "Arithmetic operation on reference"

callFun
    :: MonadExec m
    => FunExecutor m -> FunName -> [Exp] -> ExecInOut m ExpRes
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
    funName = case name of
        (FunName (Var n)) -> n
        (MainFunName)     -> "#!@"
    expectArgs k f = do
        let err = Error $ funName <> ": wrong number of arguments"
        if length args == k then f args else throwError err
    evalValue expr = do
        let err = funName <> ": not a primitive value given"
        eval executor expr `valueOnly` err
    -- evalArray expr = do
    --     let err = funName <> ": not an array given"
    --     eval executor expr `arrayOnly` err

findDefinedFun :: MonadExec m => FunName -> m FunDecl
findDefinedFun name =
    view (evFunDecls . at name) `whenNothingM` throwError noFun
  where
    noFun = Error $ sformat ("No function "%shown%" defined") name

callDefinedFun
    :: MonadExec m
    => FunExecutor m -> FunName -> [Exp] -> ExecInOut m ExpRes
callDefinedFun executor name args = do
    (FunSign _ argNames, body) <- findDefinedFun name
    when (length argNames /= length args) $
        throwError "Invalid number of arguments passed to function"
    args' <- forM args $ eval executor
    curVars <- identity <<.= M.fromList (zip argNames args')
    withEnv (executor body) `finallyE` (identity .= curVars)
  where
    withEnv = local $ evCurFun .~ name
    finallyE :: MonadError e m => m a -> m b -> m a
    finallyE a b = do
        r <- (Right <$> a) `catchError` (return . Left)
        _ <- b
        either throwError pure r
