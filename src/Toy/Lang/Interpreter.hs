{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE Rank2Types    #-}
{-# LANGUAGE TypeOperators #-}

module Toy.Lang.Interpreter
    ( execute
    ) where

import           Control.Lens               (at, (?=), (^?))
import           Control.Monad              (void)
import           Control.Monad.Error.Class  (MonadError (..))
import           Control.Monad.Morph        (hoist)
import           Control.Monad.Reader       (ReaderT, runReaderT)
import           Control.Monad.State.Strict (StateT)
import           Control.Monad.Trans.Either (EitherT, bimapEitherT)
import           Data.Conduit               (yield)
import           Data.Conduit.Lift          (evalStateC)
import           Data.Default               (def)
import           Data.Maybe                 (fromMaybe)
import           Universum                  (type ($))

import           Toy.Exp                    (Exec, ExecInOut, LocalVars, Value)
import           Toy.Lang.Data              (ExecInterrupt (..), FunDecls, Program,
                                             ProgramG (..), Stmt (..), withStmt, _Error)
import qualified Toy.Lang.Eval              as E


type ExecProcess m = ExecInOut $ ReaderT FunDecls $ StateT LocalVars $ EitherT ExecInterrupt m

execute :: Monad m => Program -> Exec m ()
execute (ProgramG funDecls stmt) =
    hoist simplifyErr . evalStateC def . hoist (`runReaderT` funDecls) $
        executeDo stmt
  where
    simplifyErr = bimapEitherT toSimpleErr id
    toSimpleErr = fromMaybe "Return at global scope" . ( ^? _Error)

-- | Execute given statement.
executeDo :: Monad m => Stmt -> ExecProcess m ()
executeDo = \case
    stmt@(var := expr) -> do
        value <- withStmt stmt $ eval expr
        at var ?= value

    stmt@(If cond stmt0 stmt1) -> do
        cond' <- withStmt stmt $ eval cond
        executeDo $ if cond' /= 0 then stmt0 else stmt1

    while@(DoWhile body cond) ->
        executeDo $ Seq body (If cond while Skip)

    stmt@(FunCall "write" [expr]) -> do
        value <- withStmt stmt $ eval expr
        yield value
    FunCall "write" _ ->
        throwError "Wrong number of arguments put to write"

    FunCall name args ->
        void $ E.callFun execFun name args

    stmt@(Return expr) -> do
        value <- withStmt stmt $ eval expr
        throwError $ Returned value

    Seq stmt0 stmt1 ->
        mapM_ executeDo [stmt0, stmt1]

    Skip -> return ()
  where
    eval = E.eval execFun

execFun :: Monad m => Stmt -> ExecProcess m (Maybe Value)
execFun stmt = (Nothing <$ executeDo stmt) `catchError` handler
  where
    handler e@(Error _)  = throwError e
    handler (Returned v) = return (Just v)
