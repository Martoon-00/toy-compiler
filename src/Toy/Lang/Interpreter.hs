{-# LANGUAGE Rank2Types #-}

module Toy.Lang.Interpreter
    ( execute
    ) where

import           Control.Lens               (at, (?=))
import           Control.Monad              (join)
import           Control.Monad.Error.Class  (MonadError (..))
import           Control.Monad.Morph        (hoist)
import           Control.Monad.Reader       (ReaderT, runReaderT)
import           Control.Monad.State.Strict (StateT)
import           Control.Monad.Trans        (MonadIO)
import           Control.Monad.Trans.Either (EitherT, bimapEitherT)
import           Data.Conduit.Lift          (evalStateC)
import           Data.Default               (def)
import           Data.Maybe                 (fromMaybe)
import           Universum                  hiding (StateT)

import           Toy.Base                   (Exec, ExecInOut)
import           Toy.Exp                    (ExpRes (..), LocalVars, NoGcEnv (..),
                                             arraySet, valueOnly)
import           Toy.Lang.Data              (ExecInterrupt (..), FunDecls, Program,
                                             Program (..), Stmt (..), withStmt, _Error)
import qualified Toy.Lang.Eval              as E


type ExecProcess m =
    ExecInOut $
    NoGcEnv $
    ReaderT FunDecls $
    StateT LocalVars $
    EitherT ExecInterrupt m

execute :: MonadIO m => Program -> Exec m ()
execute (Program funDecls stmt) =
    hoist simplifyErr $
    evalStateC def $
    hoist (`runReaderT` funDecls) $
    hoist getNoGcEnv $
    executeDo stmt
  where
    simplifyErr = bimapEitherT toSimpleErr identity
    toSimpleErr = fromMaybe "Return at global scope" . ( ^? _Error)

-- TODO: do smth with 'withStmt' everywhere
-- | Execute given statement.
executeDo :: MonadIO m => Stmt -> ExecProcess m ()
executeDo = \case
    stmt@(var := expr) -> do
        value <- withStmt stmt $ eval expr
        at var ?= value

    stmt@(If cond stmt0 stmt1) -> do
        cond' <- withStmt stmt $ eval cond `valueOnly` "If on reference"
        executeDo $ if cond' /= 0 then stmt0 else stmt1

    while@(DoWhile body cond) ->
        executeDo $ Seq body (If cond while Skip)

    stmt@(Return expr) -> do
        value <- withStmt stmt $ eval expr
        throwError $ Returned value

    ArrayAssign a i e -> do
        join $ arraySet <$> eval a <*> eval i <*> eval e

    Seq stmt0 stmt1 ->
        mapM_ executeDo [stmt0, stmt1]

    Skip -> return ()

    Label{} -> error "Labels are not supported by Lang interpreter :/"

    Goto{} -> error "Labels are not supported by Lang interpreter :/"
  where
    eval = E.eval execFun

execFun :: MonadIO m => Stmt -> ExecProcess m ExpRes
execFun stmt = (NotInitR <$ executeDo stmt) `catchError` handler
  where
    handler e@(Error _)  = throwError e
    handler (Returned v) = return v
