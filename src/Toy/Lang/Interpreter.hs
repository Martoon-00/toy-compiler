{-# LANGUAGE Rank2Types #-}

module Toy.Lang.Interpreter
    ( execute
    ) where

import           Control.Lens               (at, ix, (?=))
import           Control.Monad              (join)
import           Control.Monad.Error.Class  (MonadError (..))
import           Control.Monad.Morph        (hoist)
import           Control.Monad.Reader       (ReaderT, runReaderT)
import           Control.Monad.State.Strict (StateT)
import           Control.Monad.Trans        (MonadIO)
import           Control.Monad.Trans.Except (withExceptT)
import           Data.Conduit.Lift          (evalStateC)
import           Data.Default               (def)
import           Universum                  hiding (StateT)

import           Toy.Base                   (Exec, ExecInOut, FunName (..))
import           Toy.Exp                    (ExpRes (..), LocalVars, NoGcEnv (..),
                                             arraySet, labelOnly, valueOnly)
import           Toy.Lang.Data              (Branch (..), ExecEnv (..),
                                             ExecInterrupt (..), Program, Program (..),
                                             Stmt (..), StmtCoord, StmtFunCoord (..),
                                             evCurFun, evULabelCoords, withStmt)
import qualified Toy.Lang.Eval              as E
import           Toy.Lang.Util              (balanceProgram, buildULabelsMap)


type ExecProcess m =
    ExecInOut $
    NoGcEnv $
    ReaderT ExecEnv $
    StateT LocalVars $
    ExceptT ExecInterrupt m

execute :: MonadIO m => Program -> Exec m ()
execute (balanceProgram -> prog@Program{..}) =
    hoist simplifyErr $
    evalStateC def $
    hoist (`runReaderT` env) $
    hoist getNoGcEnv $
    launch
  where
    env = ExecEnv getProgram (buildULabelsMap prog) def

    launch = do
        mainStmt <- getMainStmt
        let action c = executeDo c mainStmt
            handler (Jumped (StmtFunCoord MainFunName c)) = action (Just c)
            handler e                                     = throwError e
        action Nothing `catchError` handler

    getMainStmt = note "No entry point found" $
                  getProgram ^? ix MainFunName . _2

    simplifyErr = withExceptT $ \case
        Error e -> e
        Returned _ -> "Return at global scope"
        Jumped _ -> "Jumped outside of main"

-- | Execute given statement.
-- If coordinates are specified, all statements till referenced point are omitted.
executeDo :: MonadIO m => Maybe StmtCoord -> Stmt -> ExecProcess m ()
executeDo mcoord = \case
    stmt@(var := expr) -> do
        value <- withStmt stmt $ eval expr
        at var ?= value

    stmt@(If cond stmt0 stmt1) -> do
        cond' <- withStmt stmt $ eval cond `valueOnly` "If on reference"
        case mcoord of
            Nothing ->
                executeDo Nothing $
                    if cond' /= 0 then stmt0 else stmt1
            Just (LeftPath : path) ->
                executeDo (Just path) stmt0
            Just (RightPath : path) ->
                executeDo (Just path) stmt0
            Just [] ->
                error "Coordinates doesn't fit"

    while@(DoWhile body cond) -> do
        executeDo mcoord body
        executeDo Nothing (If cond while Skip)

    stmt@(Return expr) -> do
        value <- withStmt stmt $ eval expr
        throwError $ Returned value

    ArrayAssign a i e -> do
        join $ arraySet <$> eval a <*> eval i <*> eval e

    Seq stmt0 stmt1 ->
        case mcoord of
            Nothing ->
                mapM_ (executeDo Nothing) [stmt0, stmt1]
            Just (LeftPath : path) -> do
                executeDo (Just path) stmt0
                executeDo Nothing stmt1
            Just (RightPath : path) ->
                executeDo (Just path) stmt1
            Just [] ->
                error "Coordinates doesn't fit"

    Skip -> return ()

    Label{} -> return ()

    Goto ul -> do
        ul' <- eval ul `labelOnly` "Goto not on label"
        mc <- view (evULabelCoords . at ul')
        case mc of
            Nothing -> throwError . fromString $ "No such label: " <> show ul'
            Just c  -> throwError (Jumped c)
  where
    eval = E.eval (execFun Nothing)

execFun :: MonadIO m => Maybe StmtCoord -> Stmt -> ExecProcess m ExpRes
execFun mcoord stmt = (NotInitR <$ executeDo mcoord stmt) `catchError` handler
  where
    handler e@(Error _)  = throwError e
    handler (Returned v) = return v
    handler e@(Jumped c) = do
        curFun <- view evCurFun
        if curFun == sfcFun c
            then execFun (Just $ sfcCoord c) stmt
            else throwError e
