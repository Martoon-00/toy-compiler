{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE Rank2Types    #-}
{-# LANGUAGE TypeOperators #-}

module Toy.Lang.Interpreter
    ( execute
    ) where

import           Control.Lens               (at, (?=))
import           Control.Monad.State.Strict (StateT)
import           Control.Monad.Trans.Either (EitherT)
import           Data.Conduit               (yield)
import           Data.Conduit.Lift          (evalStateC)
import           Data.Default               (def)
import           Universum                  (type ($))

import           Toy.Exp                    (Exec, ExecInOut, LocalVars, eval)
import           Toy.Lang.Data              (Stmt (..), withStmt)

type ExecProcess m = ExecInOut $ StateT LocalVars $ EitherT String m

execute :: Monad m => Stmt -> Exec m ()
execute = evalStateC def . executeDo

-- | Execute given statement.
executeDo :: Monad m => Stmt -> ExecProcess m ()
executeDo = \case
    stmt@(var := expr) -> do
        value <- withStmt stmt $ eval expr
        at var ?= value

    stmt@(Write expr) -> do
        value <- withStmt stmt $ eval expr
        yield value

    stmt@(If cond stmt0 stmt1) -> do
        cond' <- withStmt stmt $ eval cond
        executeDo $ if cond' /= 0 then stmt0 else stmt1

    while@(DoWhile body cond) ->
        executeDo $ Seq body (If cond while Skip)

    Seq stmt0 stmt1 ->
        mapM_ executeDo [stmt0, stmt1]

    Skip -> return ()
