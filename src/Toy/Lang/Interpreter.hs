{-# LANGUAGE Rank2Types #-}

module Toy.Lang.Interpreter
    ( eval
    , execute
    ) where

import           Control.Lens  (at, (?=))
import           Data.Conduit  (yield)

import           Toy.Exp       (Exec, eval)
import           Toy.Lang.Data (Stmt (..), withStmt)

-- | Execute given statement.
execute :: Stmt -> Exec ()
execute stmt@(var := expr) = do
    value <- withStmt stmt $ eval expr
    at var ?= value

execute stmt@(Write expr) = do
    value <- withStmt stmt $ eval expr
    yield value

execute stmt@(If cond stmt0 stmt1) = do
    cond' <- withStmt stmt $ eval cond
    execute $ if cond' /= 0 then stmt0 else stmt1

execute while@(DoWhile body cond) =
    execute $ Seq body (If cond while Skip)

execute (Seq stmt0 stmt1) =
    mapM_ execute [stmt0, stmt1]

execute Skip =
    return ()
