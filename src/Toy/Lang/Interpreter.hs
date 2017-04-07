{-# LANGUAGE Rank2Types #-}

module Toy.Lang.Interpreter
    ( eval
    , execute
    ) where

import qualified Data.Map      as M

import           Toy.Exp       (eval)
import           Toy.Lang.Data (Exec, ExecState (..), Stmt (..), withStmt)

-- | Proceed in given program state, halting at `Skip` operation.
execute :: ExecState -> Exec
execute (ExecState is os vars stmt@(var := expr)) = do
    value <- withStmt stmt $ eval expr vars
    return $ ExecState is os (M.insert var value vars) Skip

execute (ExecState is os vars stmt@(Write expr)) = do
    value <- withStmt stmt $ eval expr vars
    return $ ExecState is (value : os) vars Skip

execute (ExecState is os vars stmt@(If cond stmt0 stmt1)) = do
    cond' <- withStmt stmt $ eval cond vars
    execute . ExecState is os vars $
        if cond' /= 0 then stmt0 else stmt1

execute (ExecState is os vars while@(DoWhile body cond)) =
    execute $ ExecState is os vars (Seq body $ If cond while Skip)

execute (ExecState is os vars (Seq stmt0 stmt1)) = do
    ExecState is' os' vars' end <- execute (ExecState is os vars stmt0)
    case end of
        Skip -> execute $ ExecState is' os' vars' stmt1
        _    -> error "execute: unexpected end operation"

execute exec@(ExecState _ _ _ Skip) =
    return exec

-- this is in the end for input reading laziness sake
execute (ExecState (i:is) os vars (Read var)) =
    return $ ExecState is os (M.insert var i vars) Skip
execute (ExecState [] _ _ stmt@(Read _)) =
    Left $ show stmt ++ ": Input unavailable"
