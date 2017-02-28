{-# LANGUAGE Rank2Types #-}

module Toy.Lang.Interpreter
    ( eval
    , execute
    ) where

import qualified Data.Map      as M

import           Toy.Data      (Exp (..), LocalVars, binOp, unaryOp)
import           Toy.Lang.Data (Calc, Exec, ExecState (..), Stmt (..), withStmt)
import           Toy.Lang.Util (arithspoon)

-- | Evaluate expression in given variables context
eval :: Exp -> LocalVars -> Calc
eval e vars = ev e
  where
    ev (ValueE v   ) = Right v
    ev (VarE v     ) =
        maybe (Left $ "No variable " ++ show v ++ " defined") Right $
        M.lookup v vars
    ev (UnaryE op v) = arithspoon =<< (unaryOp op <$> ev v)
    ev (BinE op a b) = arithspoon =<< (binOp op <$> ev a <*> ev b)

-- | Proceed in given program state, halting at `SkipS` operation.
execute :: ExecState -> Exec
execute (ExecState is os vars stmt@(var := expr)) = do
    value <- withStmt stmt $ eval expr vars
    return $ ExecState is os (M.insert var value vars) Skip

execute (ExecState (i:is) os vars (Read var)) =
    Right $ ExecState is os (M.insert var i vars) Skip
execute (ExecState [] _ _ stmt@(Read _)) =
    Left (stmt, "Input unavailable")

execute (ExecState is os vars stmt@(Write expr)) = do
    value <- withStmt stmt $ eval expr vars
    return $ ExecState is (value : os) vars Skip

execute (ExecState is os vars stmt@(If cond stmt0 stmt1)) = do
    cond' <- withStmt stmt $ eval cond vars
    -- TODO: scope?
    execute . ExecState is os vars $
        if cond' /= 0 then stmt0 else stmt1

execute (ExecState is os vars while@(While cond body)) =
    execute $ ExecState is os vars (If cond (Seq body while) Skip)

execute (ExecState is os vars (Seq stmt0 stmt1)) = do
    ExecState is' os' vars' end <- execute (ExecState is os vars stmt0)
    case end of
        Skip -> execute (ExecState is' os' vars' stmt1)
        _    -> error "execute: unexpected end operation"

execute exec@(ExecState _ _ _ Skip) =
    return exec
