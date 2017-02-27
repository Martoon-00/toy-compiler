{-# LANGUAGE Rank2Types #-}

module Toy.Lang.Interpreter
    ( eval
    , execute
    , executeDebug
    ) where

import           Control.Lens  ((%~))
import           Control.Monad (liftM, liftM2)
import           Data.Bits     (xor, (.&.), (.|.))
import qualified Data.Map      as M

import           Toy.Data      (Exp (..), LocalVars, Value, binOp, unaryOp)
import           Toy.Lang.Data (Calc, Exec, ExecState (..), Stmt (..), withStmt)
import           Toy.Lang.Util (arithspoon, asToBool, binResToBool, bool)

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

-- | Proceed in given program state, halting at `Skip` or `Int` operation.
executeDebug :: ExecState -> Exec
executeDebug (ExecState is os vars stmt@(var := expr)) = do
    value <- withStmt stmt $ eval expr vars
    return $ ExecState is os (M.insert var value vars) Skip

executeDebug (ExecState (i:is) os vars (Read var)) =
    Right $ ExecState is os (M.insert var i vars) Skip
executeDebug (ExecState [] _ _ stmt@(Read _)) =
    Left (stmt, "Input unavailable")

executeDebug (ExecState is os vars stmt@(Write expr)) = do
    value <- withStmt stmt $ eval expr vars
    return $ ExecState is (value : os) vars Skip

executeDebug (ExecState is os vars stmt@(If cond stmt0 stmt1)) = do
    cond' <- withStmt stmt $ eval cond vars
    -- TODO: scope?
    executeDebug . ExecState is os vars $
        if cond' /= 0 then stmt0 else stmt1

executeDebug (ExecState is os vars while@(While cond body)) =
    executeDebug $ ExecState is os vars (If cond (Seq body while) Skip)

executeDebug (ExecState is os vars (Seq stmt0 stmt1)) = do
    ExecState is' os' vars' end <- executeDebug (ExecState is os vars stmt0)
    case end of
        Skip          -> executeDebug (ExecState is' os' vars' stmt1)
        Int code cont -> Right $ ExecState is' os' vars' $
            Int code $ Seq cont stmt1
        _             -> error "executeDebug: unexpected end operation"

executeDebug exec@(ExecState _ _ _ Skip) =
    return exec

executeDebug exec@(ExecState _ _ _ (Int _ _)) =
    return exec

-- | Proceed in given program state, halting at `SkipS` operation.
execute :: ExecState -> Exec
execute initExecState = do
    exec@(ExecState is os vars stmt) <- executeDebug initExecState
    case stmt of
        Int _ cont -> execute (ExecState is os vars cont)
        _          -> Right exec
