{-# LANGUAGE Rank2Types #-}

module Compiler.Interpreter
    ( eval
    , execute
    , executeDebug
    ) where

import           Control.Lens  ((%~))
import           Control.Monad (liftM2)
import           Data.Bits     (xor, (.&.), (.|.))
import qualified Data.Map      as M

import           Compiler.Data (Calc, Exec, ExecState (..), Exp (..), LocalVars,
                                Stmt (..), Value, withStmt)
import           Compiler.Util (arithspoon, asToBool, binResToBool, bool)

applyBin :: Exp -> Exp -> (Value -> Value -> Value) -> LocalVars -> Calc
applyBin a b f = liftM2 f <$> eval a <*> eval b

eval :: Exp -> LocalVars -> Calc
eval (ValueE v) = const $ Right v
eval (VarE v)   = maybe (Left $ "No variable " ++ show v ++ " defined") Right
                . M.lookup v
eval (a :+ b)   = applyBin a b (+)
eval (a :- b)   = applyBin a b (-)
eval (a :* b)   = applyBin a b (*)
eval (a :/ b)   = (arithspoon =<<) <$> applyBin a b div
eval (a :% b)   = (arithspoon =<<) <$> applyBin a b mod

eval (NotE a)   = fmap (bool %~ not) <$> eval a
eval (a :&& b)  = applyBin a b $ asToBool (&&)
eval (a :|| b)  = applyBin a b $ asToBool (||)
eval (a :^ b)   = applyBin a b xor
eval (a :& b)   = applyBin a b (.&.)
eval (a :| b)   = applyBin a b (.|.)

eval (a :> b)   = applyBin a b $ binResToBool (>)
eval (a :< b)   = applyBin a b $ binResToBool (<)
eval (a :>= b)  = applyBin a b $ binResToBool (>=)
eval (a :<= b)  = applyBin a b $ binResToBool (<=)
eval (a :== b)  = applyBin a b $ binResToBool (==)
eval (a :!= b)  = applyBin a b $ binResToBool (/=)

executeDebug :: ExecState -> Exec
executeDebug (ExecState is os vars stmt@(var := expr)) = do
    value <- withStmt stmt $ eval expr vars
    return $ ExecState is os (M.insert var value vars) SkipS

executeDebug (ExecState (i:is) os vars (ReadS var)) =
    Right $ ExecState is os (M.insert var i vars) SkipS
executeDebug (ExecState [] _ _ stmt@(ReadS _)) =
    Left (stmt, "Input unavailable")

executeDebug (ExecState is os vars stmt@(WriteS expr)) = do
    value <- withStmt stmt $ eval expr vars
    return $ ExecState is (value : os) vars SkipS

executeDebug (ExecState is os vars stmt@(IfS cond stmt0 stmt1)) = do
    cond' <- withStmt stmt $ eval cond vars
    -- TODO: scope?
    executeDebug . ExecState is os vars $
        if cond' /= 0 then stmt0 else stmt1

executeDebug (ExecState is os vars while@(WhileS cond body)) =
    executeDebug $ ExecState is os vars (IfS cond (SequenceS body while) SkipS)

executeDebug (ExecState is os vars (SequenceS stmt0 stmt1)) = do
    ExecState is' os' vars' end <- executeDebug (ExecState is os vars stmt0)
    case end of
        SkipS          -> executeDebug (ExecState is' os' vars' stmt1)
        IntS code cont -> Right $ ExecState is' os' vars' $
            IntS code $ SequenceS cont stmt1
        _              -> error "executeDebug: unexpected end operation"

executeDebug exec@(ExecState _ _ _ SkipS) =
    return exec

executeDebug exec@(ExecState _ _ _ (IntS _ _)) =
    return exec

execute :: ExecState -> Exec
execute initExecState = do
    exec@(ExecState is os vars stmt) <- executeDebug initExecState
    case stmt of
        IntS _ cont -> execute (ExecState is os vars cont)
        _           -> Right exec
