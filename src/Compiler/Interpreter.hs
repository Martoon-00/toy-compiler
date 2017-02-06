{-# LANGUAGE Rank2Types #-}

module Compiler.Interpreter
    ( eval
    , execute
    ) where

import           Control.Lens  (Iso', from, iso, (%~), (^.))
import           Control.Monad (liftM2)
import           Control.Spoon (teaspoon)
import           Data.Bits     (xor, (.&.), (.|.))
import qualified Data.Map      as M

import           Compiler.Data

applyBin :: Exp -> Exp -> (Value -> Value -> Value) -> LocalVars -> Calc
applyBin a b f = liftM2 f <$> eval a <*> eval b

bool :: Iso' Int Bool
bool = iso (/= 0) fromEnum

asToBool :: (Bool -> Bool -> Bool) -> Int -> Int -> Int
asToBool f a b = f (a ^. bool) (b ^. bool) ^. from bool

binResToBool :: (Int -> Int -> Bool) -> Int -> Int -> Int
binResToBool f a b = f a b ^. from bool

eval :: Exp -> LocalVars -> Calc
eval (ValueE v) = const $ Right v
eval (VarE v)   = maybe (Left $ "No variable " ++ show v ++ " defined") Right
                . M.lookup v
eval (a :+ b)   = applyBin a b (+)
eval (a :- b)   = applyBin a b (-)
eval (a :* b)   = applyBin a b (*)
eval (a :/ b)   = (maybe (Left "div error") Right . teaspoon =<<) <$> applyBin a b div
eval (a :% b)   = (maybe (Left "mod error") Right . teaspoon =<<) <$> applyBin a b mod

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

execute :: ExecState -> Exec
execute (ExecState is os vars stmt@(var := expr)) = do
    value <- withStmt stmt $ eval expr vars
    return $ ExecState is os (M.insert var value vars) SkipS

execute (ExecState (i:is) os vars (ReadS var)) =
    Right $ ExecState is os (M.insert var i vars) SkipS
execute (ExecState [] _ _ stmt@(ReadS _)) =
    Left (stmt, "Input unavailable")

execute (ExecState is os vars stmt@(WriteS expr)) = do
    value <- withStmt stmt $ eval expr vars
    return $ ExecState is (value : os) vars SkipS

execute (ExecState is os vars stmt@(IfS cond stmt0 stmt1)) = do
    cond' <- withStmt stmt $ eval cond vars
    -- TODO: scope?
    execute . ExecState is os vars $
        if cond' /= 0 then stmt0 else stmt1

execute (ExecState is os vars while@(WhileS cond body)) =
    execute $ ExecState is os vars (IfS cond (SequenceS body while) SkipS)

execute (ExecState is os vars (SequenceS stmt0 stmt1)) = do
    ExecState is' os' vars' SkipS <- execute (ExecState is os vars stmt0)
    execute (ExecState is' os' vars' stmt1)

execute exec@(ExecState _ _ _ SkipS) =
    return exec
