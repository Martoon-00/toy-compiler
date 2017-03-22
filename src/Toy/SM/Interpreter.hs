{-# LANGUAGE TupleSections #-}

module Toy.SM.Interpreter
    ( execute
    ) where

import           Control.Lens (at, (%~), (^.), _Left)
import           Data.List    (uncons)
import qualified Data.Map     as M
import qualified Data.Vector  as V

import           Toy.Exp      (Value, arithspoon, binOp)
import           Toy.SM.Data  (Exec, ExecState (..), Inst (..))

execute :: ExecState -> Exec
execute exec@(ExecState is os vars stack insts ip)
    | ip == V.length insts = return exec
    | otherwise            = execute =<< step stack (insts V.! ip)
  where
    step :: [Value] -> Inst -> Exec
    step _ (Push k) =
        return $ ExecState is os vars (k:stack) insts (ip + 1)

    step (b:a:stack') (Bin op) = do
        eval <- describeError $ arithspoon $ binOp op a b
        return $ ExecState is os vars (eval : stack') insts (ip + 1)
    step _            (Bin _ ) =
        failure "Not enough arguments on stack"

    step _ (Load name) = case vars ^. at name of
        Just var -> return $ ExecState is os vars (var:stack) insts (ip + 1)
        Nothing  -> failure $ "No variable " ++ show name ++ " defined"

    step (v:stack') (Store name) =
        return $ ExecState is os (M.insert name v vars) stack' insts (ip + 1)
    step _          (Store _   ) =
        failure "Stack is empty"

    step _ Read = case uncons is of
        Nothing       -> failure "No input"
        Just (i, is') -> return $ ExecState is' os vars (i:stack) insts (ip + 1)

    step (v:stack') Write =
        return $ ExecState is (v:os) vars stack' insts (ip + 1)
    step _          Write =
        failure "Stack is empty"

    step _ Nop =
        return $ ExecState is os vars stack insts (ip + 1)

    failure err = Left $ mconcat ["#", show ip, ": ", err]
    describeError = _Left %~ \err -> mconcat ["#", show ip, ": ", err]
