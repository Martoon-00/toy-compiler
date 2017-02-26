{-# LANGUAGE TupleSections #-}

module Toy.SM.Interpreter
    ( execute
    ) where

import           Control.Lens (at, (^.))
import           Data.List    (uncons)
import qualified Data.Map     as M
import qualified Data.Vector  as V

import           Toy.Data     (Value)
import           Toy.SM.Data  (BinOp, Exec, ExecState (..), Inst (..))

evalBinOp :: BinOp -> Value -> Value -> Value
evalBinOp "+" = (+)
evalBinOp _   = undefined

execute :: ExecState -> Exec
execute exec@(ExecState is os vars stack insts ip)
    | ip == V.length insts = return exec
    | otherwise            = execute =<< step stack (insts V.! ip)
  where
    step :: [Value] -> Inst -> Exec
    step _ (Push k) =
        return $ ExecState is os vars (k:stack) insts (ip + 1)

    step (a:b:stack') (Bin op) =
        return $ ExecState is os vars (evalBinOp op a b : stack') insts (ip + 1)
    step _            (Bin _ ) =
        failure "Not enough arguments on stack"

    step _ (Ld name) = case vars ^. at name of
        Just var -> return $ ExecState is os vars (var:stack) insts (ip + 1)
        Nothing  -> failure $ "No variable " ++ show name ++ " defined"

    step (v:stack') (St name) =
        return $ ExecState is os (M.insert name v vars) stack' insts (ip + 1)
    step _          (St _   ) =
        failure "Stack is empty"

    step _ Read = case uncons is of
        Nothing       -> Left (ip, "No input")
        Just (i, is') -> return $ ExecState is' os vars (i:stack) insts (ip + 1)

    step (v:stack') Write =
        return $ ExecState is (v:os) vars stack' insts (ip + 1)
    step _          Write =
        failure "Stack is empty"

    step _ Nop = return exec

    failure = Left . (ip, )
