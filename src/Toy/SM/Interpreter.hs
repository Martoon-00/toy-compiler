{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}

module Toy.SM.Interpreter
    ( execute
    ) where

import           Control.Lens (at, (%~), (^.), _Left)
import           Data.List    (uncons)
import qualified Data.Map     as M
import           Data.Maybe   (fromMaybe)
import qualified Data.Vector  as V

import           Toy.Exp      (Value, arithspoon, binOp)
import           Toy.SM.Data  (Exec, ExecState (..), Inst (..), Insts, LabelId)

execute :: Insts -> ExecState -> Exec
execute insts exec@(ExecState is os vars stack ip)
    | ip == V.length insts = return exec
    | otherwise            = execute insts =<< step stack (insts V.! ip)
  where
    step :: [Value] -> Inst -> Exec
    step _ (Push k) =
        return $ ExecState is os vars (k:stack) (ip + 1)

    step (b:a:stack') (Bin op) = do
        eval <- describeError $ arithspoon $ binOp op a b
        return $ ExecState is os vars (eval : stack') (ip + 1)
    step _            (Bin _ ) =
        failure "Not enough arguments on stack"

    step _ (Load name) = case vars ^. at name of
        Just var -> return $ ExecState is os vars (var:stack) (ip + 1)
        Nothing  -> failure $ "No variable " ++ show name ++ " defined"

    step (v:stack') (Store name) =
        return $ ExecState is os (M.insert name v vars) stack' (ip + 1)
    step _          (Store _   ) =
        failure "Stack is empty"

    step _ Read = case uncons is of
        Nothing       -> failure "No input"
        Just (i, is') -> return $ ExecState is' os vars (i:stack) (ip + 1)

    step (v:stack') Write =
        return $ ExecState is (v:os) vars stack' (ip + 1)
    step _          Write =
        failure "Stack is empty"

    step s Label{} = step s Nop

    step [] (Jmp labelId) =
        return $ ExecState is os vars stack (getLabel labelId)
    step st jmp@(Jmp _) =
        error $ "Not empty stack on " ++ show jmp ++ ": " ++ show st

    step [cond] (JmpIf labelId) =
        return $ ExecState is os vars [] $
            if cond /= 0 then getLabel labelId else ip + 1
    step st jmpif@(JmpIf _) =
        error $ "Not single value in stack on " ++ show jmpif
             ++ ": " ++ show st

    step _ Nop =
        return $ ExecState is os vars stack (ip + 1)

    getLabel = buildLabelsMap insts

    failure err = Left $ mconcat ["#", show ip, ": ", err]
    describeError = _Left %~ \err -> mconcat ["#", show ip, ": ", err]

buildLabelsMap :: Insts -> LabelId -> Int
buildLabelsMap (V.toList -> insts) =
    let addLabel (idx, Label li) = M.insert li idx
        addLabel (_  , _       ) = id
        labelsMap = foldr addLabel M.empty $ zip [0..] insts
    in  \labelId -> fromMaybe (error $ "No label " ++ show labelId) $
                    labelsMap ^. at labelId
