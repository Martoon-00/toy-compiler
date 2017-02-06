module Compiler.Interpreter
    ( execute
    ) where

import qualified Data.Map      as M

import           Compiler.Data

execute :: ExecState -> Exec
execute (ExecState is os vars (var := expr)) = do
    value <- eval vars expr
    return $ ExecState is os (M.insert var value vars) SkipS

execute (ExecState (i:is) os vars (ReadS var)) =
    Right $ ExecState is os (M.insert var i vars) SkipS
execute (ExecState [] _ _ stmt@(ReadS _)) =
    Left (stmt, "Input unavailable")

execute (ExecState is os vars (WriteS expr)) = do
    value <- eval vars expr
    return $ ExecState is (value : os) vars SkipS

execute (ExecState is os vars (IfS cond stmt0 stmt1)) = do
    cond' <- eval vars cond
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
