{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns  #-}

module Toy.SM.Interpreter
    ( execute
    ) where

import           Control.Lens               (at, use, uses, (%=), (+=), (.=), (<<.=),
                                             (?=), (^.))
import           Control.Monad              (forever, mzero, replicateM, void, when)
import           Control.Monad.Error.Class  (throwError)
import           Control.Monad.Morph        (hoist)
import           Control.Monad.State.Strict (StateT)
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Either (EitherT)
import           Data.Conduit               (await, yield)
import           Data.Conduit.Lift          (evalStateC, execStateC, runMaybeC)
import           Data.Default               (def)
import           Data.Functor               (($>))
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import qualified Data.Vector                as V
import           Formatting                 (formatToString, int, string, (%))
import           Universum                  (type ($))

import           Toy.Exp                    (Exec, ExecInOut, FunSign (..), arithspoon,
                                             binOp)
import           Toy.SM.Data                (ExecState (..), IP, Inst (..), Insts,
                                             LabelId (..), esIp, esLocals, esStack)

type ExecProcess m = ExecInOut $ StateT ExecState $ EitherT String m

execute :: Monad m => Insts -> Exec m ()
execute = evalStateC def . executeDo

executeDo :: Monad m => Insts -> ExecProcess m ()
executeDo insts = void . runMaybeC . forever $
    uses esIp (insts V.!) >>= step >> esIp += 1
  where
    step = \case
        Push v     -> push v
        Pop        -> void pop
        Bin op     -> do
            [b, a] <- replicateM 2 pop
            push =<< arithspoon (binOp op a b)
        Load n     -> use (esLocals . at n) >>= \case
            Nothing  -> throwError $ "No variable " ++ show n ++ " defined"
            Just var -> push var
        Store n    -> pop >>= (esLocals . at n ?= )
        Read       -> await >>= maybe (throwError "No input") push
        Write      -> pop >>= yield
        Label{}    -> step Nop
        Jmp lid    -> do
            ensureStackSize 0 "jump"
            esIp .= getLabel (CLabel lid)
        JmpIf lid  -> do
            cond <- pop
            when (cond /= 0) $ step (Jmp lid)
        Call (FunSign name args) -> do
            stack <- esStack <<.= []
            ensureStackSize (length args) "function call"
            let funExecState = ExecState
                    { _esLocals = M.fromList (zip args stack)
                    , _esStack  = []
                    , _esIp     = getLabel (FLabel name)
                    }
            -- run execution with its own `ExecState`, not allowing it to
            -- infulence on our current state
            funEndExecState <-
                hoist (lift . lift) $ execStateC funExecState $ executeDo insts

            esStack .= _esStack funEndExecState
            ensureStackSize 1 "function end"
        Ret        -> lift mzero
        Enter _    -> throwError "Got into out of nowhere"
        Nop        -> return ()

    push v = esStack %= (v:)
    pop = use esStack >>= \case
        []   -> throwError "Empty stack"
        s:ss -> (esStack .= ss) $> s
    ensureStackSize size reason = do
        st <- use esStack
        when (length st /= size) . throwError $
            formatToString ("Stack has size "%int%", but expected to have "%
                            int%" instead, reason: "%string)
                           (length st) size reason
    getLabel = buildLabelsMap insts

buildLabelsMap :: Insts -> LabelId -> IP
buildLabelsMap (V.toList -> insts) =
    let addLabel (idx, Label li) = M.insert li idx
        addLabel _               = id
        labelsMap = foldr addLabel M.empty $ zip [0..] insts
    in  \labelId -> fromMaybe (error $ "No label " ++ show labelId) $
                    labelsMap ^. at labelId
