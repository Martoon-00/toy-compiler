{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns  #-}

module Toy.SM.Interpreter
    ( execute
    ) where

import           Control.Lens               (at, use, (%=), (+=), (.=), (?=), (^.))
import           Control.Monad              (replicateM, unless, when)
import           Control.Monad.Error.Class  (throwError)
import           Control.Monad.State.Strict (StateT)
import           Control.Monad.Trans.Either (EitherT)
import           Data.Conduit               (await, yield)
import           Data.Conduit.Lift          (evalStateC)
import           Data.Default               (def)
import           Data.Functor               (($>))
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import qualified Data.Vector                as V
import           Universum                  (type ($))

import           Toy.Exp                    (Exec, ExecInOut, arithspoon, binOp)
import           Toy.SM.Data                (ExecState (..), Inst (..), Insts, LabelId,
                                             esIp, esLocals, esStack)

type ExecProcess m = ExecInOut $ StateT ExecState $ EitherT String m

execute :: Monad m => Insts -> Exec m ()
execute = evalStateC def . executeDo

executeDo :: Monad m => Insts -> ExecProcess m ()
executeDo insts = exec
  where
    exec = do
        ip <- use esIp
        when (ip /= V.length insts) $ do
            step (insts V.! ip)
            esIp += 1
            exec

    step = \case
        Push v    -> push v
        Bin op    -> do
            [b, a] <- replicateM 2 pop
            push =<< arithspoon (binOp op a b)
        Load n    -> use (esLocals . at n) >>= \case
            Nothing  -> throwError $ "No variable " ++ show n ++ " defined"
            Just var -> push var
        Store n   -> pop >>= (esLocals . at n ?= )
        Read      -> await >>= maybe (throwError "No input") push
        Write     -> pop >>= yield
        Label{}   -> step Nop
        Jmp lid   -> ensureEmptyStack >> (esIp .= getLabel lid)
        JmpIf lid -> do
            cond <- pop
            when (cond /= 0) $ step (Jmp lid)
        Nop       -> return ()

    push v = esStack %= (v:)
    pop = use esStack >>= \case
        []   -> throwError "Empty stack"
        s:ss -> (esStack .= ss) $> s
    ensureEmptyStack = do
        st <- use esStack
        unless (null st) $ throwError "Stack expected to be empty"
    getLabel = buildLabelsMap insts

buildLabelsMap :: Insts -> LabelId -> Int
buildLabelsMap (V.toList -> insts) =
    let addLabel (idx, Label li) = M.insert li idx
        addLabel (_  , _       ) = id
        labelsMap = foldr addLabel M.empty $ zip [0..] insts
    in  \labelId -> fromMaybe (error $ "No label " ++ show labelId) $
                    labelsMap ^. at labelId
