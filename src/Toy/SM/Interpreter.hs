{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}

module Toy.SM.Interpreter
    ( execute
    ) where

import           Control.Lens              (at, has, ix, use, (%=), (&), (+=), (.=), (.~),
                                            (<<%=), (?=), (^.), (^?))
import           Control.Monad             (forever, mzero, replicateM, replicateM_,
                                            unless, void, when)
import           Control.Monad.Error.Class (MonadError (..))
import           Control.Monad.Morph       (hoist)
import           Control.Monad.Trans       (lift)
import           Data.Conduit              (await, yield)
import           Data.Conduit.Lift         (evalStateC, execStateC, runMaybeC)
import           Data.Default              (def)
import           Data.Functor              (($>))
import qualified Data.Map                  as M
import qualified Data.Vector               as V
import           Formatting                (build, int, sformat, shown, string, (%))
import           Universum                 (Text, whenNothing, (<>))

import           Toy.Base                  (Exec, FunSign (..))
import           Toy.Exp                   (ExpRes (..), arithspoon, arrayOnly, binOp,
                                            valueOnly)
import           Toy.SM.Data               (ExecState (..), IP, Inst (..), Insts,
                                            LabelId (..), esIp, esLocals, esStack,
                                            initFunName)
import           Toy.Util                  (mapError)

execute :: Monad m => Insts -> Exec m ()
execute insts = evalStateC def{ _esIp = programEntry } $ executeDo
  where
    -- executeDo :: Monad m => ExecInOut $ StateT ExecState $ EitherT Text m ()
    executeDo = void . runMaybeC . forever $
        getCurInst >>= step >> esIp += 1

    getCurInst = do
        i <- use esIp
        insts V.!? i `whenNothing` throwError "To infinity and beyond! >>>"

    step = \case
        Push v      -> push v
        Drop        -> void pop
        Dup         -> replicateM_ 2 . push =<< pop
        Bin op      -> do
            let lil = "Arithmetic operation on reference"
            [b, a] <- replicateM 2 (pop `valueOnly` lil)
            push . ValueR =<< arithspoon (binOp op a b)
        Load n      -> use (esLocals . at n) >>= \case
            Nothing  -> throwError $ sformat ("No variable "%build%" defined") n
            Just var -> push var
        Store n     -> pop >>= (esLocals . at n ?= )
        ArrayMake k ->
            push (ArrayR $ V.replicate k (ValueR 0))
        ArrayAccess -> mapError ("array access: " <>) $ do
            i <- pop `valueOnly` "index is not a number"
            let i' = fromIntegral i
            a <- pop `arrayOnly` "array expected"
            e <- (a ^? ix i') `whenNothing` throwError "index out of bounds"
            push e
        ArraySet k -> mapError ("array set: " <>) $ do
            e <- pop
            a <- pop `arrayOnly` "array expected"
            unless (ix k `has` a) $ throwError "index out of bounds"
            push (ArrayR $ a & ix k .~ e)  -- FIXME: ololo bad
        Label{}     -> step Nop
        Jmp lid     -> do
            ensureStackSize 0 "jump"
            (esIp .= ) =<< getLabel (CLabel lid)
        JmpIf lid   -> do
            cond <- pop `valueOnly` "If on reference"
            when (cond /= 0) $ step (Jmp lid)
        Call (FunSign "read" _) ->
            await >>= maybe (throwError "No input") (push . ValueR)
        Call (FunSign "write" _) ->
            pop `valueOnly` "Can't write reference" >>= yield >> push (ValueR 0)
        Call (FunSign name args) -> do
            stack <- esStack <<%= drop (length args)
            entry <- getLabel (FLabel name)
            let funExecState = ExecState
                    { _esLocals = M.fromList (zip args stack)
                    , _esStack  = []
                    , _esIp     = entry
                    }
            -- run execution with its own `ExecState`, not allowing it to
            -- infulence on our current state
            funEndExecState <-
                hoist (lift . lift) $ execStateC funExecState executeDo

            case _esStack funEndExecState of
                [x]   -> esStack %= (x:)
                other -> throwError $ badStackAtFunEnd other
        Ret        -> lift mzero
        Enter{}    -> throwError "Got into out of nowhere"
        Nop        -> return ()

    push v = esStack %= (v:)
    pop = use esStack >>= \case
        []   -> throwError "Empty stack"
        s:ss -> (esStack .= ss) $> s
    ensureStackSize size reason = do
        st <- use esStack
        when (length st /= size) . throwError $
            sformat ("Stack has size "%int%", but expected to have "%
                            int%" instead, reason: "%string)
                           (length st) size reason
    badStackAtFunEnd = sformat ("Not 1 argument on function end: "%shown)
    getLabel :: MonadError Text m => LabelId -> m IP
    getLabel = buildLabelsMap insts
    programEntry =
        either (error "No entry point exists") id $
        getLabel (FLabel initFunName)

buildLabelsMap :: MonadError Text m => Insts -> LabelId -> m IP
buildLabelsMap (V.toList -> insts) =
    let addLabel (idx, Label li) = M.insert li idx
        addLabel _               = id
        labelsMap = foldr addLabel M.empty $ zip [0..] insts
    in  \labelId -> (labelsMap ^. at labelId) `whenNothing`
                        throwError (sformat ("No label "%build) labelId)
