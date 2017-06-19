module Toy.SM.Interpreter
    ( execute
    ) where

import           Control.Lens              (at, (%=), (+=), (.=), (<<%=), (?=))
import           Control.Monad             (forever, mzero, replicateM, replicateM_, void,
                                            when)
import           Control.Monad.Error.Class (MonadError (..))
import           Control.Monad.Morph       (hoist)
import           Control.Monad.Trans       (MonadIO (..))
import           Control.Monad.Trans       (lift)
import           Data.Conduit              (await, yield)
import           Data.Conduit.Lift         (evalStateC, execStateC, runMaybeC)
import           Data.Default              (def)
import           Data.Functor              (($>))
import qualified Data.Map                  as M
import qualified Data.Vector               as V
import           Formatting                (build, int, sformat, shown, string, (%))
import           Universum

import           Toy.Base                  (Exec, FunSign (..))
import           Toy.Exp                   (ExpRes (..), arithspoon, arrayAccess,
                                            arrayFree, arrayLength, arrayMake, arrayMakeU,
                                            arraySet, binOp, changeRefCounter,
                                            checkNoExpResRefs, runWholeRefCountingGc,
                                            valueOnly)
import           Toy.SM.Data               (ExecState (..), IP, Inst (..), Insts,
                                            LabelId (..), esIp, esLocals, esStack,
                                            initFunName)
import           Toy.Util.Error            (mapError)

execute :: MonadIO m => Insts -> Exec m ()
execute insts =
    hoist runWholeRefCountingGc $
    evalStateC def{ _esIp = programEntry } $
    executeDo
  where
    executeDo = void . runMaybeC . forever $ do
        i <- use esIp
        inst <- insts V.!? i `whenNothing` throwError "To infinity and beyond! >>>"
        mapError (sformat ("At "%build%" ("%shown%"): "%build) i inst) $
            step inst
        esIp += 1

    step = \case
        Push v      -> push (ValueR v)
        Drop        -> void pop
        Dup         -> replicateM_ 2 . push =<< pop
        Bin op      -> do
            let lil = "Arithmetic operation on reference"
            [b, a] <- replicateM 2 (pop `valueOnly` lil)
            push . ValueR =<< arithspoon (binOp op a b)
        Load n      -> use (esLocals . at n) >>= \case
            Nothing  -> push NotInitR
            Just var -> push var >> changeRefCounter (+) var
        Store n     -> do
            whenJustM (use $ esLocals . at n) $
                changeRefCounter (-)
            pop >>= (esLocals . at n ?= )
        ArrayMake k -> arrayMakeU k >>= push
        ArrayAccess -> do
            i <- pop
            a <- pop
            e <- arrayAccess a i
            push e
        ArraySet -> do
            e <- pop
            i <- pop
            a <- pop
            arraySet a i e
        Label{}     -> step Nop
        Jmp lid     -> do
            ensureStackSize 0 "jump"
            (esIp .= ) =<< getLabel lid
        JmpIf lid   -> do
            cond <- pop `valueOnly` "If on reference"
            when (cond /= 0) $ step (Jmp lid)
        Call (FunSign name args) -> do
            processCall name args
        Ret        -> lift mzero
        Enter{}    -> throwError "Got into out of nowhere"
        Nop        -> return ()
        Free       -> do
            a <- pop
            arrayFree a

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
        either (error "No entry point exists") identity $
        getLabel (FLabel initFunName)

    processCall name args = case name of
            "read"  ->
                await >>= maybe (throwError "No input") (push . ValueR)
            "write" -> do
                yield =<< pop `valueOnly` "Can't write reference"
                push (ValueR 0)
            "allocate" -> do
                l <- pop
                a <- arrayMake l (ValueR 0)
                push a
            "free" -> do
                a <- pop
                arrayFree a
                push (ValueR 0)
            "arrlen" -> do
                a <- pop
                l <- arrayLength a
                push l
            "arrmake" -> do
                l <- pop
                e <- pop
                a <- arrayMake l e
                push a
            "Arrmake" ->
                step (Call $ FunSign "arrmake" args)
            "ensure_no_allocations" -> do
                checkNoExpResRefs
                push (ValueR 0)
            _ -> do
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

                -- forM_ (_esLocals funEndExecState) $
                --     changeRefCounter (-)
                case _esStack funEndExecState of
                    [x]   -> esStack %= (x:)
                    other -> throwError $ badStackAtFunEnd other


buildLabelsMap :: MonadError Text m => Insts -> LabelId -> m IP
buildLabelsMap (V.toList -> insts) =
    let addLabel (idx, Label li) = M.insert li idx
        addLabel _               = identity
        labelsMap = foldr addLabel M.empty $ zip [0..] insts
    in  \labelId -> (labelsMap ^. at labelId)
            `whenNothing` throwError (sformat ("No label "%build) labelId)
