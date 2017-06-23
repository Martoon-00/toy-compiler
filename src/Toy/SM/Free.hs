{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE ViewPatterns #-}

-- | Keeps logic of our pure GC.

module Toy.SM.Free
    ( insertDeallocations

    , ValueKind (..)
    , getVarKind
    ) where

import           Control.Lens              ((%=), (.=))
import           Control.Monad             (replicateM_)
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.State       (StateT, runStateT)
import           Data.Char                 (isLower, isUpper)
import           Data.Conduit              (ConduitM, awaitForever, yield, ($$), (=$=))
import qualified Data.Conduit.List         as CL
import           Universum

import           Toy.Base                  (FunSign (..), Var (..))
import           Toy.SM.Data               (Inst (..))
import           Toy.Util.Error            (mapError)

data ValueKind
    = Primitive
    | Reference
    | UnknownKind
    deriving (Eq)

type KindsStack = [ValueKind]

type TransState =
    ConduitM Inst Inst $
    StateT KindsStack $
    Either Text

insertDeallocations :: [Inst] -> Either Text [Inst]
insertDeallocations insts = mapError ("deallocations" <> ) $ do
    (res, st) <- flip runStateT [] $
        CL.sourceList insts =$= insertDeallocationsDo $$ CL.consume
    case st of
        [] -> return res
        _  -> throwError "not empty stack at end"


insertDeallocationsDo :: TransState ()
insertDeallocationsDo = awaitForever $ \inst -> process inst >> yield inst
  where
    process = \case
        Push _  -> push Primitive
        Drop    -> kill_
        Dup     -> replicateM_ 2 . push =<< pop_
        Bin _   -> replicateM_ 2 (kill Primitive) >> push Primitive
        Load v  -> push (getVarKind v)
        Store v -> kill (getVarKind v)
        ArrayMake _ -> push Reference
        ArrayAccess -> do
            kill Primitive
            kill Reference
            push UnknownKind
        ArraySet -> do
            kill UnknownKind
            kill Primitive
            kill Reference
        Label _ -> pass
        Jmp _   -> pass
        JmpIf _ -> pass
        Call (FunSign _ args) -> do
            forM_ args $ kill . getVarKind
            push UnknownKind
        Nop -> pass
        JumpToFunEnd -> pass
        Enter _ _ -> pass

    -- 'KindsStack' manipulation
    push k = identity %= (k :)

    pop expected = use identity >>= \case
        []   -> throwError "empty stack"
        x:xs -> do
            identity .= xs
            mergeValueKinds expected x `whenNothing` kindContradiction
    pop_ = pop UnknownKind

    kill k = pop k >>= free
    kill_ = kill UnknownKind

    free Reference   = yield undefined -- Free
    free Primitive   = return ()
    free UnknownKind = return ()  -- such behaviour is still expected :(

    kindContradiction =
        throwError "Contradiction in value kind (treated as primitive and \
                   \reference at the same time)"

mergeValueKinds :: ValueKind -> ValueKind -> Maybe ValueKind
mergeValueKinds a b = case (a, b) of
    _                | a == b       -> pure a
    (UnknownKind, x) -> pure x
    (x, UnknownKind) -> pure x
    _                -> Nothing

getVarKind :: Var -> ValueKind
getVarKind (Var (toString -> n:_))
    | isLower n = Primitive
    | isUpper n = Reference
    | otherwise = error "Lil strange first variable letter"
getVarKind _ =
    error "Lalki, empty variable!"

