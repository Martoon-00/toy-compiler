{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Utils to work with arrays

module Toy.Exp.Arrays where

import           Control.Lens              (at, has, makeLenses, zoom, (.=), (<<+=))
import           Control.Monad.Error.Class (MonadError (..))
import           Control.Monad.Trans       (MonadTrans)
import           Control.Monad.Writer      (MonadWriter (..))
import           Data.Default              (Default (..))
import qualified Data.Map                  as M
import qualified Data.Vector               as V
import           Formatting                (bprint, formatToString, shown, stext, (%))
import           Serokell.Util.Text        (listJson)
import           Universum

import           Toy.Base                  (Value)
import           Toy.Exp.Data
import           Toy.Exp.RefEnv            (MRef, MRefId (..), MonadRefEnv (..))

type MonadArrays s m =
    ( MonadIO m
    , MonadError s m
    , IsString s
    , MonadRefEnv ExpRes m
    )

initArray
    :: MonadArrays __ m
    => V.Vector ExpRes -> m ExpRes
initArray v = do
    res <- newMRef $ \mRefId ->
        ArrayR <$> newIORef (Just $ ArrayInnards 0 mRefId v)
    void . runExceptT $ changeRefCounter (+) res
    return res

valueOnly
    :: MonadArrays __ m
    => m ExpRes -> Text -> m Value
valueOnly action desc = do
    value <- action
    when (_NotInitR `has` value) $
        throwError . fromString $
        formatToString ("Not initialized variable! ("%stext%")") desc
    preview _ValueR value `whenNothing` throwError (fromString $ toString desc)

labelOnly
    :: MonadArrays __ m
    => m ExpRes -> Text -> m UserLabelId
labelOnly action desc = do
    value <- action
    when (_NotInitR `has` value) $
        throwError . fromString $
        formatToString ("Not initialized variable! ("%stext%")") desc
    preview _LabelR value `whenNothing` throwError (fromString $ toString desc)

arrayShell
    :: MonadArrays __ m
    => m ExpRes -> Text -> m (MRef (Maybe ArrayInnards))
arrayShell action desc = do
    value <- action
    when (_NotInitR `has` value) $
        throwError . fromString $
        formatToString ("Not initialized variable! ("%stext%")") desc
    preview _ArrayR value `whenNothing` throwError (fromString $ toString desc)

arrayOnlyM
    :: MonadArrays __ m
    => m ExpRes -> Text -> State (V.Vector ExpRes) a -> m a
arrayOnlyM action desc modifier = do
    ref <- arrayShell action desc
    innard <- readIORef ref `whenNothingM` throwError "Array was freed"
    let (res, innard') = runState (zoom aiArray modifier) innard
    writeIORef ref (Just innard')
    return res

arrayOnly
    :: MonadArrays __ m
    => ExpRes -> Text -> State (V.Vector ExpRes) a -> m a
arrayOnly value = arrayOnlyM (pure value)


data RefCountingGcState = RefCountingGcState
    { _rcdsExistingRefs :: M.Map MRefId (MRef (Maybe ArrayInnards))
    , _rcdsRefsCounter  :: Int
    }

makeLenses ''RefCountingGcState

instance Default RefCountingGcState where
    def = RefCountingGcState def def

newtype RefCountingGc m a = RefCountingGc
    { getRefCountingGc :: StateT RefCountingGcState m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadTrans
               , MonadIO
               , MonadReader __
               , MonadWriter __
               , MonadError __
               )

runRefCountingGc :: Monad m => RefCountingGc m a -> m a
runRefCountingGc = flip evalStateT def . getRefCountingGc

instance (MonadIO m, MonadError s m, IsString s) =>
         MonadRefEnv ExpRes (RefCountingGc m) where
    newMRef mk = do
        mRefId <- RefCountingGc (rcdsRefsCounter <<+= 1)
        liftIO $ mk (MRefId mRefId)

    changeRefCounter modifier expr =
        whenJust (expr ^? _ArrayR) $ \ref -> do
            innard <- readIORef ref `whenNothingM` throwError "Array was freed"
            let innard' = innard & aiRefCounter %~ (`modifier` 1)
            let newRefStateM = guard $ _aiRefCounter innard' /= 0
            liftIO $ writeIORef ref $ newRefStateM $> innard'

            RefCountingGc $
                rcdsExistingRefs . at (innard ^. aiRefId) .= (newRefStateM $> ref)
            when (innard' ^. aiRefCounter < 0) $
                throwError "Reference counter got negative!"
            when (innard' ^. aiRefCounter == 0) $
                mapM_ (changeRefCounter (-)) (innard ^. aiArray)

    checkNoRefs _ = RefCountingGc $ do
        existingRefs <- toList <$> use rcdsExistingRefs
        unless (null existingRefs) $ do
            values <- forM existingRefs $ \a ->
                readIORef a `whenNothingM` throwError "Primitive in refs map??"
            throwError . fromString $
                formatToString ("References remained:\n"%listJson)
                (bprint (shown%"\n") <$> values)

checkNoExpResRefs :: MonadRefEnv ExpRes m => m ()
checkNoExpResRefs = checkNoRefs (Proxy @ExpRes)

runWholeRefCountingGc :: Monad m => RefCountingGc m a -> m a
runWholeRefCountingGc = runRefCountingGc
