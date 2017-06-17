{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Utils to work with arrays

module Toy.Exp.Arrays where

import           Control.Lens              (at, zoom, (.=))
import           Control.Monad.Error.Class (MonadError (..))
import           Control.Monad.Trans       (MonadTrans)
import           Control.Monad.Writer      (MonadWriter (..))
import qualified Data.Set                  as S
import qualified Data.Vector               as V
import           Universum

import           Toy.Base                  (Value)
import           Toy.Exp.Data
import           Toy.Exp.RefEnv            (MRef, MRefId, MRefsGenerator,
                                            MonadRefEnv (..), MonadRefInit (..),
                                            runRefsGenerator)

initArray
    :: (MonadIO m, MonadRefInit m, MonadRefEnv ExpRes m)
    => V.Vector ExpRes -> m ExpRes
initArray v = do
    mRefId <- newMRefId
    res <- ArrayR <$> newIORef (Just $ ArrayInnards 0 mRefId v)
    void . runExceptT $ changeRefCounter (+) res
    return res

valueOnly
    :: (IsString s, MonadError s m)
    => m ExpRes -> Text -> m Value
valueOnly action (fromString . toString -> desc) =
    maybe (throwError desc) pure . preview _ValueR =<< action

arrayShell
    :: (IsString s, MonadError s m)
    => m ExpRes -> Text -> m (MRef (Maybe ArrayInnards))
arrayShell action (fromString . toString -> desc) =
    maybe (throwError desc) pure . preview _ArrayR =<< action

arrayOnlyM
    :: (IsString s, MonadError s m, MonadIO m)
    => m ExpRes -> Text -> State (V.Vector ExpRes) a -> m a
arrayOnlyM action desc modifier = do
    ref <- arrayShell action desc
    innard <- readIORef ref `whenNothingM` throwError "Array was freed"
    let (res, innard') = runState (zoom aiArray modifier) innard
    writeIORef ref (Just innard')
    return res

arrayOnly
    :: (IsString s, MonadError s m, MonadIO m)
    => ExpRes -> Text -> State (V.Vector ExpRes) a -> m a
arrayOnly value = arrayOnlyM (pure value)


newtype RefCountingGc m a = RefCountingGc
    { getRefCountingGc :: StateT (S.Set MRefId) m a
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
runRefCountingGc = flip evalStateT mempty . getRefCountingGc

instance (MonadIO m, MonadError s m, IsString s) =>
         MonadRefEnv ExpRes (RefCountingGc m) where
    changeRefCounter modifier expr =
        whenJust (expr ^? _ArrayR) $ \ref -> do
            innard <- readIORef ref `whenNothingM` throwError "Array was freed"
            let innard' = innard & aiRefCounter %~ (`modifier` 1)
            let newRefStateM = guard $ _aiRefCounter innard' /= 0
            liftIO $ writeIORef ref $ newRefStateM $> innard'

            RefCountingGc $ identity . at (innard ^. aiRefId) .= newRefStateM
            when (innard' ^. aiRefCounter == 0) $
                mapM_ (changeRefCounter (-)) (innard ^. aiArray)

runWholeRefCountingGc :: Monad m => (MRefsGenerator $ RefCountingGc m) a -> m a
runWholeRefCountingGc = runRefCountingGc . runRefsGenerator
