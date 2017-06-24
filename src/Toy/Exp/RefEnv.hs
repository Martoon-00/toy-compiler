{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Contains monads which provide environment for references counting and
-- remembering.

module Toy.Exp.RefEnv
    ( MRef
    , MRefId (..)

    , MonadRefEnv (..)
    , NoGcEnv (..)
    ) where

import           Control.Monad.Error.Class  (MonadError)
import qualified Control.Monad.State.Strict as St
import           Control.Monad.Trans        (MonadTrans)
import           Control.Monad.Writer       (MonadWriter, WriterT)
import           Data.Conduit               (ConduitM)
import           Data.IORef                 (IORef)
import qualified Prelude
import           Universum

-- | Innard beyond reference type.
type MRef = IORef

-- | Allows to destinguish different 'MRef's.
newtype MRefId = MRefId Int
    deriving (Eq, Ord)

instance Show MRefId where
    show (MRefId id) = "#" <> show id

-- | Defines how (and whether) to perform references counting and remember
-- allocated values.
class Monad m =>
      MonadRefEnv exp m where
    newMRef :: (MRefId -> IO exp) -> m exp
    default newMRef
        :: (MonadRefEnv exp n, MonadTrans t, t n ~ m)
        => (MRefId -> IO exp) -> m exp
    newMRef = lift . newMRef

    changeRefCounter
        :: (forall a. Num a =>
                          a -> a -> a)
        -> exp
        -> m ()
    default changeRefCounter
        :: ( MonadRefEnv exp n
           , MonadTrans t
           , t n ~ m
           ) =>
        (forall a. Num a =>
                       a -> a -> a) -> exp -> m ()
    changeRefCounter e = lift . changeRefCounter e

    checkNoRefs :: Proxy exp -> m ()
    default checkNoRefs
        :: (MonadRefEnv exp n, MonadTrans t, t n ~ m) => Proxy exp -> m ()
    checkNoRefs = lift . checkNoRefs


instance MonadRefEnv e m => MonadRefEnv e (ReaderT __ m) where
instance MonadRefEnv e m => MonadRefEnv e (StateT __ m) where
instance MonadRefEnv e m => MonadRefEnv e (St.StateT __ m)
instance (MonadRefEnv e m, Monoid w) => MonadRefEnv e (WriterT w m)
instance MonadRefEnv e m => MonadRefEnv e (ExceptT __ m)
instance MonadRefEnv e m => MonadRefEnv e (MaybeT m)
instance MonadRefEnv e m => MonadRefEnv e (ConduitM __ __ m)


-- | Implementation which doesn't perform any garbage collection.
newtype NoGcEnv m a = NoGcEnv
    { getNoGcEnv :: m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadReader __
               , MonadState __
               , MonadWriter __
               , MonadError __
               )

instance MonadTrans NoGcEnv where
    lift = NoGcEnv

instance MonadIO m => MonadRefEnv exp (NoGcEnv m) where
    newMRef = liftIO . ($ MRefId 0)
    changeRefCounter _ _ = return ()
    checkNoRefs _ = return ()

