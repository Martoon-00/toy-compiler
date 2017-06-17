{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE UndecidableInstances       #-}

-- | Contains monads which provide environment for references counting and
-- remembering.

module Toy.Exp.RefEnv
    ( MRef
    , MRefId
    , MonadRefInit (..)
    , MRefsGenerator
    , runRefsGenerator

    , MonadRefEnv (..)
    , NoGcEnv (..)
    ) where

import           Control.Lens               ((<<+=))
import           Control.Monad.Error.Class  (MonadError)
import qualified Control.Monad.State.Strict as St
import           Control.Monad.Trans        (MonadTrans)
import           Control.Monad.Writer       (MonadWriter, WriterT)
import           Data.Conduit               (ConduitM)
import           Data.IORef                 (IORef)
import           Universum

-- | Innard beyond reference type.
type MRef = IORef

-- | Allows to destinguish different 'MRef's.
newtype MRefId = MRefId Int
    deriving (Show, Eq, Ord)


class Monad m => MonadRefInit m where
    newMRefId :: m MRefId
    default newMRefId
        :: (MonadRefInit n, MonadTrans t, t n ~ m)
        => m MRefId
    newMRefId = lift newMRefId

newtype MRefsGenerator m a = MRefsGenerator
    { getMRefsGenerator :: StateT Int m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadTrans
               , MonadIO
               , MonadReader __
               , MonadWriter __
               , MonadError __
               , MonadRefEnv __
               )

runRefsGenerator :: Monad m => MRefsGenerator m a -> m a
runRefsGenerator = flip evalStateT 0 . getMRefsGenerator

instance MonadRefInit m => MonadRefInit (ReaderT __ m)
instance MonadRefInit m => MonadRefInit (StateT __ m)
instance MonadRefInit m => MonadRefInit (St.StateT __ m)
instance (MonadRefInit m, Monoid w) => MonadRefInit (WriterT w m)
instance MonadRefInit m => MonadRefInit (ExceptT __ m)
instance MonadRefInit m => MonadRefInit (MaybeT m)
instance MonadRefInit m => MonadRefInit (ConduitM __ __ m)

instance MonadState s m => MonadState s (MRefsGenerator m) where
    state = lift . state

instance Monad m => MonadRefInit (MRefsGenerator m) where
    newMRefId = MRefsGenerator . fmap MRefId $ identity <<+= 1


-- | Defines how (and whether) to perform references counting and remember
-- allocated values.
class Monad m =>
      MonadRefEnv exp m where
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
                       a -> a -> a) -> exp -> t n ()
    changeRefCounter e = lift . changeRefCounter e


instance MonadRefEnv e m => MonadRefEnv e (ReaderT __ m)
instance MonadRefEnv e m => MonadRefEnv e (StateT __ m)
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

instance Monad m => MonadRefInit (NoGcEnv m) where
    newMRefId = return (MRefId 0)

instance Monad m => MonadRefEnv exp (NoGcEnv m) where
    changeRefCounter _ _ = return ()

