{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Toy.Execution.Exec
    ( Executable (..)
    , BinaryFile (..)
    ) where

import           Control.Lens               ((%~), _Left)
import           Control.Monad.Catch        (SomeException, try)
import           Control.Monad.Morph        (generalize, hoist)
import           Control.Monad.State        (evalStateT)
import           Control.Monad.Trans.Either (EitherT (..))
import           Data.Conduit               (($$), ($=))
import qualified Data.Conduit.List          as C
import qualified Data.Text                  as T
import           GHC.Exts                   (IsString (..))
import           System.Process             (readProcess)

import           Toy.Execution.Data         (In, InOut, withEmptyInput)
import qualified Toy.Lang                   as L
import qualified Toy.SM                     as SM
import           Toy.Util                   (getOutputValues, parseDataOrFail)


class Executable e where
    exec :: e -> In -> EitherT String IO InOut

instance Executable L.Stmt where
    exec stmt is = hoist generalize . flip evalStateT mempty $
        C.sourceList is $$ do
            out   <- L.execute stmt $= C.consume
            remIn <- C.consume
            return (remIn, out)

instance Executable SM.Insts where
    exec insts is =
        let outcome = SM.execute insts $ SM.anExecState is
        in  EitherT . return $ SM.getIO <$> outcome

newtype BinaryFile = BinaryFile FilePath
    deriving (Show, Eq, IsString)

instance Executable BinaryFile where
    exec (BinaryFile path) is = do
        let input = unlines (show <$> is)
        -- TODO: extract errors
        output <- grab $ readProcess path [] input
        return . withEmptyInput . getOutputValues . parseDataOrFail $ T.pack output
      where
        showError :: SomeException -> String
        showError = show

        grab = EitherT . fmap (_Left %~ showError) . try
