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

module Test.Execution.Exec
    ( Executable (..)
    , BinaryFile (..)
    ) where

import           Control.Lens               ((%~), _Left)
import           Control.Monad.Catch        (SomeException, try)
import           Control.Monad.Trans.Either (EitherT (..))
import qualified Data.Map                   as M
import qualified Data.Text                  as T
import           GHC.Exts                   (IsString (..))
import           System.Process             (readProcess)

import           Test.Execution.Data        (In, InOut, withEmptyInput)
import           Test.Util                  (getOutputValues, parseDataOrFail)
import qualified Toy.Lang                   as L
import qualified Toy.SM                     as SM


class Executable e where
    exec :: e -> In -> EitherT String IO InOut

instance Executable L.Stmt where
    exec stmt is =
        let outcome = L.execute $ L.ExecState is [] M.empty stmt
        in  EitherT . return $ L.getIO <$> outcome

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
