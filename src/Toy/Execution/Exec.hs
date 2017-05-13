{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Toy.Execution.Exec
    ( Executable (..)
    , BinaryFile (..)
    ) where

import           Control.Lens               ((%~), _Left)
import           Control.Monad.Catch        (SomeException, try)
import           Control.Monad.Trans.Either (EitherT (..))
import           Data.Conduit               (ConduitM, ($$), ($=))
import qualified Data.Conduit.List          as C
import qualified Data.Text                  as T
import           GHC.Exts                   (IsString (..))
import           System.Process             (readProcess)

import           Toy.Execution.Data         (In, InOut, withEmptyInput)
import           Toy.Exp                    (Value)
import qualified Toy.Lang                   as L
import qualified Toy.SM                     as SM
import           Toy.Util                   (getOutputValues, parseDataOrFail)


class Executable e where
    exec :: e -> In -> EitherT String IO InOut

condExec :: Monad m => ConduitM Value Value m () -> In -> m InOut
condExec ex input =
    C.sourceList input $$ do
        out   <- ex $= C.consume
        remIn <- C.consume
        return (remIn, out)

instance Executable L.Program where
    exec = condExec . L.execute

instance Executable L.Stmt where
    exec = exec . L.Program mempty

instance Executable SM.Insts where
    exec = condExec . SM.execute

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
