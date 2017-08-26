{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Toy.Execution.Exec
    ( Executable (..)
    , BinaryFile (..)
    ) where

import           Control.Lens        (_Left)
import           Control.Monad.Catch (SomeException, try)
import           Data.Conduit        (ConduitM, ($$), ($=))
import qualified Data.Conduit.List   as C
import qualified Data.Text.Lazy      as LT
import           GHC.Exts            (IsString (..))
import           System.Process      (readProcess)
import           Universum

import           Toy.Base            (Value, getOutputValues)
import           Toy.Execution.Data  (In, InOut, withEmptyInput)
import qualified Toy.Lang            as L
import qualified Toy.SM              as SM
import           Toy.Util            (parseData)


class Executable e where
    exec :: e -> In -> ExceptT Text IO InOut

condExec :: Monad m => ConduitM Value Value m () -> In -> m InOut
condExec ex input =
    C.sourceList input $$ do
        out   <- ex $= C.consume
        remIn <- C.take 100
        -- ^ don't take too much for the sake of infinite inputs
        return (remIn, out)

instance Executable L.Program where
    exec = condExec . L.execute

instance Executable L.Stmt where
    exec = exec . L.toProgram

instance Executable SM.Insts where
    exec = condExec . SM.execute

newtype BinaryFile = BinaryFile FilePath
    deriving (Show, Eq, IsString)

instance Executable BinaryFile where
    exec (BinaryFile path) is = do
        let input = LT.unlines (show <$> is)
        -- TODO: extract errors
        output <- grab $ readProcess path [] (toString input)
        ExceptT . return $
            withEmptyInput . getOutputValues <$> parseData (toText output)
      where
        showError = show :: SomeException -> Text
        grab = ExceptT . fmap (_Left %~ showError) . try

