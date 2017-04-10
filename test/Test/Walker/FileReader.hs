{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}

module Test.Walker.FileReader
    ( FileReader
    , Reads (..)
    , runFileReader
    , readFile
    ) where

import           Control.Applicative       (Alternative)
import           Control.Lens              (at, makeLenses, (.=))
import           Control.Monad             (MonadPlus)
import           Control.Monad.Catch       (MonadCatch, MonadThrow, SomeException,
                                            handleAll)
import           Control.Monad.Reader      (ReaderT, ask, runReaderT)
import           Control.Monad.State       (StateT, mzero, runStateT)
import           Control.Monad.Trans       (MonadIO (..), lift)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Default              (Default (..))
import qualified Data.Map                  as M
import           Data.Text                 (Text)
import qualified Data.Text.IO              as TIO
import           Prelude                   hiding (readFile)

data Reads = Reads
    { _readFails   :: M.Map FilePath (Maybe SomeException)
    , _readSuccess :: Bool
    }
makeLenses ''Reads

instance Show Reads where
    show Reads{..} = flip foldMap (M.toList _readFails) $ \(path, me) ->
        let outcome = maybe "exists" show me
        in  show path ++ " -> " ++ outcome ++ "\n"

instance Default Reads where
    def = Reads def False

newtype FileReader b a = FileReader (ReaderT (b -> FilePath) (MaybeT (StateT Reads IO)) a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch,
              Alternative, MonadPlus)

runFileReader :: FileReader b a -> (b -> FilePath) -> IO (Either Reads a)
runFileReader (FileReader fr) dataIdToPath = do
    (mres, fileReads) <- runStateT (runMaybeT (runReaderT fr dataIdToPath)) def
    return $ case mres of
        Nothing -> Left fileReads
        Just x  -> Right x

readFile :: b -> FileReader b Text
readFile name = FileReader $ do
    path <- ($ name) <$> ask
    let onSuccess = do
            lift . lift $ readSuccess .= True
            readFails . at path .= Just Nothing
        handler e = readFails . at path .= Just (Just e) >> mzero
    handleAll handler $ liftIO (TIO.readFile path) <* onSuccess
