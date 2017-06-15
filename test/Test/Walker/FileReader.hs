{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Test.Walker.FileReader
    ( GatherFile (..)

    , TestFilesPresence (..)
    , testFilesPresence
    , collectFiles

    , LoadedFileGatherer (..)
    , applyFileGatherer
    ) where

import           Control.Applicative        (Const (..))
import           Control.Monad.Reader       (ReaderT (..), runReaderT)
import           Control.Monad.Trans        (MonadIO (..))
import           Control.Monad.Trans.Either (EitherT (..), runEitherT)
import           Data.Default               (Default (..))
import           Data.Text                  (Text)
import qualified Data.Text.IO               as TIO
import           Formatting                 (sformat, shown, stext, (%))
import           Prelude                    hiding (readFile)
import           System.Directory           (doesFileExist)
import           Universum                  (first, (<&>))

import           Toy.Util                   (Parsable, parseData)

class GatherFile m where
    gatherFile :: Parsable a => u -> m u a

type TestFilesLocation u = u -> FilePath


-- * Files presence

newtype TestFilesPresence = TestFilesPresence ([(FilePath, Bool)])
    deriving (Monoid)

instance Default TestFilesPresence where
    def = TestFilesPresence def

instance Show TestFilesPresence where
    show (TestFilesPresence fails) =
        flip foldMap fails $ \(path, exists) ->
            let outcome = if exists then "exists" else "absent"
            in  show path ++ " -> " ++ outcome ++ "\n"

newtype FilesPresenceT u a =
    FilesPresenceT (Const (TestFilesLocation u -> IO TestFilesPresence) a)
    deriving (Functor, Applicative)

testFilesPresence :: FilesPresenceT u a -> TestFilesLocation u -> IO TestFilesPresence
testFilesPresence (FilesPresenceT (Const tc)) loc = tc loc

instance GatherFile FilesPresenceT where
    gatherFile ext = FilesPresenceT $ Const $ \(($ ext) -> path) ->
        doesFileExist path <&> \exists -> TestFilesPresence [(path, exists)]


-- * Files collection

newtype TestCollector u a = TestCollector
    (ReaderT (TestFilesLocation u) (EitherT Text IO) a)
    deriving (Functor, Applicative, Monad, MonadIO)

collectFiles :: TestCollector u a -> TestFilesLocation u -> IO (Either Text a)
collectFiles (TestCollector tc) loc = runEitherT $ runReaderT tc loc

instance GatherFile TestCollector where
    gatherFile ext = TestCollector . ReaderT $ \mkPath -> EitherT $
        let path = mkPath ext
            withDesc = sformat ("("%shown%") "%stext) path
        in  first withDesc . parseData <$> TIO.readFile path


-- * Semi-applied

data LoadedFileGatherer u a = LoadedFileGatherer
    { lfgGatherer :: forall m. (GatherFile m, Applicative (m u)) => m u a
    , lftLocation :: TestFilesLocation u
    }

applyFileGatherer
    :: (GatherFile m, Applicative (m u))
    => (m u a -> TestFilesLocation u -> r) -> LoadedFileGatherer u a -> r
applyFileGatherer f LoadedFileGatherer{..} = f lfgGatherer lftLocation
