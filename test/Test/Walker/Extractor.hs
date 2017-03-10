{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Walker.Extractor
    ( Parsable (..)
    , TestCaseData (..)
    , TestWalker (..)
    , file
    , readingFiles
    , describeDir
    ) where

import           Control.Monad              (forM_, when)
import           Control.Monad.Catch        (Exception, throwM)
import           Control.Monad.Trans        (MonadIO (..), lift)
import           Control.Monad.Trans.Either (EitherT (..))
import           Data.Text                  (Text)
import           Prelude                    hiding (readFile)
import           System.Directory           (doesDirectoryExist, doesFileExist,
                                             listDirectory)
import           System.FilePath.Posix      ((</>))
import           Test.Hspec                 (SpecWith, describe, it)
import           Test.QuickCheck            (Property, once)

import           Test.Util                  ()
import           Test.Walker.FileReader     (FileReader, Reads (..), readFile,
                                             runFileReader)

class Parsable e where
    parseData :: Text -> Either String e

class Show d => TestCaseData d where
    tryGetTestCaseData :: FilePath -> IO (Either Reads (Either String d))

newtype WalkingError = WalkingError String
    deriving (Show, Eq)

instance Exception WalkingError

walk :: forall d . TestCaseData d
     => FilePath -> (Either String d -> Property) -> SpecWith ()
walk path apply = do
    validPath <- liftIO $
        (||) <$> doesDirectoryExist path <*> doesFileExist path
    when (not validPath) $
        liftIO $ throwM $ WalkingError $ "Invalid path: " ++ show path

    edata <- liftIO $ tryGetTestCaseData path
    case edata of
        Left r -> do
            when (_readSuccess r) $
                liftIO $ putStrLn $ "Incomplete test data!\n" ++ show r ++ "\n"
        Right d -> it "🐱" $ apply d

    contents <- liftIO $ listDirectory path
    forM_ contents $ \dirname -> do
        let dir = path </> dirname
        exists <- liftIO $ doesDirectoryExist dir
        when exists $ describe dirname $ walk dir apply

data TestWalker d = TestWalker
    { twRoot  :: FilePath
    , twApply :: Either String d -> Property
    }

describeDir
    :: TestCaseData d
    => FilePath -> (Either String d -> Property) -> SpecWith ()
describeDir path apply =
    describe path $ walk path $ once . apply

file :: Parsable a => FilePath -> EitherT String FileReader a
file path = do
    rd <- lift $ readFile path
    EitherT . return $ parseData rd

readingFiles
    :: EitherT String FileReader a
    -> FilePath
    -> IO (Either Reads (Either String a))
readingFiles = runFileReader . runEitherT
