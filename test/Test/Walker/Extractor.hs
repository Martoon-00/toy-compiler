{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.Walker.Extractor
    ( TestCaseData (..)
    , TestWalker (..)
    , file
    , readWithExtension
    , describeDir
    ) where

import           Control.Lens               ((%~), (^.), _Left)
import           Control.Monad              (forM, forM_, when)
import           Control.Monad.Catch        (Exception, throwM)
import           Control.Monad.Trans        (MonadIO (..), lift)
import           Control.Monad.Trans.Either (EitherT (..))
import           Data.List                  (sort)
import qualified Data.Set                   as S
import           Prelude                    hiding (readFile)
import           System.Directory           (doesDirectoryExist, doesFileExist,
                                             listDirectory)
import           System.FilePath.Lens       (basename)
import           System.FilePath.Posix      ((</>))
import           Test.Hspec                 (SpecWith, describe, it)
import           Test.QuickCheck            (Property, once, property)
import           Test.QuickCheck.Property   (failed, reason)

import           Test.Util                  (Parsable (..))
import           Test.Walker.FileReader     (FileReader, Reads (..), readFile,
                                             runFileReader)

class Show d => TestCaseData d where
    tryGetTestCaseData :: FilePath -> String -> IO (Either Reads (Either String d))

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

    contents <- liftIO $ sort <$> listDirectory path
    filenames <- fmap unique $ forM contents $ \filename -> do
        let file' = path </> filename
        exists <- liftIO $ doesFileExist file'
        return $ if exists then [filename ^. basename] else []
    forM_ filenames collectTestCase

    forM_ contents $ \dirname -> do
        let dir = path </> dirname
        exists <- liftIO $ doesDirectoryExist dir
        when exists $ describe dirname $ walk dir apply
  where
    unique = S.toList . mconcat . map S.fromList

    collectTestCase basename' = do
        edata <- liftIO $ tryGetTestCaseData path basename'
        case edata of
            Left r -> do
                when (_readSuccess r) $
                    it basename' $ property $
                        failed { reason = "Incomplete test data!\n"
                                 ++ show r ++ "\n" }
            Right d -> it basename' $ apply d

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
    EitherT . return $ (_Left %~ withDesc) $ parseData rd
  where
    withDesc err = "(" ++ path ++ ") " ++ err

readWithExtension
    :: EitherT String FileReader a
    -> FilePath
    -> String
    -> IO (Either Reads (Either String a))
readWithExtension action path basename' =
    runFileReader (runEitherT action) (\ext -> path </> basename' ++ ext)
