{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Test.Walker.Extractor
    ( TestCaseData (..)
    , describeDir
    , readTestCase
    , gatherFile

    , Extension
    ) where

import           Control.Lens             (view)
import           Control.Monad            (forM, forM_, when)
import           Control.Monad.Catch      (Exception, throwM)
import           Control.Monad.Trans      (MonadIO (..))
import           Data.List                (sort)
import           Data.Proxy               (Proxy (..))
import           Data.String              (IsString)
import           Prelude                  hiding (readFile)
import qualified System.Console.ANSI      as ANSI
import           System.Directory         (doesDirectoryExist, doesFileExist,
                                           listDirectory)
import           System.FilePath.Lens     (basename)
import           System.FilePath.Posix    ((</>))
import           Test.Hspec               (SpecWith, describe, it)
import           Test.Hspec.QuickCheck    (modifyMaxSuccess)
import           Test.QuickCheck          (Property, ioProperty, property)
import           Test.QuickCheck.Property (failed, reason)
import           Universum                (Text, guard, ($>))

import           Test.Util                ()
import           Test.Walker.FileReader   (GatherFile, LoadedFileGatherer (..),
                                           TestFilesPresence (..), applyFileGatherer,
                                           collectFiles, gatherFile, testFilesPresence)
import           Universum                (ordNub)


type PathConstructor u = CurLocation -> u -> FilePath

class (Show d, HasPathConstructor (PathDiffObj d)) => TestCaseData d where
    type PathDiffObj d :: *
    type PathDiffObj d = ()

    mkTestCollector :: CurLocation -> LoadedFileGatherer (PathDiffObj d) d

-- | Full path to file can be splitted into 3 parts: path, base, difference
-- object.
-- Test cases differ in path and base, while difference object distinguishes
-- files within same test case.
class HasPathConstructor u where
    -- | Make full path from location and difference object
    constructPath :: PathConstructor u

    -- | Get base from name
    extractBase :: Proxy u -> String -> String

instance HasPathConstructor () where
    constructPath CurLocation{..} _ = clPath </> clBase
    extractBase _ = id

newtype Extension = Extension String
    deriving (Show, Eq, IsString)

instance HasPathConstructor Extension where
    constructPath CurLocation{..} (Extension ext) = clPath </> clBase ++ ext
    extractBase _ = view basename

newtype WalkingError = WalkingError String
    deriving (Show, Eq)

instance Exception WalkingError

withColor :: ANSI.Color -> String -> String
withColor sgr str = concat
    [ ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid sgr]
    , str
    , ANSI.setSGRCode [ANSI.Reset]
    ]

data CurLocation = CurLocation
    { clPath :: FilePath
    , clBase :: String
    }

walk :: forall d . TestCaseData d
     => FilePath -> (Either Text d -> Property) -> SpecWith ()
walk path apply = do
    validPath <- liftIO $
        (||) <$> doesDirectoryExist path <*> doesFileExist path
    when (not validPath) $
        liftIO $ throwM $ WalkingError $ "Invalid path: " ++ show path

    contents <- liftIO $ sort <$> listDirectory path
    basenames <- fmap (ordNub . concat) $ forM contents $ \filename -> do
        let file' = path </> filename
        exists <- liftIO $ doesFileExist file'
        return $ guard exists $> extractBase (Proxy @(PathDiffObj d)) filename
    forM_ basenames collectTestCase

    forM_ contents $ \dirname -> do
        let dir = path </> dirname
        exists <- liftIO $ doesDirectoryExist dir
        when exists $ describe (withColor ANSI.Blue dirname) $ walk dir apply
  where
    collectTestCase basename' = do
        let gatherer = mkTestCollector $ CurLocation path basename'
        presence <- liftIO $ applyFileGatherer testFilesPresence gatherer
        let TestFilesPresence presence' = presence
        if | all snd presence' ->
                 modifyMaxSuccess (const 1) . it basename' . ioProperty $
                 apply <$> applyFileGatherer collectFiles gatherer
           | any snd presence' -> do
                 it basename' $ property $
                     failed { reason = "Incomplete test data!\n" ++ show presence ++ "\n" }
           | otherwise -> return ()

describeDir
    :: TestCaseData d
    => FilePath -> (Either Text d -> Property) -> SpecWith ()
describeDir path apply =
    describe (withColor ANSI.Cyan path) $ walk path apply

readTestCase
    :: HasPathConstructor u
    => (forall m. (GatherFile m, Applicative (m u)) =>
                      m u a)
    -> CurLocation
    -> LoadedFileGatherer u a
readTestCase gatherer location =
    LoadedFileGatherer gatherer $ constructPath location
