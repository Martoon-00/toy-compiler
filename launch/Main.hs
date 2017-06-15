module Main
    ( main
    ) where

import           Control.Lens               ((&))
import           Control.Monad              (forever)
import           Control.Monad.Trans        (liftIO)
import           Control.Monad.Trans.Either (EitherT (..))
import           Data.Conduit               (awaitForever, yield, ($$), (=$=))
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text.IO               as T
import           Formatting                 (sformat, string, (%))
import           GHC.IO.Handle              (hFlush)
import           GHC.IO.Handle.FD           (stdout)
import           Prelude                    (readLn)
import           System.Environment         (lookupEnv)
import           System.FilePath.Lens       (basename, filename)
import           Universum                  hiding (interact)

import           Toy.Base                   (Exec)
import qualified Toy.Lang                   as L
import qualified Toy.SM                     as SM
import           Toy.Util                   (parseData)
import qualified Toy.X86                    as X86

main :: IO ()
main = getArgs >>= launch

launch :: [String] -> IO ()
launch [mode, inputFile] = do
    prog <- either parseError identity . parseData <$> T.readFile inputFile
    case mode of
        "-i" -> interact $ L.execute prog
        "-s" -> interact $ SM.execute $ either error identity (L.toIntermediate prog)
        "-o" -> do
            let insts      = X86.compile $ either error identity $ L.toIntermediate prog
                outputPath = inputFile & filename %~ view basename
            runtimePath <- fromMaybe "./runtime" <$> lookupEnv "RC_RUNTIME"
            X86.produceBinary runtimePath outputPath insts
                `whenLeftM` \err -> error $ "Compilation error: " <> err
        other -> error $ sformat ("Unrecognised mode: "%string) other
  where
    parseError err = error $ "Parse error: " <> err

launch _ = putStrLn $ unlines
    [ "Usage:"
    , "  1) mode: -i / -s / -o"
    , "  2) path to file with program"
    ]

interact :: Exec IO () -> IO ()
interact executor = handleRes $ readInput =$= executor $$ writeOutput
  where
    readInput        = forever $ readValue >>= yield
    readValue        = liftIO $ putText "> " >> hFlush stdout >> readLn
    writeOutput      = awaitForever $ liftIO . print
    handleRes action = runEitherT action `whenLeftM` printError
    printError err   = error $ "Execution failed: " <> err
