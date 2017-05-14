module Main
    ( main
    ) where

import           Control.Lens               (view, (%~), (&))
import           Control.Monad              (forever)
import           Control.Monad.Trans        (liftIO)
import           Control.Monad.Trans.Either (EitherT (..))
import           Data.Conduit               (awaitForever, yield, ($$), (=$=))
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text.IO               as T
import           GHC.IO.Handle              (hFlush)
import           GHC.IO.Handle.FD           (stdout)
import           Prelude                    hiding (interact)
import           System.Environment         (getArgs, lookupEnv)
import           System.FilePath.Lens       (basename, filename)
import           Universum                  (whenLeftM)

import           Toy.Exp                    (Exec)
import qualified Toy.Lang                   as L
import qualified Toy.SM                     as SM
import           Toy.Util                   (parseData)
import qualified Toy.X86                    as X86

main :: IO ()
main = getArgs >>= launch

launch :: [String] -> IO ()
launch [mode, inputFile] = do
    prog <- either parseError id . parseData <$> T.readFile inputFile
    case mode of
        "-i" -> interact $ L.execute prog
        "-s" -> interact $ SM.execute $ either error id (L.toIntermediate prog)
        "-o" -> do
            let insts      = X86.compile $ either error id $ L.toIntermediate prog
                outputPath = inputFile & filename %~ view basename
            runtimePath <- fromMaybe "./runtime" <$> lookupEnv "RC_RUNTIME"
            X86.produceBinary runtimePath outputPath insts
                `whenLeftM` \err -> error $ "Compilation error: " ++ err
        other -> error $ "Unrecognised mode: " ++ other
  where
    parseError err = error $ "Parse error: " ++ show err

launch _ = putStrLn $ unlines
    [ "Usage:"
    , "\t1) mode: -i / -s / -o"
    , "\t2) path to file with program"
    ]

interact :: Exec IO () -> IO ()
interact executor = handleRes $ readInput =$= executor $$ writeOutput
  where
    readInput        = forever $ readValue >>= yield
    readValue        = liftIO $ putStr "> " >> hFlush stdout >> readLn
    writeOutput      = awaitForever $ liftIO . print
    handleRes action = runEitherT action `whenLeftM` printError
    printError err   = error $ "Execution failed: " ++ err
