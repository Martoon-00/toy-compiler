module Main
    ( main
    ) where

import           Control.Lens               (view, (%~), (&))
import           Control.Monad.Trans.Either (EitherT (..))
import           Data.Maybe                 (fromMaybe)
import qualified Data.Text.IO               as T
import qualified Data.Text.Lazy             as LT
import qualified Data.Text.Lazy.IO          as LT
import           Prelude                    hiding (interact)
import           System.Environment         (getArgs, lookupEnv)
import           System.FilePath.Lens       (basename, filename)
import           Universum                  (whenLeftM)

import           Toy.Execution              (In, InOut, exec)
import qualified Toy.Lang                   as L
import           Toy.Util                   (parseData)
import qualified Toy.X86                    as X86

main :: IO ()
main = getArgs >>= launch

launch :: [String] -> IO ()
launch [mode, inputFile] = do
    prog <- either parseError id . parseData <$> T.readFile inputFile
    case mode of
        "-i" -> interact $ exec prog
        "-s" -> interact $ exec (L.toIntermediate prog)
        "-o" -> do
            let insts      = X86.compile $ L.toIntermediate prog
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

interact :: (In -> EitherT String IO InOut) -> IO ()
interact executor = do
    stdin <- LT.getContents
    let input = read . LT.unpack <$> LT.words stdin
    eres <- runEitherT $ executor input
    case eres of
        Left err          -> error $ "Execution failed: " ++ err
        Right (_, output) -> putStrLn . unlines $ show <$> output
