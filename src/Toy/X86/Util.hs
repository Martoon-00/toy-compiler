module Toy.X86.Util
    ( readCreateProcess
    ) where

import           Control.Concurrent      (forkIO, killThread)
import           Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, takeMVar)
import           Control.DeepSeq         (rnf)
import           Control.Exception       (SomeException, evaluate, handle, mask,
                                          onException, throwIO, try)
import           Control.Monad           (unless)
import           Foreign.C.Error         (Errno (..), ePIPE)
import           GHC.IO.Exception        (IOErrorType (..), IOException (..))
import           System.Exit             (ExitCode (..))
import           System.IO               (hClose, hGetContents, hPutStr)
import           System.Process          (CreateProcess (..), StdStream (..),
                                          waitForProcess, withCreateProcess)

-- | Copy-pasted `readCreateProcess` fnctions, but ignores stdout and
-- extracts stderr.
readCreateProcess
    :: CreateProcess
    -> String                   -- ^ standard input
    -> IO (Either String ())    -- ^ stderr
readCreateProcess cp input = do
    let cp_opts = cp
            { std_in  = CreatePipe
            , std_out = NoStream
            , std_err = CreatePipe
            }
    (ex, output) <- withCreateProcess cp_opts $
      \(Just inh) _ (Just errh) ph -> do

        -- fork off a thread to start consuming the output
        output  <- hGetContents errh
        withForkWait (evaluate $ rnf output) $ \waitOut -> do

          -- now write any input
          unless (null input) $
            ignoreSigPipe $ hPutStr inh input
          -- hClose performs implicit hFlush, and thus may trigger a SIGPIPE
          ignoreSigPipe $ hClose inh

          -- wait on the output
          waitOut
          hClose errh

        -- wait on the process
        ex <- waitForProcess ph
        return (ex, output)

    case ex of
     ExitSuccess   -> return $ Right ()
     ExitFailure _ -> return $ Left output


-- | Fork a thread while doing something else, but kill it if there's an
-- exception.
--
-- This is important in the cases above because we want to kill the thread
-- that is holding the Handle lock, because when we clean up the process we
-- try to close that handle, which could otherwise deadlock.
--
withForkWait :: IO () -> (IO () ->  IO a) -> IO a
withForkWait async body = do
  waitVar <- newEmptyMVar :: IO (MVar (Either SomeException ()))
  mask $ \restore -> do
    tid <- forkIO $ try (restore async) >>= putMVar waitVar
    let wait = takeMVar waitVar >>= either throwIO return
    restore (body wait) `onException` killThread tid

ignoreSigPipe :: IO () -> IO ()
ignoreSigPipe = handle $ \e -> case e of
                                   IOError { ioe_type  = ResourceVanished
                                           , ioe_errno = Just ioe }
                                     | Errno ioe == ePIPE -> return ()
                                   _ -> throwIO e
