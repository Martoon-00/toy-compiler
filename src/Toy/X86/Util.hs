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
import           System.IO               (Handle, hClose, hGetContents, hPutStr)
import           System.Process          (CreateProcess (..), ProcessHandle,
                                          ProcessHandle, StdStream (..), createProcess,
                                          waitForProcess)

-- | Copy-pasted `readCreateProcess` functions, but ignores stdout and
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


-- * Following functions are exported by /process/ library in latest lts only.

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

withCreateProcess
  :: CreateProcess
  -> (Maybe Handle -> Maybe Handle -> Maybe Handle -> ProcessHandle -> IO a)
  -> IO a
withCreateProcess c action =
    createProcess c >>=
              (\(m_in, m_out, m_err, ph) -> action m_in m_out m_err ph)
