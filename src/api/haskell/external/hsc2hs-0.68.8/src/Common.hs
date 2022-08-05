{-# LANGUAGE CPP #-}
module Common where

import qualified Control.Exception as Exception
import qualified Compat.TempFile as Compat
import Control.Monad            ( when )
import Data.Char                ( isSpace )
import Data.List                ( foldl' )
import System.IO
#if defined(mingw32_HOST_OS)
import Control.Concurrent       ( threadDelay )
import System.IO.Error          ( isPermissionError )
#endif
import System.Process           ( createProcess, waitForProcess
                                , proc, CreateProcess(..), StdStream(..) )
import System.Exit              ( ExitCode(..), exitWith )
import System.Directory         ( removeFile )

die :: String -> IO a
die s = hPutStr stderr s >> exitWith (ExitFailure 1)

default_compiler :: String
default_compiler = "cc"

------------------------------------------------------------------------
-- Write the output files.

writeBinaryFile :: FilePath -> String -> IO ()
writeBinaryFile fp str = withBinaryFile fp WriteMode $ \h -> hPutStr h str

rawSystemL :: FilePath -> FilePath -> String -> Bool -> FilePath -> [String] -> IO ()
rawSystemL outDir outBase action flg prog args = withResponseFile outDir outBase args $ \rspFile -> do
  let cmdLine = prog++" "++unwords args
  when flg $ hPutStrLn stderr ("Executing: (@" ++ rspFile ++ ") " ++ cmdLine)
  (_ ,_ ,progerr ,ph) <- createProcess (proc prog ['@':rspFile])
  -- Because of the response files being written and removed after the process
  -- terminates we now need to use process jobs here to correctly wait for all
  -- child processes to terminate.  Not doing so would causes a race condition
  -- between the last child dieing and not holding a lock on the response file
  -- and the response file getting deleted.
    { std_err = CreatePipe
#if MIN_VERSION_process(1,5,0)
    , use_process_jobs = True
#endif
    }
  errdata <- maybeReadHandle progerr
  exitStatus <- waitForProcess ph
  case exitStatus of
    ExitFailure exitCode ->
      do die $ action ++ " failed "
                      ++ "(exit code "    ++ show exitCode ++ ")\n"
                      ++ "rsp file was: " ++ show rspFile ++ "\n"
                      ++ "command was: "  ++ cmdLine ++ "\n"
                      ++ "error: "        ++ errdata ++ "\n"
    _                    -> return ()


rawSystemWithStdOutL :: FilePath -> FilePath -> String -> Bool -> FilePath -> [String] -> FilePath -> IO ()
rawSystemWithStdOutL outDir outBase action flg prog args outFile = withResponseFile outDir outBase args $ \rspFile -> do
  let cmdLine = prog++" "++unwords args++" >"++outFile
  when flg (hPutStrLn stderr ("Executing: (@" ++ rspFile ++ ") " ++ cmdLine))
  hOut <- openFile outFile WriteMode
  (_ ,_ ,progerr , process) <-
    -- We use createProcess here instead of runProcess since we need to specify
    -- a custom CreateProcess structure to turn on use_process_jobs when
    -- available.
    createProcess
      (proc prog  ['@':rspFile])
         { std_out = UseHandle hOut, std_err = CreatePipe
#if MIN_VERSION_process(1,5,0)
         , use_process_jobs = True
#endif
         }
  errdata <- maybeReadHandle progerr
  exitStatus <- waitForProcess process
  hClose hOut
  case exitStatus of
    ExitFailure exitCode ->
      do die $ action ++ " failed "
                      ++ "(exit code "    ++ show exitCode ++ ")\n"
                      ++ "rsp file was: " ++ show rspFile ++ "\n"
                      ++ "output file:"   ++ show outFile ++ "\n"
                      ++ "command was: "  ++ cmdLine ++ "\n"
                      ++ "error: "        ++ errdata ++ "\n"
    _                    -> return ()

maybeReadHandle :: Maybe Handle -> IO String
maybeReadHandle Nothing  = return "<no data>"
maybeReadHandle (Just h) = do
    str <- hGetContents h
    -- Pipes have a buffer, once buffer gets full writes to the pipe block
    -- until the data currently in the buffer is read.  To ensure we don't
    -- block indefinitely we need to actually read from the pipe we requested.
    -- Because of the lazy IO, hGetContents doesn't actually drain handle.
    -- See https://github.com/haskell/hsc2hs/issues/47
    Exception.evaluate (rnf str `seq` str)
  where
    rnf :: String -> ()
    rnf []     = ()
    rnf (c:cs) = c `seq` rnf cs

-- delay the cleanup of generated files until the end; attempts to
-- get around intermittent failure to delete files which has
-- just been exec'ed by a sub-process (Win32 only.)
finallyRemove :: FilePath -> IO a -> IO a
finallyRemove fp act =
  Exception.bracket_ (return fp)
           (noisyRemove fp)
           act
 where
  max_retries :: Int
  max_retries = 5

  noisyRemove :: FilePath -> IO ()
  noisyRemove fpath =
    catchIO (removeFileInternal max_retries fpath)
            (\ e -> hPutStrLn stderr ("Failed to remove file " ++ fpath ++ "; error= " ++ show e))
  removeFileInternal _retries path = do
#if defined(mingw32_HOST_OS)
  -- On Windows we have to retry the delete a couple of times.
  -- The reason for this is that a FileDelete command just marks a
  -- file for deletion. The file is really only removed when the last
  -- handle to the file is closed. Unfortunately there are a lot of
  -- system services that can have a file temporarily opened using a shared
  -- read-only lock, such as the built in AV and search indexer.
  --
  -- We can't really guarantee that these are all off, so what we can do is
  -- whenever after an rm the file still exists to try again and wait a bit.
    res <- Exception.try $ removeFile path
    case res of
      Right a -> return a
      Left ex | isPermissionError ex && _retries > 1 -> do
                  let retries' = _retries - 1
                  threadDelay ((max_retries - retries') * 200)
                  removeFileInternal retries' path
              | otherwise -> Exception.throw ex
#else
    removeFile path
#endif

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

onlyOne :: String -> IO a
onlyOne what = die ("Only one "++what++" may be specified\n")

-- response file handling borrowed from cabal's at Distribution.Simple.Program.ResponseFile

withTempFile :: FilePath -- ^ Temp dir to create the file in
             -> FilePath -- ^ Name of the hsc file being processed or template
             -> String   -- ^ Template for temp file
             -> Int      -- ^ Random seed for tmp name
             -> (FilePath -> Handle -> IO a) -> IO a
withTempFile tmpDir _outBase template _seed action = do
  Exception.bracket
    (Compat.openTempFile tmpDir template)
    (\(name, handle) -> finallyRemove name $ hClose handle)
    (uncurry action)

withResponseFile ::
     FilePath           -- ^ Working directory to create response file in.
  -> FilePath           -- ^ Template for response file name.
  -> [String]           -- ^ Arguments to put into response file.
  -> (FilePath -> IO a)
  -> IO a
withResponseFile workDir outBase arguments f =
  withTempFile workDir outBase "hsc2hscall.rsp" (length arguments) $ \responseFileName hf -> do
    let responseContents = unlines $ map escapeResponseFileArg arguments
    hPutStr hf responseContents
    hClose hf
    f responseFileName

-- Support a gcc-like response file syntax.  Each separate
-- argument and its possible parameter(s), will be separated in the
-- response file by an actual newline; all other whitespace,
-- single quotes, double quotes, and the character used for escaping
-- (backslash) are escaped.  The called program will need to do a similar
-- inverse operation to de-escape and re-constitute the argument list.
escapeResponseFileArg :: String -> String
escapeResponseFileArg = reverse . foldl' escape []
  where
    escape :: String -> Char -> String
    escape cs c =
      case c of
        '\\'          -> c:'\\':cs
        '\''          -> c:'\\':cs
        '"'           -> c:'\\':cs
        _ | isSpace c -> c:'\\':cs
          | otherwise -> c:cs
