{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 704
{-# LANGUAGE Safe #-}
#endif

-- This module backports `openTempFile` from GHC 8.10 to hsc2hs in order to get
-- an atomic `openTempFile` implementation on Windows when using older GHC
-- compilers.
-- See also https://gitlab.haskell.org/ghc/ghc/issues/10731
--
-- When hsc2hs supports GHC 8.10 as minimum then this module can be removed.
-- When using WINIO we MUST use the version in base so force it to be used.
-- WINIO is supported in GHC 8.12+ so the extra check is just for sanity.
module Compat.TempFile (
    openBinaryTempFile,
    openTempFile
  ) where

#if !MIN_VERSION_base(4,14,0) && defined(mingw32_HOST_OS) \
    && !defined(__IO_MANAGER_WINIO__)
#define NEEDS_TEMP_WORKAROUND 1
#else
#define NEEDS_TEMP_WORKAROUND 0
#endif

#if NEEDS_TEMP_WORKAROUND
import Data.Bits
import Foreign.C.Error
import Foreign.C.String
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal.Alloc
import Foreign.Storable
import GHC.IO.Encoding
import GHC.IO.IOMode
import qualified GHC.IO.FD as FD
import qualified GHC.IO.Handle.FD as POSIX
import System.Posix.Internals
import System.Posix.Types
#else
import qualified System.IO as IOUtils
#endif

import GHC.IO.Handle

-- | The function creates a temporary file in ReadWrite mode.
-- The created file isn\'t deleted automatically, so you need to delete it manually.
--
-- The file is created with permissions such that only the current
-- user can read\/write it.
--
-- With some exceptions (see below), the file will be created securely
-- in the sense that an attacker should not be able to cause
-- openTempFile to overwrite another file on the filesystem using your
-- credentials, by putting symbolic links (on Unix) in the place where
-- the temporary file is to be created.  On Unix the @O_CREAT@ and
-- @O_EXCL@ flags are used to prevent this attack, but note that
-- @O_EXCL@ is sometimes not supported on NFS filesystems, so if you
-- rely on this behaviour it is best to use local filesystems only.
--
openTempFile :: FilePath   -- ^ Directory in which to create the file
             -> String     -- ^ File name template. If the template is \"foo.ext\" then
                           -- the created file will be \"fooXXX.ext\" where XXX is some
                           -- random number. Note that this should not contain any path
                           -- separator characters.
             -> IO (FilePath, Handle)
openTempFile tmp_dir template
#if NEEDS_TEMP_WORKAROUND
    = openTempFile' "openTempFile" tmp_dir template False 0o600
#else
    = IOUtils.openTempFile tmp_dir template
#endif

-- | Like 'openTempFile', but opens the file in binary mode. See 'openBinaryFile' for more comments.
openBinaryTempFile :: FilePath -> String -> IO (FilePath, Handle)
openBinaryTempFile tmp_dir template
#if NEEDS_TEMP_WORKAROUND
    = openTempFile' "openBinaryTempFile" tmp_dir template True 0o600
#else
    = IOUtils.openBinaryTempFile tmp_dir template
#endif


#if NEEDS_TEMP_WORKAROUND
openTempFile' :: String -> FilePath -> String -> Bool -> CMode
              -> IO (FilePath, Handle)
openTempFile' loc tmp_dir template binary mode
    | pathSeparator template
    = error $ "openTempFile': Template string must not contain path separator characters: "++template
    | otherwise = findTempName
  where
    -- We split off the last extension, so we can use .foo.ext files
    -- for temporary files (hidden on Unix OSes). Unfortunately we're
    -- below filepath in the hierarchy here.
    (prefix, suffix) =
       case break (== '.') $ reverse template of
         -- First case: template contains no '.'s. Just re-reverse it.
         (rev_suffix, "")       -> (reverse rev_suffix, "")
         -- Second case: template contains at least one '.'. Strip the
         -- dot from the prefix and prepend it to the suffix (if we don't
         -- do this, the unique number will get added after the '.' and
         -- thus be part of the extension, which is wrong.)
         (rev_suffix, '.':rest) -> (reverse rest, '.':reverse rev_suffix)
         -- Otherwise, something is wrong, because (break (== '.')) should
         -- always return a pair with either the empty string or a string
         -- beginning with '.' as the second component.
         _                      -> error "bug in System.IO.openTempFile"
    findTempName = do
      let label = if null prefix then "ghc" else prefix
      withCWString tmp_dir $ \c_tmp_dir ->
        withCWString label $ \c_template ->
          withCWString suffix $ \c_suffix ->
            -- FIXME: revisit this when new I/O manager in place and use a UUID
            --       based one when we are no longer MAX_PATH bound.
            allocaBytes (sizeOf (undefined :: CWchar) * 260) $ \c_str -> do
            res <- c_getTempFileNameErrorNo c_tmp_dir c_template c_suffix 0
                                            c_str
            if not res
               then do errno <- getErrno
                       ioError (errnoToIOError loc errno Nothing (Just tmp_dir))
               else do filename <- peekCWString c_str
                       handleResults filename

    handleResults filename = do
      let oflags1 = rw_flags .|. o_EXCL
          binary_flags
              | binary    = o_BINARY
              | otherwise = 0
          oflags = oflags1 .|. binary_flags
      fd <- withFilePath filename $ \ f -> c_open f oflags mode
      case fd < 0 of
        True -> do errno <- getErrno
                   ioError (errnoToIOError loc errno Nothing (Just tmp_dir))
        False ->
          do (fD,fd_type) <- FD.mkFD fd ReadWriteMode Nothing{-no stat-}
                                     False{-is_socket-}
                                     True{-is_nonblock-}

             enc <- getLocaleEncoding
             h <- POSIX.mkHandleFromFD fD fd_type filename ReadWriteMode
                                 False{-set non-block-} (Just enc)

             return (filename, h)

foreign import ccall "__get_temp_file_name" c_getTempFileNameErrorNo
  :: CWString -> CWString -> CWString -> CUInt -> Ptr CWchar -> IO Bool

pathSeparator :: String -> Bool
pathSeparator template = any (\x-> x == '/' || x == '\\') template

output_flags = std_flags

-- XXX Copied from GHC.Handle
std_flags, output_flags, rw_flags :: CInt
std_flags    = o_NONBLOCK   .|. o_NOCTTY
rw_flags     = output_flags .|. o_RDWR
#endif /* NEEDS_TEMP_WORKAROUND */
