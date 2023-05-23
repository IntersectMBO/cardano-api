{-# LANGUAGE CPP #-}

#if !defined(mingw32_HOST_OS)
#define UNIX
#endif

module Cardano.Api.IO.Compat.Win32
  (
#ifndef UNIX
    checkVrfFilePermissionsImpl,
    handleFileForWritingWithOwnerPermissionImpl,
    writeSecretsImpl,
#endif
  ) where

#ifndef UNIX

import           Cardano.Api.Error (FileError (..))
import           Cardano.Api.IO.Base

import           Control.Exception (bracketOnError)
import           Control.Monad (forM_, when)
import           Control.Monad.Except (ExceptT)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except.Extra (left)
import           Data.Bits
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified System.Directory as IO
import           System.Directory (emptyPermissions, readable, setPermissions)
import           System.FilePath (splitFileName, (<.>), (</>))
import qualified System.IO as IO
import           System.IO (Handle)
import           System.Win32.File
import           Text.Printf (printf)

handleFileForWritingWithOwnerPermissionImpl
  :: FilePath
  -> (Handle -> IO ())
  -> IO (Either (FileError e) ())
handleFileForWritingWithOwnerPermissionImpl path f = do
  -- On something other than unix, we make a _new_ file, and since we created it,
  -- we must own it. We then place it at the target location. Unfortunately this
  -- won't work correctly with pseudo-files.
  bracketOnError
    (IO.openTempFile targetDir $ targetFile <.> "tmp")
    (\(tmpPath, h) -> do
      IO.hClose h >> IO.removeFile tmpPath
      return . Left $ FileErrorTempFile path tmpPath h)
    (\(tmpPath, h) -> do
        f h
        IO.hClose h
        IO.renameFile tmpPath path
        return $ Right ())
  where
    (targetDir, targetFile) = splitFileName path

writeSecretsImpl :: FilePath -> [Char] -> [Char] -> (a -> ByteString) -> [a] -> IO ()
writeSecretsImpl outDir prefix suffix secretOp xs =
  forM_ (zip xs [0::Int ..]) $
  \(secret, nr)-> do
    let filename = outDir </> prefix <> "." <> printf "%03d" nr <> "." <> suffix
    BS.writeFile filename $ secretOp secret
    setPermissions filename (emptyPermissions {readable = True})


-- | Make sure the VRF private key file is readable only
-- by the current process owner the node is running under.
checkVrfFilePermissionsImpl :: File content direction -> ExceptT VRFPrivateKeyFilePermissionError IO ()
checkVrfFilePermissionsImpl (File vrfPrivKey) = do
  attribs <- liftIO $ getFileAttributes vrfPrivKey
  -- https://docs.microsoft.com/en-us/windows/win32/api/fileapi/nf-fileapi-createfilea
  -- https://docs.microsoft.com/en-us/windows/win32/fileio/file-access-rights-constants
  -- https://docs.microsoft.com/en-us/windows/win32/secauthz/standard-access-rights
  -- https://docs.microsoft.com/en-us/windows/win32/secauthz/generic-access-rights
  -- https://docs.microsoft.com/en-us/windows/win32/secauthz/access-mask
  when (attribs `hasPermission` genericPermissions)
       (left $ GenericPermissionsExist vrfPrivKey)
 where
  genericPermissions = gENERIC_ALL .|. gENERIC_READ .|. gENERIC_WRITE .|. gENERIC_EXECUTE
  hasPermission fModeA fModeB = fModeA .&. fModeB /= gENERIC_NONE

#endif
