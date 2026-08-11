{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

#if !defined(mingw32_HOST_OS) && !defined(wasm32_HOST_ARCH)
#define UNIX
#endif

module Cardano.Api.IO.Internal.Compat.Posix
  (
#ifdef UNIX
    VRFPrivateKeyFilePermissionError
  , checkVrfFilePermissionsImpl
  , handleFileForWritingWithOwnerPermissionImpl
  , writeSecretsImpl
#endif
  )
where

#ifdef UNIX

import           Cardano.Api.Error (FileError (..), throwErrorM)
import           Cardano.Api.IO.Internal.Base

import           Control.Exception (IOException, bracket, bracketOnError, try)
import           Control.Monad (forM_, when)
import           Control.Monad.Except (ExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except.Extra (left)
import qualified Data.ByteString as BS
import           GHC.Stack (HasCallStack)
import qualified System.Directory as IO
import           System.FilePath (splitFileName, (<.>), (</>))
import qualified System.IO as IO
import           System.IO (Handle)
import           System.Posix.Files (fileMode, getFileStatus, groupModes, intersectFileModes,
                   nullFileMode, otherModes, ownerReadMode, setFileMode)
import           System.Posix.IO (closeFd, handleToFd)
import           System.Posix.Types (FileMode)
import           System.Posix.Unistd (fileSynchronise)
import           Text.Printf (printf)

handleFileForWritingWithOwnerPermissionImpl
  :: FilePath
  -> (Handle -> IO ())
  -> IO (Either (FileError e) ())
handleFileForWritingWithOwnerPermissionImpl path f = do
  -- On a unix based system, we write to a fresh temporary file (which
  -- 'IO.openTempFile' creates with owner-only permissions) and rename it over
  -- the target path once the contents are safely on disk. The target path thus
  -- always holds either its previous contents or the complete new contents.
  result <-
    try $
      bracketOnError
        (IO.openTempFile targetDir $ targetFile <.> "tmp")
        ( \(tmpPath, h) -> do
            IO.hClose h
            IO.removeFile tmpPath
        )
        ( \(tmpPath, h) -> do
            f h
            -- 'handleToFd' flushes the handle's buffers and closes it, handing
            -- us the raw file descriptor. Syncing the descriptor before the
            -- rename ensures a power failure cannot leave an empty file at the
            -- target path.
            bracket (handleToFd h) closeFd fileSynchronise
            IO.renameFile tmpPath path
        )
  case result of
    Left (err :: IOException) -> pure $ Left $ FileIOError path err
    Right () -> pure $ Right ()
 where
  (targetDir, targetFile) = splitFileName path

writeSecretsImpl
  :: HasCallStack => FilePath -> [Char] -> [Char] -> (a -> BS.ByteString) -> [a] -> IO ()
writeSecretsImpl outDir prefix suffix secretOp xs =
  forM_ (zip xs [0 :: Int ..]) $
    \(secret, nr) -> do
      let filename = outDir </> prefix <> "." <> printf "%03d" nr <> "." <> suffix
      result <- handleFileForWritingWithOwnerPermissionImpl filename $ \h ->
        BS.hPut h $ secretOp secret
      case result of
        Left err -> throwErrorM (err :: FileError ())
        Right () -> setFileMode filename ownerReadMode

-- | Make sure the VRF private key file is readable only
-- by the current process owner the node is running under.
checkVrfFilePermissionsImpl
  :: File content direction -> ExceptT VRFPrivateKeyFilePermissionError IO ()
checkVrfFilePermissionsImpl (File vrfPrivKey) = do
  fs <- liftIO $ getFileStatus vrfPrivKey
  let fm = fileMode fs
  -- Check the the VRF private key file does not give read/write/exec permissions to others.
  when
    (hasOtherPermissions fm)
    (left $ OtherPermissionsExist vrfPrivKey)
  -- Check the the VRF private key file does not give read/write/exec permissions to any group.
  when
    (hasGroupPermissions fm)
    (left $ GroupPermissionsExist vrfPrivKey)
 where
  hasPermission :: FileMode -> FileMode -> Bool
  hasPermission fModeA fModeB = fModeA `intersectFileModes` fModeB /= nullFileMode

  hasOtherPermissions :: FileMode -> Bool
  hasOtherPermissions fm' = fm' `hasPermission` otherModes

  hasGroupPermissions :: FileMode -> Bool
  hasGroupPermissions fm' = fm' `hasPermission` groupModes
#endif
