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

import           Control.Exception.Safe (IOException, bracket, bracketOnError, try)
import           Control.Monad (forM_, when)
import           Control.Monad.Except (ExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except.Extra (left)
import           Data.Bifunctor (first)
import qualified Data.ByteString as BS
import           GHC.Stack (HasCallStack)
import qualified System.Directory as IO
import           System.FilePath (splitFileName, (<.>), (</>))
import qualified System.IO as IO
import           System.IO (Handle)
import           System.Posix.Files (fileMode, getFileStatus, getSymbolicLinkStatus, groupModes,
                   intersectFileModes, isRegularFile, nullFileMode, otherModes, ownerReadMode,
                   ownerWriteMode, setFdOwnerAndGroup, setFileMode, unionFileModes)
import           System.Posix.IO (OpenFileFlags (..), OpenMode (..), closeFd, defaultFileFlags,
                   fdToHandle, handleToFd, openFd)
import           System.Posix.Types (FileMode)
import           System.Posix.Unistd (fileSynchronise)
import           System.Posix.User (getRealUserID)
import           Text.Printf (printf)

handleFileForWritingWithOwnerPermissionImpl
  :: FilePath
  -> (Handle -> IO ())
  -> IO (Either (FileError e) ())
handleFileForWritingWithOwnerPermissionImpl path f = do
  -- Replacing a symbolic link (like /dev/stdout), pipe or device would not write
  -- to what it stands for, so those are written in place.
  status <- try $ getSymbolicLinkStatus path
  let isWrittenInPlace = either (\(_ :: IOException) -> False) (not . isRegularFile) status
  if isWrittenInPlace
    then writeInPlace path f
    else replaceAtomically path f

-- | Write in place, through a symbolic link if there is one. A new file gets
-- mode 0600; an existing one keeps its owner and permissions.
writeInPlace
  :: FilePath
  -> (Handle -> IO ())
  -> IO (Either (FileError e) ())
writeInPlace path f =
  fmap (first $ FileIOError path) $
    try $
      bracket
        (fdToHandle =<< openFd path WriteOnly flags)
        IO.hClose
        f
 where
  flags = defaultFileFlags{creat = Just (unionFileModes ownerReadMode ownerWriteMode), trunc = True}

replaceAtomically
  :: FilePath
  -> (Handle -> IO ())
  -> IO (Either (FileError e) ())
replaceAtomically path f = do
  -- On a unix based system, we write to a fresh temporary file (which
  -- 'IO.openTempFile' creates with owner-only permissions) and rename it over
  -- the target path once the contents are safely on disk. The target path thus
  -- always holds either its previous contents or the complete new contents.
  user <- getRealUserID
  fmap (first $ FileIOError path) $
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
            -- us the raw file descriptor. Before the rename, we set the file's
            -- ownership to the real user (which can differ from the effective
            -- user in setuid programs) and sync the descriptor, so a power
            -- failure cannot leave an empty file at the target path.
            bracket
              (handleToFd h)
              closeFd
              (\fd -> setFdOwnerAndGroup fd user (-1) >> fileSynchronise fd)
            IO.renameFile tmpPath path
            -- Sync the directory as well, so the rename itself (and not just
            -- the file contents) survives a power failure.
            bracket
              (openFd targetDir ReadOnly defaultFileFlags)
              closeFd
              fileSynchronise
        )
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
