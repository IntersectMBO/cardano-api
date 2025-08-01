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

import           Cardano.Api.Error (FileError (..))
import           Cardano.Api.IO.Internal.Base

import           Control.Exception (IOException, bracket, bracketOnError, try)
import           Control.Monad (forM_, when)
import           Control.Monad.Except (ExceptT, runExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except.Extra (handleIOExceptT, left)
import qualified Data.ByteString as BS
import           System.Directory ()
import           System.FilePath ((</>))
import qualified System.IO as IO
import           System.IO (Handle)
import           System.Posix.Files (fileMode, getFileStatus, groupModes, intersectFileModes,
                   nullFileMode, otherModes, ownerModes, ownerReadMode, setFdOwnerAndGroup,
                   setFileMode, stdFileMode)
#if MIN_VERSION_unix(2,8,0)
import           System.Posix.IO (OpenFileFlags (..), OpenMode (..), closeFd, defaultFileFlags,
                   fdToHandle, openFd)
#else
import           System.Posix.IO (OpenMode (..), closeFd, defaultFileFlags, fdToHandle, openFd)
#endif
import           System.Posix.Types (Fd, FileMode)
import           System.Posix.User (getRealUserID)
import           Text.Printf (printf)

handleFileForWritingWithOwnerPermissionImpl
  :: FilePath
  -> (Handle -> IO ())
  -> IO (Either (FileError e) ())
handleFileForWritingWithOwnerPermissionImpl path f = do
  -- On a unix based system, we grab a file descriptor and set ourselves as owner.
  -- Since we're holding the file descriptor at this point, we can be sure that
  -- what we're about to write to is owned by us if an error didn't occur.
  user <- getRealUserID
  ownedFile <-
    try $
      -- We only close the FD on error here, otherwise we let it leak out, since
      -- it will be immediately turned into a Handle (which will be closed when
      -- the Handle is closed)
      bracketOnError
        (openFileDescriptor path WriteOnly)
        closeFd
        (\fd -> setFdOwnerAndGroup fd user (-1) >> pure fd)
  case ownedFile of
    Left (err :: IOException) -> do
      pure $ Left $ FileIOError path err
    Right fd -> do
      bracket
        (fdToHandle fd)
        IO.hClose
        (runExceptT . handleIOExceptT (FileIOError path) . f)

writeSecretsImpl :: FilePath -> [Char] -> [Char] -> (a -> BS.ByteString) -> [a] -> IO ()
writeSecretsImpl outDir prefix suffix secretOp xs =
  forM_ (zip xs [0 :: Int ..]) $
    \(secret, nr) -> do
      let filename = outDir </> prefix <> "." <> printf "%03d" nr <> "." <> suffix
      BS.writeFile filename $ secretOp secret
      setFileMode filename ownerReadMode

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

-- | Opens a file from disk.
openFileDescriptor :: FilePath -> OpenMode -> IO Fd
# if MIN_VERSION_unix(2,8,0)
openFileDescriptor fp openMode =
  openFd fp openMode fileFlags
 where
  fileFlags =
    case openMode of
      ReadOnly ->
        defaultFileFlags
      ReadWrite ->
        defaultFileFlags{creat = Just stdFileMode}
      WriteOnly ->
        defaultFileFlags{creat = Just ownerModes}

# else
openFileDescriptor fp openMode =
  openFd fp openMode fMode fileFlags
 where
  (fMode, fileFlags) =
    case openMode of
      ReadOnly ->
        ( Nothing
        , defaultFileFlags
        )
      ReadWrite ->
        ( Just stdFileMode
        , defaultFileFlags
        )
      WriteOnly ->
        ( Just ownerModes
        , defaultFileFlags
        )

# endif
#endif
