{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cardano.Api.IO.Internal.Compat
  ( checkVrfFilePermissions
  , handleFileForWritingWithOwnerPermission
  , writeSecrets
  )
where

import Cardano.Api.Error
import Cardano.Api.IO.Internal.Base
import Cardano.Api.IO.Internal.Compat.Posix
import Cardano.Api.IO.Internal.Compat.Wasm
import Cardano.Api.IO.Internal.Compat.Win32

import Control.Monad.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString (ByteString)
import GHC.Stack (HasCallStack)
import System.IO

handleFileForWritingWithOwnerPermission
  :: MonadIO m
  => FilePath
  -> (Handle -> IO ())
  -> m (Either (FileError e) ())
handleFileForWritingWithOwnerPermission path f =
  liftIO $ handleFileForWritingWithOwnerPermissionImpl path f

-- | Write a list of secrets to individual files in the given directory, as
-- @\<prefix\>.\<3-digit index\>.\<suffix\>@. Each file is written atomically
-- (the contents go to a temporary file which is then renamed into place) and
-- ends up readable only by its owner.
writeSecrets
  :: HasCallStack
  => MonadIO m
  => FilePath
  -- ^ Output directory
  -> [Char]
  -- ^ Filename prefix
  -> [Char]
  -- ^ Filename suffix
  -> (a -> ByteString)
  -- ^ Serialisation function for the secrets
  -> [a]
  -- ^ Secrets to write, one file each
  -> m ()
writeSecrets outDir prefix suffix secretOp xs =
  liftIO $ writeSecretsImpl outDir prefix suffix secretOp xs

checkVrfFilePermissions :: File content direction -> ExceptT VRFPrivateKeyFilePermissionError IO ()
checkVrfFilePermissions = checkVrfFilePermissionsImpl
