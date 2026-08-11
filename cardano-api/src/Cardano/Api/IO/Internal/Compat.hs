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
import Data.ByteString (ByteString)
import GHC.Stack (HasCallStack)
import System.IO

handleFileForWritingWithOwnerPermission
  :: FilePath
  -> (Handle -> IO ())
  -> IO (Either (FileError e) ())
handleFileForWritingWithOwnerPermission = handleFileForWritingWithOwnerPermissionImpl

-- | Write a list of secrets to individual files in the given directory, as
-- @\<prefix\>.\<3-digit index\>.\<suffix\>@. Each file is written atomically
-- (the contents go to a temporary file which is then renamed into place) and
-- ends up readable only by its owner.
writeSecrets
  :: HasCallStack => FilePath -> [Char] -> [Char] -> (a -> ByteString) -> [a] -> IO ()
writeSecrets = writeSecretsImpl

checkVrfFilePermissions :: File content direction -> ExceptT VRFPrivateKeyFilePermissionError IO ()
checkVrfFilePermissions = checkVrfFilePermissionsImpl
