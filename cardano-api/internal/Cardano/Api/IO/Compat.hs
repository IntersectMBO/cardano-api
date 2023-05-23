{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cardano.Api.IO.Compat
  ( checkVrfFilePermissions
  , handleFileForWritingWithOwnerPermission
  , writeSecrets
  ) where

import           Cardano.Api.Error
import           Cardano.Api.IO.Base
import           Cardano.Api.IO.Compat.Posix
import           Cardano.Api.IO.Compat.Win32

import           Control.Monad.Except (ExceptT)
import           Data.ByteString (ByteString)
import           System.IO

handleFileForWritingWithOwnerPermission
  :: FilePath
  -> (Handle -> IO ())
  -> IO (Either (FileError e) ())
handleFileForWritingWithOwnerPermission = handleFileForWritingWithOwnerPermissionImpl

writeSecrets :: FilePath -> [Char] -> [Char] -> (a -> ByteString) -> [a] -> IO ()
writeSecrets = writeSecretsImpl

checkVrfFilePermissions :: File content direction -> ExceptT VRFPrivateKeyFilePermissionError IO ()
checkVrfFilePermissions = checkVrfFilePermissionsImpl
