{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Cardano.Api.Internal.IO.Compat
  ( checkVrfFilePermissions
  , handleFileForWritingWithOwnerPermission
  , writeSecrets
  )
where

import Cardano.Api.Internal.Error
import Cardano.Api.Internal.IO.Base
import Cardano.Api.Internal.IO.Compat.Posix
import Cardano.Api.Internal.IO.Compat.Win32

import Control.Monad.Except (ExceptT)
import Data.ByteString (ByteString)
import System.IO

handleFileForWritingWithOwnerPermission
  :: FilePath
  -> (Handle -> IO ())
  -> IO (Either (FileError e) ())
handleFileForWritingWithOwnerPermission = handleFileForWritingWithOwnerPermissionImpl

writeSecrets :: FilePath -> [Char] -> [Char] -> (a -> ByteString) -> [a] -> IO ()
writeSecrets = writeSecretsImpl

checkVrfFilePermissions :: File content direction -> ExceptT VRFPrivateKeyFilePermissionError IO ()
checkVrfFilePermissions = checkVrfFilePermissionsImpl
