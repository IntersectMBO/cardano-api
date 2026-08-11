{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Api.IO
  ( tests
  )
where

import Cardano.Api

import Data.ByteString qualified as BS
import System.Directory (removeFile)
#ifndef mingw32_HOST_OS
import System.Posix.Files (setFileMode)
#endif

import Hedgehog
import Hedgehog.Extras qualified as H
import Hedgehog.Internal.Property
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog

prop_createVrfFileWithOwnerPermissions :: Property
prop_createVrfFileWithOwnerPermissions =
  H.propertyOnce . H.moduleWorkspace "help" $ \ws -> do
    file <- H.noteTempFile ws "file"

    result <- liftIO $ writeLazyByteStringFileWithOwnerPermissions (File file) ""

    case result of
      Left err -> failWith Nothing $ docToString $ prettyError @(FileError ()) err
      Right () -> return ()

    fResult <- liftIO . runExceptT $ checkVrfFilePermissions (File file)

    case fResult of
      Left err -> failWith Nothing $ show err
      Right () -> liftIO (removeFile file) >> success

prop_overwriteFileWithOwnerPermissions :: Property
prop_overwriteFileWithOwnerPermissions =
  H.propertyOnce . H.moduleWorkspace "help" $ \ws -> do
    file <- H.noteTempFile ws "file"

    -- Create the target with different contents, to check that overwriting
    -- replaces them.
    firstResult <- liftIO $ writeByteStringFile (File file) "old contents"

    case firstResult of
      Left err -> failWith Nothing $ docToString $ prettyError @(FileError ()) err
      Right () -> return ()

#ifndef mingw32_HOST_OS
    -- 'writeByteStringFile' creates the file with umask-dependent permissions,
    -- so loosen them explicitly and verify the file really is group/other
    -- readable: otherwise the owner-only assertion below could pass vacuously
    -- (e.g. under umask 077). Windows has no group/other permission bits, so
    -- there is no precondition to establish there.
    liftIO $ setFileMode file 0o644

    preResult <- liftIO . runExceptT $ checkVrfFilePermissions (File file)

    case preResult of
      Left _ -> return ()
      Right () ->
        failWith Nothing "precondition failed: file is not group/other readable before the overwrite"
#endif

    result <- liftIO $ writeLazyByteStringFileWithOwnerPermissions (File file) "new contents"

    case result of
      Left err -> failWith Nothing $ docToString $ prettyError @(FileError ()) err
      Right () -> return ()

    contents <- liftIO $ BS.readFile file
    contents === "new contents"

    fResult <- liftIO . runExceptT $ checkVrfFilePermissions (File file)

    case fResult of
      Left err -> failWith Nothing $ show err
      Right () -> liftIO (removeFile file) >> success

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.IO"
    [ testProperty "Create VRF File with Owner Permissions" prop_createVrfFileWithOwnerPermissions
    , testProperty "Overwrite file with Owner Permissions" prop_overwriteFileWithOwnerPermissions
    ]
