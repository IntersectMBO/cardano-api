{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Api.IO
  ( tests
  )
where

import Cardano.Api

import System.Directory (removeFile)

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

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.IO"
    [ testProperty "Create VRF File with Owner Permissions" prop_createVrfFileWithOwnerPermissions
    ]
