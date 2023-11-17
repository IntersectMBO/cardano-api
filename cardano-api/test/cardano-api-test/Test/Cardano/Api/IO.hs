{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Api.IO
  ( tests
  ) where

import           Cardano.Api
import           Cardano.Api.IO
import           Cardano.Api.Pretty

import           Control.Monad.Except (runExceptT)
import           Control.Monad.IO.Class (liftIO)
import           System.Directory (removeFile)

import           Hedgehog
import qualified Hedgehog.Extras as H
import           Hedgehog.Internal.Property
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog

prop_createVrfFileWithOwnerPermissions :: Property
prop_createVrfFileWithOwnerPermissions =
  H.propertyOnce . H.moduleWorkspace "help" $ \ws -> do
    file <- H.noteTempFile ws "file"

    result <- liftIO $ writeLazyByteStringFileWithOwnerPermissions (File file) ""

    case result of
      Left err -> failWith Nothing $ prettyToString $ prettyError @(FileError ()) err
      Right () -> return ()

    fResult <- liftIO . runExceptT $ checkVrfFilePermissions (File file)

    case fResult of
      Left err -> failWith Nothing $ show err
      Right () -> liftIO (removeFile file) >> success

tests :: TestTree
tests = testGroup "Test.Cardano.Api.IO"
  [ testProperty "Create VRF File with Owner Permissions" prop_createVrfFileWithOwnerPermissions
  ]
