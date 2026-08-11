{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Api.IO
  ( tests
  )
where

import Cardano.Api

import Control.Monad (unless)
import Data.ByteString qualified as BS
import Data.Either (isLeft)
import System.Directory (removeFile)
import System.FilePath ((</>))
import System.PosixCompat.Files (setFileMode)

import Hedgehog
import Hedgehog.Extras qualified as H
import Hedgehog.Extras.Stock.OS (isWin32)
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

    H.leftFail (firstResult :: Either (FileError ()) ())

    -- 'writeByteStringFile' creates the file with umask-dependent permissions,
    -- so loosen them explicitly and verify the file really is group/other
    -- readable: otherwise the owner-only assertion below could pass vacuously
    -- (e.g. under umask 077). Skipped at runtime on Windows, which has no
    -- group/other permission bits to loosen.
    unless isWin32 $ do
      liftIO $ setFileMode file 0o644

      preResult <- liftIO . runExceptT $ checkVrfFilePermissions (File file)

      unless (isLeft preResult) $
        failWith Nothing "precondition failed: file is not group/other readable before the overwrite"

    result <- liftIO $ writeLazyByteStringFileWithOwnerPermissions (File file) "new contents"

    H.leftFail (result :: Either (FileError ()) ())

    contents <- liftIO $ BS.readFile file
    contents === "new contents"

    fResult <- liftIO . runExceptT $ checkVrfFilePermissions (File file)

    H.leftFail fResult

    liftIO $ removeFile file

prop_writeSecretsOverwritesItsOwnOutput :: Property
prop_writeSecretsOverwritesItsOwnOutput =
  H.propertyOnce . H.moduleWorkspace "help" $ \ws ->
    -- Windows sets the read-only attribute on the secret files, which blocks
    -- replacing them there (pre-existing behaviour): the rerun-overwrite
    -- guarantee is POSIX-only.
    unless isWin32 $ do
      -- The first run leaves 0400, owner-read-only files behind.
      liftIO $ writeSecrets ws "secret" "key" id ["old"]

      -- The second run must replace them regardless.
      liftIO $ writeSecrets ws "secret" "key" id ["new"]

      contents <- liftIO $ BS.readFile (ws </> "secret.000.key")
      contents === "new"

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.IO"
    [ testProperty "Create VRF File with Owner Permissions" prop_createVrfFileWithOwnerPermissions
    , testProperty "Overwrite file with Owner Permissions" prop_overwriteFileWithOwnerPermissions
    , testProperty "writeSecrets overwrites its own output" prop_writeSecretsOverwritesItsOwnOutput
    ]
