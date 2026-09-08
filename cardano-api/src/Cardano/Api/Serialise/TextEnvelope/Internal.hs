{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | TextEnvelope Serialisation
--
-- The envelope type, its JSON codec and the pure decoders now live in the
-- cardano-keys package; re-exported here for compatibility, together with the
-- era-coupled and file-based functions this API adds on top of them.
module Cardano.Api.Serialise.TextEnvelope.Internal
  ( HasTextEnvelope (..)
  , textEnvelopeType
  , textEnvelopeTypeInEra
  , TextEnvelope (..)
  , TextEnvelopeType (..)
  , TextEnvelopeDescr (..)
  , textEnvelopeRawCBOR
  , TextEnvelopeError (..)
  , serialiseToTextEnvelope
  , deserialiseFromTextEnvelope
  , readFileTextEnvelope
  , writeFileTextEnvelope
  , writeFileTextEnvelopeWithOwnerPermissions
  , readTextEnvelopeFromFile
  , readTextEnvelopeOfTypeFromFile
  , textEnvelopeToJSON
  , serialiseTextEnvelope
  , legacyComparison
  , textEnvelopeTypeToEra

    -- * Reading one of several key types
  , FromSomeType (..)
  , deserialiseFromTextEnvelopeAnyOf
  , decodeTextEnvelopeJSON
  , deserialiseFromTextEnvelopeJSON
  , deserialiseFromTextEnvelopeJSONAnyOf
  , readFileTextEnvelopeAnyOf

    -- * Data family instances
  , AsType (..)
  )
where

import Cardano.Api.Era
import Cardano.Api.Error
import Cardano.Api.HasTypeProxy
import Cardano.Api.IO

import Cardano.Keys.Serialise.TextEnvelope

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Except.Extra (firstExceptT, hoistEither)
import Data.Aeson qualified as Aeson
import Data.Text (Text)

instance Error TextEnvelopeError where
  prettyError = renderTextEnvelopeError

textEnvelopeTypeInEra
  :: ()
  => HasTextEnvelope (f era)
  => CardanoEra era
  -> AsType (f era)
  -> TextEnvelopeType
textEnvelopeTypeInEra _ =
  textEnvelopeType

-- | Write a value to a file in the text envelope format.
--
-- Note that this does /not/ set conservative file permissions: the file is
-- created with the default permissions which should be restricted with `umask`. When writing sensitive data such as
-- signing keys, use 'writeFileTextEnvelopeWithOwnerPermissions' instead,
-- which tries to restrict access to the file owner where the platform
-- supports it (see its documentation for the exact guarantees).
writeFileTextEnvelope
  :: HasTextEnvelope a
  => File content Out
  -> Maybe TextEnvelopeDescr
  -> a
  -> IO (Either (FileError ()) ())
writeFileTextEnvelope outputFile mbDescr a =
  writeLazyByteStringFile outputFile (textEnvelopeToJSON mbDescr a)

-- | Like 'writeFileTextEnvelope', but the file is created so that only its
-- owner has access to it, to the extent the platform allows it.
--
-- On POSIX and Windows, the contents are written to a freshly created
-- temporary file which is then renamed over the target path. The target file
-- therefore never exists in a partially written state: if writing fails
-- midway (e.g. on a crash or a full disk), its previous contents are left
-- untouched. A pre-existing file is replaced wholesale: its previous
-- permission bits are not preserved, and a symlink is replaced by a regular
-- file rather than written through.
--
-- * On POSIX systems, the file is created with @0600@ permissions (read
--   and write for the file owner only, further filtered by the process's
--   @umask@), and its ownership is set to the current (real) user. The
--   contents are synced to disk before the rename and the directory after
--   it, so a power failure can neither leave an empty file at the target
--   path nor undo a completed write.
--
-- * On Windows, the file is owned by the current user, but no explicit
--   ACL is set: the file inherits the access control list of the target
--   directory.
--
-- * On WASM, this is currently a no-op: no file is written at all.
--
-- Use this when writing sensitive data such as signing keys.
writeFileTextEnvelopeWithOwnerPermissions
  :: HasTextEnvelope a
  => MonadIO m
  => File content Out
  -> Maybe TextEnvelopeDescr
  -> a
  -> m (Either (FileError ()) ())
writeFileTextEnvelopeWithOwnerPermissions outputFile mbDescr a =
  writeLazyByteStringFileWithOwnerPermissions outputFile (textEnvelopeToJSON mbDescr a)

readFileTextEnvelope
  :: HasTextEnvelope a
  => File content In
  -> IO (Either (FileError TextEnvelopeError) a)
readFileTextEnvelope path =
  runExceptT $ do
    content <- fileIOExceptT (unFile path) readFileBlocking
    firstExceptT (FileError (unFile path)) $
      hoistEither $
        deserialiseFromTextEnvelopeJSON content

readFileTextEnvelopeAnyOf
  :: [FromSomeType HasTextEnvelope b]
  -> File content In
  -> IO (Either (FileError TextEnvelopeError) b)
readFileTextEnvelopeAnyOf types path =
  runExceptT $ do
    content <- fileIOExceptT (unFile path) readFileBlocking
    firstExceptT (FileError (unFile path)) $
      hoistEither $
        deserialiseFromTextEnvelopeJSONAnyOf types content

readTextEnvelopeFromFile
  :: FilePath
  -> IO (Either (FileError TextEnvelopeError) TextEnvelope)
readTextEnvelopeFromFile path =
  runExceptT $ do
    bs <- fileIOExceptT path readFileBlocking
    firstExceptT (FileError path . TextEnvelopeAesonDecodeError)
      . hoistEither
      $ Aeson.eitherDecodeStrict' bs

readTextEnvelopeOfTypeFromFile
  :: TextEnvelopeType
  -> FilePath
  -> IO (Either (FileError TextEnvelopeError) TextEnvelope)
readTextEnvelopeOfTypeFromFile expectedType path =
  runExceptT $ do
    te <- ExceptT (readTextEnvelopeFromFile path)
    firstExceptT (FileError path) $
      hoistEither $
        expectTextEnvelopeOfType (pure expectedType) te
    return te

textEnvelopeTypeToEra :: Text -> Either TextEnvelopeError AnyShelleyBasedEra
textEnvelopeTypeToEra =
  \case
    "TxSignedShelley" -> return $ AnyShelleyBasedEra ShelleyBasedEraShelley
    "Tx AllegraEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraAllegra
    "Tx MaryEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraMary
    "Tx AlonzoEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraAlonzo
    "Tx BabbageEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraBabbage
    "Tx ConwayEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraConway
    "Tx DijkstraEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraDijkstra
    "Witnessed Tx ShelleyEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraShelley
    "Witnessed Tx AllegraEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraAllegra
    "Witnessed Tx MaryEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraMary
    "Witnessed Tx AlonzoEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraAlonzo
    "Witnessed Tx BabbageEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraBabbage
    "Witnessed Tx ConwayEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraConway
    "Witnessed Tx DijkstraEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraDijkstra
    "Unwitnessed Tx ShelleyEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraShelley
    "Unwitnessed Tx AllegraEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraAllegra
    "Unwitnessed Tx MaryEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraMary
    "Unwitnessed Tx AlonzoEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraAlonzo
    "Unwitnessed Tx BabbageEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraBabbage
    "Unwitnessed Tx ConwayEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraConway
    "Unwitnessed Tx DijkstraEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraDijkstra
    "TxWitness ShelleyEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraShelley
    "TxWitness AllegraEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraAllegra
    "TxWitness MaryEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraMary
    "TxWitness AlonzoEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraAlonzo
    "TxWitness BabbageEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraBabbage
    "TxWitness ConwayEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraConway
    "TxWitness DijkstraEra" -> return $ AnyShelleyBasedEra ShelleyBasedEraDijkstra
    unknownCddlType -> Left $ TextEnvelopeUnknownType unknownCddlType
