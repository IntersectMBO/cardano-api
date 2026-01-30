{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Test helpers and assertion utilities for TxOut JSON testing
module Test.Cardano.Api.TxOut.Helpers
  ( -- * JSON Field Assertions
    assertHasFields
  , assertFieldPresent
  , assertFieldNull
  , assertAllNull
  , assertFieldEquals

    -- * Parse Failure Assertions
  , assertParseFails
  , assertParseFailsWithMessage

    -- * Datum Assertions
  , assertDatumEqual
  , assertDatumHashMatches

    -- * JSON Object Manipulation
  , getObjectField
  , hasField
  , isNullField
  )
where

import Cardano.Api hiding (Value)

import Control.Monad (unless)
import Data.Aeson (Object, Value (..))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Stack (HasCallStack, callStack)

import Hedgehog.Extras qualified as H
import Hedgehog.Internal.Property (MonadTest)

-- | Assert that a JSON value has all specified fields
assertHasFields :: (MonadTest m, HasCallStack) => Value -> [Text] -> m ()
assertHasFields (Object obj) fields = do
  let missing = filter (not . hasField obj) fields
  unless (null missing) $
    H.failMessage callStack $
      "Missing fields: " <> show missing <> "\nObject: " <> show obj
assertHasFields val _ =
  H.failMessage callStack $ "Expected Object but got: " <> show val

-- | Assert that a field is present with a specific value
assertFieldPresent :: (MonadTest m, HasCallStack) => Value -> Text -> Value -> m ()
assertFieldPresent (Object obj) field expected = do
  case getObjectField obj field of
    Nothing ->
      H.failMessage callStack $ "Field '" <> Text.unpack field <> "' not found in object"
    Just actual ->
      unless (actual == expected) $
        H.failMessage callStack $
          "Field '"
            <> Text.unpack field
            <> "' has wrong value.\nExpected: "
            <> show expected
            <> "\nActual: "
            <> show actual
assertFieldPresent val field _ =
  H.failMessage callStack $
    "Expected Object but got: " <> show val <> " when checking field " <> Text.unpack field

-- | Assert that a field equals a specific value (same as assertFieldPresent)
assertFieldEquals :: (MonadTest m, HasCallStack) => Value -> Text -> Value -> m ()
assertFieldEquals = assertFieldPresent

-- | Assert that a field is present and is null
assertFieldNull :: (MonadTest m, HasCallStack) => Value -> Text -> m ()
assertFieldNull (Object obj) field = do
  case getObjectField obj field of
    Nothing ->
      H.failMessage callStack $ "Field '" <> Text.unpack field <> "' not found in object"
    Just Null -> return ()
    Just val ->
      H.failMessage callStack $
        "Field '" <> Text.unpack field <> "' is not null, got: " <> show val
assertFieldNull val field =
  H.failMessage callStack $
    "Expected Object but got: " <> show val <> " when checking field " <> Text.unpack field

-- | Assert that all specified fields are null
assertAllNull :: (MonadTest m, HasCallStack) => Value -> [Text] -> m ()
assertAllNull obj fields = mapM_ (assertFieldNull obj) fields

-- | Assert that parsing a JSON value fails
assertParseFails :: forall a m. (Aeson.FromJSON a, MonadTest m, HasCallStack) => Value -> m ()
assertParseFails val =
  case Aeson.fromJSON val of
    Aeson.Success (_ :: a) ->
      H.failMessage callStack $ "Expected parse failure but succeeded for: " <> show val
    Aeson.Error _ -> return ()

-- | Assert that parsing fails with a message containing the specified text
assertParseFailsWithMessage
  :: forall a m. (Aeson.FromJSON a, MonadTest m, HasCallStack) => Value -> Text -> m ()
assertParseFailsWithMessage val expectedMsg =
  case Aeson.fromJSON val of
    Aeson.Success (_ :: a) ->
      H.failMessage callStack $ "Expected parse failure but succeeded for: " <> show val
    Aeson.Error msg ->
      unless (expectedMsg `Text.isInfixOf` Text.pack msg) $
        H.failMessage callStack $
          "Error message doesn't contain expected text.\n"
            <> "Expected substring: "
            <> Text.unpack expectedMsg
            <> "\nActual message: "
            <> msg

-- | Assert that two datums are equal
assertDatumEqual
  :: (MonadTest m, HasCallStack)
  => TxOutDatum ctx era
  -> TxOutDatum ctx era
  -> m ()
assertDatumEqual d1 d2 =
  unless (d1 == d2) $
    H.failMessage callStack $
      "Datums not equal.\nExpected: " <> show d1 <> "\nActual: " <> show d2

-- | Assert that a datum's hash matches the expected hash
assertDatumHashMatches
  :: (MonadTest m, HasCallStack)
  => HashableScriptData
  -> Hash ScriptData
  -> m ()
assertDatumHashMatches datum expectedHash =
  let actualHash = hashScriptDataBytes datum
   in unless (actualHash == expectedHash) $
        H.failMessage callStack $
          "Datum hash mismatch.\n"
            <> "Expected: "
            <> show expectedHash
            <> "\nActual: "
            <> show actualHash

-- | Get a field from a JSON object
getObjectField :: Object -> Text -> Maybe Value
getObjectField obj field = KeyMap.lookup (Aeson.Key.fromText field) obj

-- | Check if an object has a field
hasField :: Object -> Text -> Bool
hasField obj field = KeyMap.member (Aeson.Key.fromText field) obj

-- | Check if a field is null
isNullField :: Object -> Text -> Bool
isNullField obj field =
  case getObjectField obj field of
    Just Null -> True
    _ -> False
