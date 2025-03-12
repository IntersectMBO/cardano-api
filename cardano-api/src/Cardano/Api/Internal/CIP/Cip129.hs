{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Api.Internal.CIP.Cip129
  ( Cip129 (..)
  , deserialiseFromBech32Cip129
  , serialiseToBech32Cip129
  , serialiseGovActionIdToBech32Cip129
  , deserialiseGovActionIdFromBech32Cip129
  , AsType (AsColdCommitteeCredential, AsDrepCredential, AsHotCommitteeCredential)
  )
where

import Cardano.Api.Internal.Governance.Actions.ProposalProcedure
import Cardano.Api.Internal.HasTypeProxy
import Cardano.Api.Internal.Orphans (AsType (..))
import Cardano.Api.Internal.SerialiseBech32
import Cardano.Api.Internal.SerialiseRaw
import Cardano.Api.Internal.TxIn
import Cardano.Api.Internal.Utils

import Cardano.Ledger.Conway.Governance qualified as Gov
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Credential qualified as L
import Cardano.Ledger.Keys qualified as L

import Codec.Binary.Bech32 qualified as Bech32
import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as C8
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import GHC.Exts (IsList (..))

-- | Cip-129 is a typeclass that captures the serialisation requirements of https://cips.cardano.org/cip/CIP-0129
-- which pertain to governance credentials and governance action ids.
class (SerialiseAsRawBytes a, HasTypeProxy a) => Cip129 a where
  -- | The human readable part of the Bech32 encoding for the credential.
  cip129Bech32PrefixFor :: AsType a -> Bech32.HumanReadablePart

  -- | The header byte that identifies the credential type according to Cip-129.
  cip129HeaderHexByte :: a -> ByteString

  -- | Permitted bech32 prefixes according to Cip-129.
  cip129Bech32PrefixesPermitted :: AsType a -> [Text]
  default cip129Bech32PrefixesPermitted :: AsType a -> [Text]
  cip129Bech32PrefixesPermitted = return . Bech32.humanReadablePartToText . cip129Bech32PrefixFor

-- | The human readable part of the Bech32 encoding for the credential. This will
-- error if the prefix is not valid.
unsafeHumanReadablePartFromText :: Text -> Bech32.HumanReadablePart
unsafeHumanReadablePartFromText =
  either (error . ("Error while parsing Bech32: " <>) . show) id
    . Bech32.humanReadablePartFromText

instance Cip129 (Credential L.ColdCommitteeRole) where
  cip129Bech32PrefixFor _ = unsafeHumanReadablePartFromText "cc_cold"
  cip129Bech32PrefixesPermitted AsColdCommitteeCredential = ["cc_cold"]

  cip129HeaderHexByte =
    BS.singleton . \case
      L.KeyHashObj{} -> 0x12 -- 0001 0010
      L.ScriptHashObj{} -> 0x13 -- 0001 0011

instance Cip129 (Credential L.HotCommitteeRole) where
  cip129Bech32PrefixFor _ = unsafeHumanReadablePartFromText "cc_hot"
  cip129Bech32PrefixesPermitted AsHotCommitteeCredential = ["cc_hot"]
  cip129HeaderHexByte =
    BS.singleton . \case
      L.KeyHashObj{} -> 0x02 -- 0000 0010
      L.ScriptHashObj{} -> 0x03 -- 0000 0011

instance Cip129 (Credential L.DRepRole) where
  cip129Bech32PrefixFor _ = unsafeHumanReadablePartFromText "drep"
  cip129Bech32PrefixesPermitted AsDrepCredential = ["drep"]
  cip129HeaderHexByte =
    BS.singleton . \case
      L.KeyHashObj{} -> 0x22 -- 0010 0010
      L.ScriptHashObj{} -> 0x23 -- 0010 0011

-- | Serialize a accoding to the serialisation requirements of https://cips.cardano.org/cip/CIP-0129
-- which currently pertain to governance credentials. Governance action ids are dealt separately with
-- via 'serialiseGovActionIdToBech32Cip129'.
serialiseToBech32Cip129 :: forall a. Cip129 a => a -> Text
serialiseToBech32Cip129 a =
  Bech32.encodeLenient
    humanReadablePart
    (Bech32.dataPartFromBytes (cip129HeaderHexByte a <> serialiseToRawBytes a))
 where
  humanReadablePart = cip129Bech32PrefixFor (proxyToAsType (Proxy :: Proxy a))

deserialiseFromBech32Cip129
  :: Cip129 a
  => AsType a -> Text -> Either Bech32DecodeError a
deserialiseFromBech32Cip129 asType bech32Str = do
  (prefix, dataPart) <-
    Bech32.decodeLenient bech32Str
      ?!. Bech32DecodingError

  let actualPrefix = Bech32.humanReadablePartToText prefix
      permittedPrefixes = cip129Bech32PrefixesPermitted asType
  guard (actualPrefix `elem` permittedPrefixes)
    ?! Bech32UnexpectedPrefix actualPrefix (fromList permittedPrefixes)

  payload <-
    Bech32.dataPartToBytes dataPart
      ?! Bech32DataPartToBytesError (Bech32.dataPartToText dataPart)

  (header, credential) <-
    case C8.uncons payload of
      Just (header, credential) -> return (C8.singleton header, credential)
      Nothing -> Left $ Bech32DeserialiseFromBytesError payload

  value <- case deserialiseFromRawBytes asType credential of
    Right a -> Right a
    Left _ -> Left $ Bech32DeserialiseFromBytesError payload

  let expectedHeader = cip129HeaderHexByte value

  guard (header == expectedHeader)
    ?! Bech32UnexpectedHeader (toBase16Text expectedHeader) (toBase16Text header)

  let expectedPrefix = Bech32.humanReadablePartToText $ cip129Bech32PrefixFor asType
  guard (actualPrefix == expectedPrefix)
    ?! Bech32WrongPrefix actualPrefix expectedPrefix

  return value
 where
  toBase16Text = Text.decodeUtf8 . Base16.encode

-- | Governance Action ID
-- According to Cip129 there is no header byte for GovActionId.
-- Instead they append the txid and index to form the payload.
serialiseGovActionIdToBech32Cip129 :: Gov.GovActionId -> Text
serialiseGovActionIdToBech32Cip129 (Gov.GovActionId txid index) =
  let txidHex = serialiseToRawBytes $ fromShelleyTxId txid
      indexHex = C8.pack $ show $ Gov.unGovActionIx index
      payload = txidHex <> indexHex
   in Bech32.encodeLenient
        humanReadablePart
        (Bech32.dataPartFromBytes payload)
 where
  humanReadablePart =
    let prefix = "gov_action"
     in case Bech32.humanReadablePartFromText prefix of
          Right p -> p
          Left err ->
            error $
              "serialiseGovActionIdToBech32Cip129: invalid prefix "
                ++ show prefix
                ++ ", "
                ++ show err

deserialiseGovActionIdFromBech32Cip129
  :: Text -> Either Bech32DecodeError Gov.GovActionId
deserialiseGovActionIdFromBech32Cip129 bech32Str = do
  let permittedPrefixes = ["gov_action"]
  (prefix, dataPart) <-
    Bech32.decodeLenient bech32Str
      ?!. Bech32DecodingError
  let actualPrefix = Bech32.humanReadablePartToText prefix
  guard (actualPrefix `elem` permittedPrefixes)
    ?! Bech32UnexpectedPrefix actualPrefix (fromList permittedPrefixes)

  payload <-
    Bech32.dataPartToBytes dataPart
      ?! Bech32DataPartToBytesError (Bech32.dataPartToText dataPart)

  case deserialiseFromRawBytes AsGovActionId payload of
    Right a -> Right a
    Left _ -> Left $ Bech32DeserialiseFromBytesError payload
