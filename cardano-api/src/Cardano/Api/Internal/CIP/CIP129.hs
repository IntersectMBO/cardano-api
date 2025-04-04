{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Api.Internal.CIP.CIP129
  ( CIP129 (..)
  , deserialiseFromBech32CIP129
  , serialiseToBech32CIP129
  )
where

import Cardano.Api.Internal.Governance.Actions.ProposalProcedure
import Cardano.Api.Internal.HasTypeProxy
import Cardano.Api.Internal.Orphans ()
import Cardano.Api.Internal.SerialiseBech32
import Cardano.Api.Internal.SerialiseRaw
import Cardano.Api.Internal.TxIn
import Cardano.Api.Internal.Utils

import Cardano.Binary qualified as CBOR
import Cardano.Ledger.Conway.Governance qualified as Gov
import Cardano.Ledger.Credential (Credential (..))
import Cardano.Ledger.Credential qualified as L
import Cardano.Ledger.Keys qualified as L

import Codec.Binary.Bech32 qualified as Bech32
import Control.Monad (guard)
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Char8 qualified as C8
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import GHC.Exts (IsList (..))
import Text.Read

class SerialiseAsRawBytes a => CIP129 a where
  cip129Bech32PrefixFor :: a -> Text
  cip129HeaderHexByte :: a -> ByteString
  cip129Bech32PrefixesPermitted :: AsType a -> [Text]

instance CIP129 (Credential L.ColdCommitteeRole) where
  cip129Bech32PrefixFor _ = "cc_cold"
  cip129Bech32PrefixesPermitted AsColdCommitteeCredential = ["cc_cold"]
  cip129HeaderHexByte c =
    case c of
      L.ScriptHashObj{} -> "\x13"
      L.KeyHashObj{} -> "\x12"

instance HasTypeProxy (Credential L.ColdCommitteeRole) where
  data AsType (Credential L.ColdCommitteeRole) = AsColdCommitteeCredential
  proxyToAsType _ = AsColdCommitteeCredential

instance SerialiseAsRawBytes (Credential L.ColdCommitteeRole) where
  serialiseToRawBytes = CBOR.serialize'
  deserialiseFromRawBytes AsColdCommitteeCredential =
    first
      ( \e ->
          SerialiseAsRawBytesError
            ("Unable to deserialise Credential ColdCommitteeRole: " ++ show e)
      )
      . CBOR.decodeFull'

instance CIP129 (Credential L.HotCommitteeRole) where
  cip129Bech32PrefixFor _ = "cc_hot"
  cip129Bech32PrefixesPermitted AsHotCommitteeCredential = ["cc_hot"]
  cip129HeaderHexByte c =
    case c of
      L.ScriptHashObj{} -> "\x03"
      L.KeyHashObj{} -> "\x02"

instance HasTypeProxy (Credential L.HotCommitteeRole) where
  data AsType (Credential L.HotCommitteeRole) = AsHotCommitteeCredential
  proxyToAsType _ = AsHotCommitteeCredential

instance SerialiseAsRawBytes (Credential L.HotCommitteeRole) where
  serialiseToRawBytes = CBOR.serialize'
  deserialiseFromRawBytes AsHotCommitteeCredential =
    first
      ( \e ->
          SerialiseAsRawBytesError
            ("Unable to deserialise Credential HotCommitteeRole: " ++ show e)
      )
      . CBOR.decodeFull'

instance CIP129 (Credential L.DRepRole) where
  cip129Bech32PrefixFor _ = "drep"
  cip129Bech32PrefixesPermitted AsDrepCredential = ["drep"]
  cip129HeaderHexByte c =
    case c of
      L.ScriptHashObj{} -> "\x23"
      L.KeyHashObj{} -> "\x22"

instance HasTypeProxy (Credential L.DRepRole) where
  data AsType (Credential L.DRepRole) = AsDrepCredential
  proxyToAsType _ = AsDrepCredential

instance SerialiseAsRawBytes (Credential L.DRepRole) where
  serialiseToRawBytes = CBOR.serialize'
  deserialiseFromRawBytes AsDrepCredential =
    first
      ( \e ->
          SerialiseAsRawBytesError ("Unable to deserialise Credential DRepRole: " ++ show e)
      )
      . CBOR.decodeFull'

instance CIP129 Gov.GovActionId where
  cip129Bech32PrefixFor _ = "gov_action"
  cip129Bech32PrefixesPermitted AsGovActionId = ["gov_action"]
  cip129HeaderHexByte _ = "\x01"

instance HasTypeProxy Gov.GovActionId where
  data AsType (Gov.GovActionId) = AsGovActionId
  proxyToAsType _ = AsGovActionId

instance SerialiseAsRawBytes Gov.GovActionId where
  serialiseToRawBytes (Gov.GovActionId txid (Gov.GovActionIx ix)) =
    let hex = Base16.encode $ C8.pack $ show ix
     in mconcat [serialiseToRawBytes $ fromShelleyTxId txid, hex]
  deserialiseFromRawBytes AsGovActionId bytes = do
    let (txidBs, index) = BS.splitAt 32 bytes

    txid <- deserialiseFromRawBytes AsTxId txidBs
    let asciiIndex = C8.unpack $ Base16.decodeLenient index
    case readMaybe asciiIndex of
      Just ix -> return $ Gov.GovActionId (toShelleyTxId txid) (Gov.GovActionIx ix)
      Nothing ->
        Left $ SerialiseAsRawBytesError $ "Unable to deserialise GovActionId: invalid index: " <> asciiIndex

serialiseToBech32CIP129 :: forall a. CIP129 a => a -> Text
serialiseToBech32CIP129 a =
  Bech32.encodeLenient
    humanReadablePart
    (Bech32.dataPartFromBytes (cip129HeaderHexByte a <> serialiseToRawBytes a))
 where
  prefix = cip129Bech32PrefixFor a
  humanReadablePart =
    case Bech32.humanReadablePartFromText prefix of
      Right p -> p
      Left err ->
        error $
          "serialiseToBech32: invalid prefix "
            ++ show prefix
            ++ ", "
            ++ show err

deserialiseFromBech32CIP129
  :: forall a
   . CIP129 a
  => AsType a -> Text -> Either Bech32DecodeError a
deserialiseFromBech32CIP129 asType bech32Str = do
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

  let (header, credential) = BS.splitAt 1 payload

  value <- case deserialiseFromRawBytes asType credential of
    Right a -> Right a
    Left _ -> Left $ Bech32DeserialiseFromBytesError payload

  let expectedHeader = cip129HeaderHexByte value

  guard (header == expectedHeader)
    ?! Bech32UnexpectedHeader (toBase16Text expectedHeader) (toBase16Text header)

  let expectedPrefix = cip129Bech32PrefixFor value
  guard (actualPrefix == expectedPrefix)
    ?! Bech32WrongPrefix actualPrefix expectedPrefix

  return value
 where
  toBase16Text = Text.decodeUtf8 . Base16.encode
