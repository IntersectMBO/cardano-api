{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Wasm.Api.Tx where

import Cardano.Api (FromJSON)
import Cardano.Api qualified as Api
import Cardano.Api.Experimental (obtainCommonConstraints)
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Plutus qualified as Shelley
import Cardano.Api.Tx qualified as TxBody

import Cardano.Ledger.Api qualified as Ledger
import Cardano.Ledger.Binary (Annotator, DecCBOR (decCBOR), EncCBOR, Version, decodeFullAnnotator)
import Cardano.Wasm.General.ExceptionHandling (justOrError, rightOrError)

import Codec.CBOR.Write qualified as CBOR
import Data.Aeson (ToJSON (toJSON), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy (fromStrict)
import Data.Sequence.Strict qualified as StrictSeq
import Data.Set qualified as Set
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Stack (HasCallStack)
import Lens.Micro ((%~), (&), (.~))

-- | Function to convert an era to its corresponding version
eraToVersion :: Exp.Era era -> Version
eraToVersion era =
  case era of
    Exp.ConwayEra -> Ledger.eraProtVerHigh @(Exp.LedgerEra Exp.ConwayEra)

-- * @UnsignedTx@ object

-- | An object representing a transaction that is being built and hasn't
-- been signed yet. It abstracts over the era of the transaction.
-- It is meant to be an opaque object in JavaScript API.
data UnsignedTxObject
  = forall era. UnsignedTxObject (Exp.Era era) [Ledger.WitVKey Ledger.Witness] (Exp.UnsignedTx era)

instance ToJSON UnsignedTxObject where
  toJSON :: UnsignedTxObject -> Aeson.Value
  toJSON (UnsignedTxObject era keyWitnesess (Exp.UnsignedTx tx)) =
    obtainCommonConstraints era $
      let encode :: forall a. EncCBOR a => a -> Text.Text
          encode = Text.decodeUtf8 . Base16.encode . Ledger.serialize' (eraToVersion era)
       in Aeson.object
            [ "era" .= Exp.Some era
            , "keyWitnesess" .= map encode keyWitnesess
            , "tx" .= encode tx
            ]

instance FromJSON UnsignedTxObject where
  parseJSON :: HasCallStack => Aeson.Value -> Aeson.Parser UnsignedTxObject
  parseJSON = Aeson.withObject "UnsignedTxObject" $ \o -> do
    Exp.Some era <- o Aeson..: "era"
    let decode :: forall a. DecCBOR (Annotator a) => Text.Text -> Text.Text -> a
        decode desc cbor = do
          let cddlBS = rightOrError $ Base16.decode $ Text.encodeUtf8 cbor
          rightOrError $ decodeFullAnnotator (eraToVersion era) desc decCBOR (fromStrict cddlBS)
    keyWitnesses :: [Text.Text] <- o Aeson..: "keyWitnesess"
    tx :: Text.Text <- o Aeson..: "tx"
    obtainCommonConstraints era $
      return $
        UnsignedTxObject era (map (decode "KeyWitness") keyWitnesses) (Exp.UnsignedTx (decode "Tx" tx))

-- | Create a new unsigned transaction object for making a Conway era transaction.
newConwayTxImpl :: UnsignedTxObject
newConwayTxImpl = UnsignedTxObject Exp.ConwayEra [] (Exp.UnsignedTx (Ledger.mkBasicTx Ledger.mkBasicTxBody))

-- | Add a simple transaction input to an unsigned transaction object.
addTxInputImpl :: UnsignedTxObject -> Api.TxId -> Api.TxIx -> UnsignedTxObject
addTxInputImpl (UnsignedTxObject era keyWitnesess (Exp.UnsignedTx tx)) txId txIx =
  obtainCommonConstraints era $
    let txIn = Api.TxIn txId txIx
        tx' = tx & Ledger.bodyTxL . Ledger.inputsTxBodyL %~ (<> Set.fromList [TxBody.toShelleyTxIn txIn])
     in UnsignedTxObject era keyWitnesess $ Exp.UnsignedTx tx'

-- | Add a simple transaction output to an unsigned transaction object.
-- It takes a destination address and an amount in lovelace.
addSimpleTxOutImpl :: HasCallStack => UnsignedTxObject -> String -> Ledger.Coin -> UnsignedTxObject
addSimpleTxOutImpl (UnsignedTxObject era keyWitnesess (Exp.UnsignedTx tx)) destAddr lovelaceAmount =
  obtainCommonConstraints era $
    let destAddress = deserialiseAddress era destAddr
        sbe = Api.convert era
        txOut =
          Api.TxOut
            destAddress
            (Api.lovelaceToTxOutValue sbe lovelaceAmount)
            Api.TxOutDatumNone
            Shelley.ReferenceScriptNone
        shelleyTxOut = TxBody.toShelleyTxOutAny sbe txOut
        tx' = tx & Ledger.bodyTxL . Ledger.outputsTxBodyL %~ (<> StrictSeq.fromList [shelleyTxOut])
     in UnsignedTxObject era keyWitnesess $ Exp.UnsignedTx tx'
 where
  deserialiseAddress
    :: HasCallStack
    => Exp.EraCommonConstraints era
    => Exp.Era era -> String -> Api.AddressInEra era
  deserialiseAddress _eon destAddrStr =
    justOrError
      "Couldn't deserialise destination address"
      $ Api.deserialiseAddress
        (Api.AsAddressInEra Api.asType)
        (Text.pack destAddrStr)

-- | Set the fee for an unsigned transaction object.
setFeeImpl :: UnsignedTxObject -> Ledger.Coin -> UnsignedTxObject
setFeeImpl (UnsignedTxObject era keyWitnesess (Exp.UnsignedTx tx)) fee =
  obtainCommonConstraints era $
    let tx' = tx & Ledger.bodyTxL . Ledger.feeTxBodyL .~ fee
     in UnsignedTxObject era keyWitnesess $ Exp.UnsignedTx tx'

-- | Add a payment key to the list of signatories for an unsigned transaction.
addSigningKeyImpl :: UnsignedTxObject -> Api.SigningKey Api.PaymentKey -> UnsignedTxObject
addSigningKeyImpl (UnsignedTxObject era keyWitnesess unsignedTx) signingKey =
  obtainCommonConstraints era $
    let witness = Api.WitnessPaymentKey signingKey
        keyWitness = Exp.makeKeyWitness era unsignedTx witness
     in UnsignedTxObject era (keyWitnesess ++ [keyWitness]) unsignedTx

-- | Sign an unsigned transaction using the list of signatories.
signTxImpl :: UnsignedTxObject -> SignedTxObject
signTxImpl (UnsignedTxObject era keyWitnesess unsignedTx) =
  SignedTxObject era (Exp.signTx era [] keyWitnesess unsignedTx)

-- * @SignedTx@ object

-- | An object representing a signed transaction.
data SignedTxObject
  = forall era. SignedTxObject (Exp.Era era) (Ledger.Tx (Exp.LedgerEra era))

instance ToJSON SignedTxObject where
  toJSON :: SignedTxObject -> Aeson.Value
  toJSON (SignedTxObject era ledgerTx) =
    obtainCommonConstraints era $
      let encode :: forall a. EncCBOR a => a -> Text.Text
          encode = Text.decodeUtf8 . Base16.encode . Ledger.serialize' (eraToVersion era)
       in Aeson.object
            [ "era" .= Exp.Some era
            , "tx" .= encode ledgerTx
            ]

instance FromJSON SignedTxObject where
  parseJSON :: HasCallStack => Aeson.Value -> Aeson.Parser SignedTxObject
  parseJSON = Aeson.withObject "SignedTxObject" $ \o -> do
    Exp.Some era <- o Aeson..: "era"
    let decode :: forall a. DecCBOR (Annotator a) => Text.Text -> Text.Text -> a
        decode desc cbor = do
          let cddlBS = rightOrError $ Base16.decode $ Text.encodeUtf8 cbor
          rightOrError $ decodeFullAnnotator (eraToVersion era) desc decCBOR (fromStrict cddlBS)
    tx :: Text.Text <- o Aeson..: "tx"
    obtainCommonConstraints era $
      return $
        SignedTxObject era (decode "Tx" tx)

-- | Convert a signed transaction object to a base16 encoded string of its CBOR representation.
toCborImpl :: SignedTxObject -> String
toCborImpl (SignedTxObject era ledgerTx) =
  obtainCommonConstraints era $
    let cborEncoding = Ledger.toCBOR ledgerTx
     in Text.unpack (Text.decodeUtf8 (Base16.encode (CBOR.toStrictByteString cborEncoding)))
