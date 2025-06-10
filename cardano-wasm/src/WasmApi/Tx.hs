{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module WasmApi.Tx where

import qualified Cardano.Api as Api
import Cardano.Api.Experimental (obtainCommonConstraints)
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Internal.Tx.Body as TxBody
import qualified Cardano.Api.Ledger as Ledger
import qualified Cardano.Api.Shelley as Shelley

import qualified Cardano.Ledger.Core as Ledger

import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Lens.Micro ((%~), (&), (.~))

import General.ExceptionHandling (justOrError)

-- * @UnsignedTx@ object

-- | An object representing a transaction that is being built and hasn't
-- been signed yet. It abstracts over the era of the transaction.
-- It is meant to be an opaque object in JavaScript API.
data UnsignedTxObject
  = forall era. UnsignedTxObject (Exp.Era era) (Exp.UnsignedTx era)

-- | Create a new unsigned transaction object for making a Conway era transaction.
newConwayTxImpl :: UnsignedTxObject
newConwayTxImpl = UnsignedTxObject Exp.ConwayEra (Exp.UnsignedTx (Ledger.mkBasicTx Ledger.mkBasicTxBody))

-- | Add a simple transaction input to an unsigned transaction object.
addTxInput :: UnsignedTxObject -> Api.TxId -> Api.TxIx -> UnsignedTxObject
addTxInput (UnsignedTxObject era (Exp.UnsignedTx tx)) txId txIx =
  obtainCommonConstraints era $
    let txIn = Api.TxIn txId txIx
        tx' = tx & Ledger.bodyTxL . Ledger.inputsTxBodyL %~ (<> Set.fromList [Shelley.toShelleyTxIn txIn])
     in UnsignedTxObject era $ Exp.UnsignedTx tx'

-- | Add a simple transaction output to an unsigned transaction object.
-- It takes a destination address and an amount in lovelace.
addSimpleTxOutImpl :: UnsignedTxObject -> String -> Ledger.Coin -> UnsignedTxObject
addSimpleTxOutImpl (UnsignedTxObject era (Exp.UnsignedTx tx)) destAddr lovelaceAmount =
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
     in UnsignedTxObject era $ Exp.UnsignedTx tx'
 where
  deserialiseAddress
    :: Exp.EraCommonConstraints era
    => Exp.Era era -> String -> Api.AddressInEra era
  deserialiseAddress _eon destAddrStr =
    justOrError
      "Couldn't deserialise destination address"
      $ Api.deserialiseAddress
        (Api.AsAddressInEra Api.asType)
        (Text.pack destAddrStr)

-- | Set the fee for an unsigned transaction object.
setFeeImpl :: UnsignedTxObject -> Ledger.Coin -> UnsignedTxObject
setFeeImpl (UnsignedTxObject era (Exp.UnsignedTx tx)) fee =
  obtainCommonConstraints era $
    let tx' = tx & Ledger.bodyTxL . Ledger.feeTxBodyL .~ fee
     in UnsignedTxObject era $ Exp.UnsignedTx tx'

-- | Sign an unsigned transaction with a signing key.
signTxImpl :: UnsignedTxObject -> Api.SigningKey Api.PaymentKey -> SignedTxObject
signTxImpl (UnsignedTxObject era unsignedTx) signingKey =
  obtainCommonConstraints era $
    let witness = Api.WitnessPaymentKey signingKey
        keyWitness = Exp.makeKeyWitness era unsignedTx witness
        signedTx = Exp.signTx era [] [keyWitness] unsignedTx
     in SignedTxObject era signedTx

-- | An object representing a signed transaction.
data SignedTxObject
  = forall era. SignedTxObject (Exp.Era era) (Ledger.Tx (Exp.LedgerEra era))

-- | Convert a signed transaction object to a base16 encoded string of its CBOR representation.
toCborImpl :: SignedTxObject -> String
toCborImpl (SignedTxObject era ledgerTx) =
  obtainCommonConstraints era $
    let cborEncoding = Ledger.toCBOR ledgerTx
     in Text.unpack (Text.decodeUtf8 (Base16.encode (CBOR.toStrictByteString cborEncoding)))
