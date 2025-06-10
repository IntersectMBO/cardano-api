{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WasmApi.WasmApi where

import Cardano.Api (ConwayEraOnwards (ConwayEraOnwardsConway))
import qualified Cardano.Api as Api
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Internal.Tx.Body as Api
import qualified Cardano.Api.Ledger as Ledger
import qualified Cardano.Api.Shelley as Script

import qualified Cardano.Ledger.Core as Ledger

import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteString.Base16 as Base16
import Data.Function ((&))
import qualified Data.Sequence.Strict as StrictSeq
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import GHC.Stack (HasCallStack)
import Lens.Micro ((%~), (.~))

import General.ExceptionHandling (justOrError, rightOrError)

-- | Create a transaction body from a transaction input, destination address, amount, and fees.
mkTransactionImpl
  :: HasCallStack => Api.TxIn -> Text.Text -> Ledger.Coin -> Ledger.Coin -> Api.TxBody Api.ConwayEra
mkTransactionImpl srcTxIn destAddr amount fees =
  let sbe :: Api.ShelleyBasedEra Api.ConwayEra = Api.shelleyBasedEra
      txIn =
        ( srcTxIn
        , Api.BuildTxWith (Api.KeyWitness Api.KeyWitnessForSpending)
        )
      destAddress =
        justOrError
          "Couldn't deserialise destination address"
          $ Api.deserialiseAddress
            (Api.AsAddressInEra Api.AsConwayEra)
            destAddr
      txOut =
        Api.TxOut
          destAddress
          (Api.lovelaceToTxOutValue sbe amount)
          Api.TxOutDatumNone
          Script.ReferenceScriptNone
      txFee = Api.TxFeeExplicit sbe fees

      txBodyContent =
        Api.defaultTxBodyContent sbe
          & Api.setTxIns [txIn]
          & Api.setTxOuts [txOut]
          & Api.setTxFee txFee
   in rightOrError $ Api.createTransactionBody sbe txBodyContent

-- | Sign a transaction body with a private key.
signTransactionImpl
  :: Api.TxBody Api.ConwayEra -> Api.SigningKey Api.PaymentKey -> Api.Tx Api.ConwayEra
signTransactionImpl unsignedTx signingKey =
  let sbe :: Api.ShelleyBasedEra Api.ConwayEra = Api.shelleyBasedEra
      witness = Api.WitnessPaymentKey signingKey
      oldApiSignedTx :: Api.Tx Api.ConwayEra = Api.signShelleyTransaction sbe unsignedTx [witness]
   in oldApiSignedTx

-- | Create a new unsigned transaction (empty).
createTx :: Exp.Era era -> Exp.UnsignedTx era
createTx Exp.ConwayEra = Exp.UnsignedTx (Ledger.mkBasicTx Ledger.mkBasicTxBody)

-- | Add a transaction input to an unsigned transaction.
addTxIn :: Exp.Era era -> Exp.UnsignedTx era -> Api.TxIn -> Exp.UnsignedTx era
addTxIn Exp.ConwayEra (Exp.UnsignedTx tx) txIn =
  Script.conwayEraOnwardsConstraints ConwayEraOnwardsConway $
    let tx' = tx & Ledger.bodyTxL . Ledger.inputsTxBodyL %~ (<> Set.fromList [Api.toShelleyTxIn txIn])
     in Exp.UnsignedTx tx'

-- | Add a transaction output to an unsigned transaction.
addTxOut :: Exp.Era era -> Exp.UnsignedTx era -> Api.TxOut Api.CtxTx era -> Exp.UnsignedTx era
addTxOut Exp.ConwayEra (Exp.UnsignedTx tx) txOut =
  let shelleyTxOut = Api.toShelleyTxOutAny Script.ShelleyBasedEraConway txOut
      tx' = tx & Ledger.bodyTxL . Ledger.outputsTxBodyL %~ (<> StrictSeq.fromList [shelleyTxOut])
   in Exp.UnsignedTx tx'

-- | Set the transaction fee for an unsigned transaction.
setFee :: Exp.Era era -> Exp.UnsignedTx era -> Ledger.Coin -> Exp.UnsignedTx era
setFee Exp.ConwayEra (Exp.UnsignedTx tx) fee =
  let tx' = tx & Ledger.bodyTxL . Ledger.feeTxBodyL .~ fee
   in Exp.UnsignedTx tx'

-- | Sign an unsigned transaction with a private payment key.
signUnsignedTx
  :: Exp.Era era
  -> Exp.UnsignedTx era
  -> Api.SigningKey Api.PaymentKey
  -> Ledger.Tx (Exp.LedgerEra era)
signUnsignedTx era (Exp.UnsignedTx tx) signingKey =
  let witness = Api.WitnessPaymentKey signingKey
      unsignedTx = Exp.UnsignedTx tx
      keyWitness = Exp.makeKeyWitness era unsignedTx witness
   in Exp.signTx era [] [keyWitness] unsignedTx

-- | Convert a Ledger.Tx to CBOR (hex encoded String).
ledgerTxToHexCbor
  :: Ledger.EraTx era
  => Exp.Era era
  -> Ledger.Tx era
  -> String
ledgerTxToHexCbor Exp.ConwayEra ledgerTx =
  let cborEncoding = Ledger.toCBOR ledgerTx
   in Text.unpack (Text.decodeUtf8 (Base16.encode (CBOR.toStrictByteString cborEncoding)))
