{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wasm.Api.Info where

import Cardano.Api qualified as Api
import Cardano.Api.Ledger qualified as Ledger
import Cardano.Api.Plutus qualified as Script

import Cardano.Wasm.General.ExceptionHandling (justOrError, rightOrError)

import Data.Function ((&))
import Data.Text qualified as Text
import GHC.Stack (HasCallStack)

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
