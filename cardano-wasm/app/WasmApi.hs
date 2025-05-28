{-# LANGUAGE ScopedTypeVariables #-}

module WasmApi where

import qualified Cardano.Api as Api
import qualified Cardano.Api.Shelley as Script

import Data.Function ((&))
import qualified Data.Text as Text

mkTransactionImpl :: Api.TxIn -> Text.Text -> Integer -> Integer -> Api.TxBody Api.ConwayEra
mkTransactionImpl srcTxIn destAddr amount fees =
  let sbe :: Api.ShelleyBasedEra Api.ConwayEra = Api.shelleyBasedEra
      txIn =
        ( srcTxIn
        , Api.BuildTxWith (Api.KeyWitness Api.KeyWitnessForSpending)
        )
      (Just destAddress) = Api.deserialiseAddress (Api.AsAddressInEra Api.AsConwayEra) destAddr
      txOut =
        Api.TxOut
          destAddress
          (Api.lovelaceToTxOutValue sbe (fromInteger amount))
          Api.TxOutDatumNone
          Script.ReferenceScriptNone
      txFee = Api.TxFeeExplicit sbe (fromInteger fees)

      txBodyContent =
        Api.defaultTxBodyContent sbe
          & Api.setTxIns [txIn]
          & Api.setTxOuts [txOut]
          & Api.setTxFee txFee
      (Right txBody) = Api.createTransactionBody sbe txBodyContent
   in txBody

signTransactionImpl
  :: Api.TxBody Api.ConwayEra -> Api.SigningKey Api.PaymentKey -> Api.Tx Api.ConwayEra
signTransactionImpl unsignedTx signingKey =
  let sbe :: Api.ShelleyBasedEra Api.ConwayEra = Api.shelleyBasedEra
      witness = Api.WitnessPaymentKey signingKey
      oldApiSignedTx :: Api.Tx Api.ConwayEra = Api.signShelleyTransaction sbe unsignedTx [witness]
   in oldApiSignedTx
