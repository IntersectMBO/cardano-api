{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Test.Cardano.Api.Experimental
  ( tests
  )
where

import qualified Cardano.Api as Api
import qualified Cardano.Api.Experimental as Exp
import           Cardano.Api.Internal.Eon.ShelleyBasedEra (ShelleyBasedEraConstraints)
import qualified Cardano.Api.Internal.Script as Script
import           Cardano.Api.Internal.Tx.Sign (Tx (ShelleyTx))
import qualified Cardano.Api.Ledger as Ledger

import           Lens.Micro ((&))

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

-- | Tests in this module can be run by themselves by writing:
-- ```bash
-- cabal test cardano-api-test --test-options="--pattern=Test.Cardano.Api.Experimental"
-- ```
--
-- IMPORTANT NOTE: If this file requires changes, please update the examples in the
-- documentation in 'cardano-api/src/Cardano/Api/Experimental.hs' too.
tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Experimental"
    [ testProperty
        "Created transaction with traditional and experimental APIs are equivalent"
        prop_created_transaction_with_both_apis_are_the_same
    ]

prop_created_transaction_with_both_apis_are_the_same :: Property
prop_created_transaction_with_both_apis_are_the_same = H.propertyOnce $ do
  let era = Exp.ConwayEra
  let sbe = Api.convert era

  signedTxTraditional <- exampleTransacitonTraditionalWay sbe
  signedTxExperimental <- exampleTransactionExperimentalWay era sbe

  let oldStyleTx :: Api.Tx Api.ConwayEra = ShelleyTx sbe signedTxExperimental

  oldStyleTx H.=== signedTxTraditional
 where
  exampleTxBodyContent
    :: (ShelleyBasedEraConstraints era, H.MonadTest m)
    => Api.AsType era
    -> Api.ShelleyBasedEra era
    -> m (Api.TxBodyContent Api.BuildTx era)
  exampleTxBodyContent eraAsType sbe = do
    srcTxId <-
      H.evalEither $
        Api.deserialiseFromRawBytesHex
          Api.AsTxId
          "be6efd42a3d7b9a00d09d77a5d41e55ceaf0bd093a8aa8a893ce70d9caafd978"
    let srcTxIx = Api.TxIx 0
    destAddress <-
      H.evalMaybe $
        Api.deserialiseAddress
          (Api.AsAddressInEra eraAsType)
          "addr_test1vzpfxhjyjdlgk5c0xt8xw26avqxs52rtf69993j4tajehpcue4v2v"

    let txBodyContent =
          Api.defaultTxBodyContent sbe
            & Api.setTxIns
              [
                ( Api.TxIn srcTxId srcTxIx
                , Api.BuildTxWith (Api.KeyWitness Api.KeyWitnessForSpending)
                )
              ]
            & Api.setTxOuts
              [ Api.TxOut
                  destAddress
                  (Api.TxOutValueShelleyBased sbe (Api.inject (Ledger.Coin 10_000_000)))
                  Api.TxOutDatumNone
                  Script.ReferenceScriptNone
              ]
            & Api.setTxFee (Api.TxFeeExplicit sbe (Ledger.Coin 2_000_000))

    return txBodyContent

  exampleSigningKey :: H.MonadTest m => m (Api.SigningKey Api.PaymentKey)
  exampleSigningKey =
    H.evalEither $
      Api.deserialiseFromBech32
        (Api.AsSigningKey Api.AsPaymentKey)
        "addr_sk1648253w4tf6fv5fk28dc7crsjsaw7d9ymhztd4favg3cwkhz7x8sl5u3ms"

  exampleTransacitonTraditionalWay
    :: H.MonadTest m => Api.ShelleyBasedEra Exp.ConwayEra -> m (Tx Exp.ConwayEra)
  exampleTransacitonTraditionalWay sbe = do
    txBodyContent <- exampleTxBodyContent Api.AsConwayEra sbe
    signingKey <- exampleSigningKey

    txBody <- H.evalEither $ Api.createTransactionBody sbe txBodyContent

    let signedTx :: Api.Tx Api.ConwayEra = Api.signShelleyTransaction sbe txBody [Api.WitnessPaymentKey signingKey]

    return signedTx

  exampleTransactionExperimentalWay
    :: H.MonadTest m
    => Exp.Era Exp.ConwayEra
    -> Api.ShelleyBasedEra Exp.ConwayEra
    -> m (Ledger.Tx (Exp.LedgerEra Exp.ConwayEra))
  exampleTransactionExperimentalWay era sbe = do
    txBodyContent <- exampleTxBodyContent Api.AsConwayEra sbe
    signingKey <- exampleSigningKey

    unsignedTx <- H.evalEither $ Exp.makeUnsignedTx era txBodyContent
    let witness = Exp.makeKeyWitness era unsignedTx (Api.WitnessPaymentKey signingKey)

    let bootstrapWitnesses = []
        keyWitnesses = [witness]

    let signedTx :: Ledger.Tx (Exp.LedgerEra Exp.ConwayEra) = Exp.signTx era bootstrapWitnesses keyWitnesses unsignedTx
    return signedTx
