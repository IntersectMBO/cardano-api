{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.Cardano.Api.Experimental
  ( tests
  )
where

import qualified Cardano.Api as Api
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Ledger as Ledger
import qualified Cardano.Api.Script as Script

import           Lens.Micro ((&))

import           Hedgehog (Property, success)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

import Cardano.Api.Eon.ShelleyBasedEra (ShelleyBasedEraConstraints)

-- | Tests in this module can be run by themselves by writing:
-- ```bash
-- cabal test cardano-api-test --test-options="--pattern=Test.Cardano.Api.Experimental"
-- ```
tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Experimental"
    [ testProperty
        "Create transaction with traditional API"
        prop_create_transaction_with_traditional_api
    , testProperty
        "Create transaction with experimental API"
        prop_create_transaction_with_experimental_api
    ]

exampleTxBodyContent :: (ShelleyBasedEraConstraints era, H.MonadTest m)
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

prop_create_transaction_with_traditional_api :: Property
prop_create_transaction_with_traditional_api = H.propertyOnce $ do
  let sbe = Api.ShelleyBasedEraConway

  txBodyContent <- exampleTxBodyContent Api.AsConwayEra sbe
  signingKey <- exampleSigningKey

  txBody <- H.evalEither $ Api.createTransactionBody sbe txBodyContent

  let signedTx :: Api.Tx Api.ConwayEra = Api.signShelleyTransaction sbe txBody [Api.WitnessPaymentKey signingKey]

  H.note_ $ show $ Api.textEnvelopeToJSON Nothing signedTx

  success

prop_create_transaction_with_experimental_api :: Property
prop_create_transaction_with_experimental_api = H.propertyOnce $ do
  let era = Exp.ConwayEra
  let sbe = Api.convert era

  txBodyContent <- exampleTxBodyContent Api.AsConwayEra sbe
  signingKey <- exampleSigningKey

  unsignedTx <- H.evalEither $ Exp.makeUnsignedTx era txBodyContent
  let witness = Exp.makeKeyWitness era unsignedTx (Api.WitnessPaymentKey signingKey)

  let bootstrapWitnesses = []
      keyWitnesses = [witness]

  let _signedTx :: Ledger.Tx (Exp.LedgerEra Exp.ConwayEra) = Exp.signTx era bootstrapWitnesses keyWitnesses unsignedTx

  -- H.note_ $ show $ Api.textEnvelopeToJSON Nothing signedTx

  success
