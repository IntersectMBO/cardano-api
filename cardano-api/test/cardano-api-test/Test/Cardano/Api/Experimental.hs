{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Cardano.Api.Experimental
  ( tests
  )
where

import qualified Cardano.Api as Api
import qualified Cardano.Api.Experimental as Exp
import qualified Cardano.Api.Ledger as Ledger
import qualified Cardano.Api.Script as Script

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
tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Experimental"
    [ testProperty
        "Create transaction with experimental API"
        prop_create_transaction_with_experimental_api
    ]

prop_create_transaction_with_experimental_api :: Property
prop_create_transaction_with_experimental_api = H.propertyOnce $ do
  let era = Exp.ConwayEra
  let sbe = Api.convert era

  srcTxId <-
    H.evalEither $
      Api.deserialiseFromRawBytesHex
        Api.AsTxId
        "be6efd42a3d7b9a00d09d77a5d41e55ceaf0bd093a8aa8a893ce70d9caafd978"
  let srcTxIx = Api.TxIx 0
  destAddress <-
    H.evalMaybe $
      Api.deserialiseAddress
        (Api.AsAddressInEra Api.AsConwayEra)
        "addr_test1vzpfxhjyjdlgk5c0xt8xw26avqxs52rtf69993j4tajehpcue4v2v"
  signingKey <-
    H.evalEither $
      Api.deserialiseFromBech32
        (Api.AsSigningKey Api.AsPaymentKey)
        "addr_sk1648253w4tf6fv5fk28dc7crsjsaw7d9ymhztd4favg3cwkhz7x8sl5u3ms"

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

  unsignedTx <- H.evalEither $ Exp.makeUnsignedTx era txBodyContent
  let witness = Exp.makeKeyWitness era unsignedTx (Api.WitnessPaymentKey signingKey)

  let bootstrapWitnesses = []
      keyWitnesses = [witness]

  let signedTx :: Ledger.Tx (Exp.LedgerEra Api.ConwayEra) = Exp.signTx era bootstrapWitnesses keyWitnesses unsignedTx
  
  -- The following line gives the following compilation type error:
  --  Script.ReferenceScriptNone
  -- • No instance for ‘Api.HasTextEnvelope
  --                      (Cardano.Ledger.Alonzo.Tx.AlonzoTx
  --                         (cardano-ledger-conway-1.17.0.0:Cardano.Ledger.Conway.Era.ConwayEra
  --                            Ledger.StandardCrypto))’
  --     arising from a use of ‘Api.textEnvelopeToJSON’

  H.note_ $ show $ Api.textEnvelopeToJSON Nothing signedTx

  fail "TODO: implement this test"
