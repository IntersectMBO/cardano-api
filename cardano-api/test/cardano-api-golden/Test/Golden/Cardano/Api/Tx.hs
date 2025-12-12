{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Golden.Cardano.Api.Tx
  ( test_golden_tx
  )
where

import Cardano.Api
import Cardano.Api.Experimental qualified as Exp

import Cardano.Crypto.Seed (mkSeedFromBytes)

import Control.Monad (void)
import Data.ByteString.Char8 qualified as BSC

import Hedgehog (Property)
import Hedgehog qualified as H
import Hedgehog.Extras qualified as H
import Test.Tasty (TestTree)
import Test.Tasty.Hedgehog (testProperty)

test_golden_tx :: TestTree
test_golden_tx =
  testProperty "golden tx canonical" tx_canonical

-- This test can be run with: cabal test cardano-api-golden --test-options="-p \"golden tx canonical\""
tx_canonical :: Property
tx_canonical = H.propertyOnce $ do
  H.workspace "tx-canonical" $ \wsPath -> do
    let goldenFile = "test/cardano-api-golden/files/tx-canonical.json"
    outFileCanonical <- H.noteTempFile wsPath "tx-canonical.json"
    outFileNonCanonical <- H.noteTempFile wsPath "tx-non-canonical.json"

    let era = Exp.ConwayEra
        sbe = convert era
        txBodyContent = defaultTxBodyContent sbe
    dummyTxId <-
      H.evalEither $
        deserialiseFromRawBytesHex $
          BSC.pack "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53"
    let txIn = TxIn dummyTxId (TxIx 0)
        seedSize1 = fromIntegral $ deterministicSigningKeySeedSize AsPaymentKey
        seedSize2 = fromIntegral $ deterministicSigningKeySeedSize AsStakeKey
        dummyKey = deterministicSigningKey AsPaymentKey (mkSeedFromBytes (BSC.pack (replicate seedSize1 '\0')))
        dummyStakeKey = deterministicSigningKey AsStakeKey (mkSeedFromBytes (BSC.pack (replicate seedSize2 '\0')))

    let addr1 =
          makeShelleyAddressInEra
            sbe
            Mainnet
            (PaymentCredentialByKey (verificationKeyHash $ getVerificationKey dummyKey))
            (StakeAddressByValue (StakeCredentialByKey (verificationKeyHash $ getVerificationKey dummyStakeKey)))

        simpleScript = SimpleScript (RequireSignature (verificationKeyHash $ getVerificationKey dummyKey))
        refScript = ReferenceScript BabbageEraOnwardsConway (ScriptInAnyLang SimpleScriptLanguage simpleScript)

        txOut =
          TxOut
            addr1
            (lovelaceToTxOutValue sbe 1)
            TxOutDatumNone
            refScript

        txBodyContent' =
          txBodyContent
            { txIns = [(txIn, BuildTxWith (KeyWitness KeyWitnessForSpending))]
            , txOuts = [txOut]
            , txFee = TxFeeExplicit sbe (Coin 0)
            , txValidityLowerBound = TxValidityLowerBound AllegraEraOnwardsConway (SlotNo 0)
            , txValidityUpperBound = TxValidityUpperBound sbe Nothing
            , txTotalCollateral = TxTotalCollateral BabbageEraOnwardsConway (Coin 1)
            , txReturnCollateral = TxReturnCollateral BabbageEraOnwardsConway txOut
            }

    unsignedTx <- H.evalEither $ Exp.makeUnsignedTx era txBodyContent'
    let tx = Exp.signTx era [] [] unsignedTx
    let Exp.SignedTx ledgerTx = tx
    let oldStyleTx = ShelleyTx sbe ledgerTx

    void . H.evalIO $ writeTxFileTextEnvelope sbe (File outFileNonCanonical) oldStyleTx
    void . H.evalIO $ writeTxFileTextEnvelopeCanonical sbe (File outFileCanonical) oldStyleTx

    canonical <- H.readFile outFileCanonical
    nonCanonical <- H.readFile outFileNonCanonical

    -- Ensure canonical is different from non canonical
    H.assert $ canonical /= nonCanonical

    -- Ensure canonical file matches golden
    H.diffFileVsGoldenFile outFileCanonical goldenFile
