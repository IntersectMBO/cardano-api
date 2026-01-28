{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Golden.Cardano.Api.Tx
  ( test_golden_tx
  )
where

import Cardano.Api
  ( AsType (..)
  , PaymentCredential (..)
  , StakeAddressReference (..)
  , StakeCredential (..)
  , TxIn (..)
  , TxIx (..)
  , deterministicSigningKey
  , deterministicSigningKeySeedSize
  , getVerificationKey
  , lovelaceToTxOutValue
  , makeShelleyAddressInEra
  , txOutValueToLovelace
  , verificationKeyHash
  )
import Cardano.Api qualified as OldApi
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.Crypto.Seed (mkSeedFromBytes)

import Control.Monad (void)
import Data.ByteString.Char8 qualified as BSC
import Data.Function
import Lens.Micro

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
        sbe = OldApi.convert era
    dummyTxId <-
      H.evalEither $
        OldApi.deserialiseFromRawBytesHex $
          BSC.pack "01f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53"
    let txIn = TxIn dummyTxId (TxIx 0)
        seedSize1 = fromIntegral $ deterministicSigningKeySeedSize AsPaymentKey
        seedSize2 = fromIntegral $ deterministicSigningKeySeedSize AsStakeKey
        dummyKey = deterministicSigningKey AsPaymentKey (mkSeedFromBytes (BSC.pack (replicate seedSize1 '\0')))
        dummyStakeKey = deterministicSigningKey AsStakeKey (mkSeedFromBytes (BSC.pack (replicate seedSize2 '\0')))

    let addr1 =
          makeShelleyAddressInEra
            sbe
            OldApi.Mainnet
            (PaymentCredentialByKey (verificationKeyHash $ getVerificationKey dummyKey))
            (StakeAddressByValue (StakeCredentialByKey (verificationKeyHash $ getVerificationKey dummyStakeKey)))

        simpleScript = OldApi.SimpleScript (OldApi.RequireSignature (verificationKeyHash $ getVerificationKey dummyKey))
        refScript =
          OldApi.refScriptToShelleyScript sbe $
            OldApi.ReferenceScript
              OldApi.BabbageEraOnwardsConway
              (OldApi.ScriptInAnyLang OldApi.SimpleScriptLanguage simpleScript)

        amt = txOutValueToLovelace $ lovelaceToTxOutValue sbe 1
        basicOut =
          L.mkBasicTxOut
            (OldApi.toShelleyAddr addr1)
            (L.inject amt)
            & L.referenceScriptTxOutL .~ refScript
        txOut =
          Exp.TxOut
            basicOut

        txRetColl = Exp.TxReturnCollateral basicOut
        txTotalColl = Exp.TxTotalCollateral (L.inject $ L.Coin 1)
        txBodyContent' =
          Exp.defaultTxBodyContent
            & Exp.setTxIns [(txIn, Exp.AnyKeyWitnessPlaceholder)]
            & Exp.setTxOuts [txOut]
            & Exp.setTxValidityLowerBound 0
            & Exp.setTxValidityUpperBound 0
            & Exp.setTxReturnCollateral txRetColl
            & Exp.setTxTotalCollateral txTotalColl
            & Exp.setTxFee (L.Coin 0)

    let unsignedTx = Exp.makeUnsignedTx era txBodyContent'
        tx = Exp.signTx era [] [] unsignedTx
        Exp.SignedTx ledgerTx = tx
        oldStyleTx = OldApi.ShelleyTx sbe ledgerTx

    void . H.evalIO $ OldApi.writeTxFileTextEnvelope sbe (OldApi.File outFileNonCanonical) oldStyleTx
    void . H.evalIO $
      OldApi.writeTxFileTextEnvelopeCanonical sbe (OldApi.File outFileCanonical) oldStyleTx

    canonical <- H.readFile outFileCanonical
    nonCanonical <- H.readFile outFileNonCanonical

    -- Ensure canonical file matches golden
    H.diffFileVsGoldenFile outFileCanonical goldenFile

    -- Ensure canonical is different from non canonical
    H.assert $ canonical /= nonCanonical
