{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Test.Cardano.Api.Typed.CBOR
  ( tests
  ) where

import           Cardano.Api
import           Cardano.Api.Shelley (AsType (..))

import           Data.Proxy (Proxy (..))

import           Test.Gen.Cardano.Api.Typed

import           Test.Cardano.Api.Typed.Orphans ()

import           Hedgehog (Property, forAll, property, tripping)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Test.Hedgehog.Roundtrip.CBOR as H
import           Test.Hedgehog.Roundtrip.CBOR
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

{- HLINT ignore "Use camelCase" -}

-- TODO: Need to add PaymentExtendedKey roundtrip tests however
-- we can't derive an Eq instance for Crypto.HD.XPrv

prop_roundtrip_txbody_CBOR :: Property
prop_roundtrip_txbody_CBOR = H.property $ do
  AnyCardanoEra era <- H.forAll $ Gen.element [minBound..AnyCardanoEra BabbageEra]
  x <- H.forAll $ makeSignedTransaction [] <$> genTxBody era
  H.tripping x serialiseTxLedgerCddl (deserialiseTxLedgerCddl era)

prop_roundtrip_tx_CBOR :: Property
prop_roundtrip_tx_CBOR = H.property $ do
  AnyCardanoEra era <- H.forAll $ Gen.element [minBound..AnyCardanoEra BabbageEra]
  x <- H.forAll $ genTx era
  H.trippingCbor (proxyToAsType Proxy) x

prop_roundtrip_witness_CBOR :: Property
prop_roundtrip_witness_CBOR = H.property $ do
  AnyCardanoEra era <- H.forAll $ Gen.element [minBound..maxBound]
  x <- H.forAll $ genCardanoKeyWitness era
  H.trippingCbor (AsKeyWitness (proxyToAsType Proxy)) x

prop_roundtrip_operational_certificate_CBOR :: Property
prop_roundtrip_operational_certificate_CBOR = H.property $ do
  x <- H.forAll genOperationalCertificate
  H.trippingCbor AsOperationalCertificate x

prop_roundtrip_operational_certificate_issue_counter_CBOR :: Property
prop_roundtrip_operational_certificate_issue_counter_CBOR = H.property $ do
  x <- H.forAll genOperationalCertificateIssueCounter
  H.trippingCbor AsOperationalCertificateIssueCounter x

prop_roundtrip_verification_key_byron_CBOR :: Property
prop_roundtrip_verification_key_byron_CBOR = H.property $ do
  x <- H.forAll $ genVerificationKey AsByronKey
  H.trippingCbor (AsVerificationKey AsByronKey) x

prop_roundtrip_signing_key_byron_CBOR :: Property
prop_roundtrip_signing_key_byron_CBOR = H.property $ do
  x <- H.forAll $ genSigningKey AsByronKey
  H.trippingCbor (AsSigningKey AsByronKey) x

prop_roundtrip_verification_key_payment_CBOR :: Property
prop_roundtrip_verification_key_payment_CBOR = H.property $ do
  x <- H.forAll $ genVerificationKey AsPaymentKey
  H.trippingCbor (AsVerificationKey AsPaymentKey) x

prop_roundtrip_signing_key_payment_CBOR :: Property
prop_roundtrip_signing_key_payment_CBOR = H.property $ do
  x <- H.forAll $ genSigningKey AsPaymentKey
  H.trippingCbor (AsSigningKey AsPaymentKey) x

prop_roundtrip_verification_key_stake_CBOR :: Property
prop_roundtrip_verification_key_stake_CBOR = H.property $ do
  x <- H.forAll $ genVerificationKey AsStakeKey
  H.trippingCbor (AsVerificationKey AsStakeKey) x

prop_roundtrip_signing_key_stake_CBOR :: Property
prop_roundtrip_signing_key_stake_CBOR = H.property $ do
  x <- H.forAll $ genSigningKey AsStakeKey
  H.trippingCbor (AsSigningKey AsStakeKey) x

prop_roundtrip_verification_key_genesis_CBOR :: Property
prop_roundtrip_verification_key_genesis_CBOR = H.property $ do
  x <- H.forAll $ genVerificationKey AsGenesisKey
  H.trippingCbor (AsVerificationKey AsGenesisKey) x

prop_roundtrip_signing_key_genesis_CBOR :: Property
prop_roundtrip_signing_key_genesis_CBOR = H.property $ do
  x <- H.forAll $ genSigningKey AsGenesisKey
  H.trippingCbor (AsSigningKey AsGenesisKey) x

prop_roundtrip_verification_key_genesis_delegate_CBOR :: Property
prop_roundtrip_verification_key_genesis_delegate_CBOR = H.property $ do
  x <- H.forAll $ genVerificationKey AsGenesisDelegateKey
  H.trippingCbor (AsVerificationKey AsGenesisDelegateKey) x

prop_roundtrip_signing_key_genesis_delegate_CBOR :: Property
prop_roundtrip_signing_key_genesis_delegate_CBOR = H.property $ do
  x <- H.forAll $ genSigningKey AsGenesisDelegateKey
  H.trippingCbor (AsSigningKey AsGenesisDelegateKey) x

prop_roundtrip_verification_key_stake_pool_CBOR :: Property
prop_roundtrip_verification_key_stake_pool_CBOR = H.property $ do
  x <- H.forAll $ genVerificationKey AsStakePoolKey
  H.trippingCbor (AsVerificationKey AsStakePoolKey) x

prop_roundtrip_signing_key_stake_pool_CBOR :: Property
prop_roundtrip_signing_key_stake_pool_CBOR = H.property $ do
  x <- H.forAll $ genSigningKey AsStakePoolKey
  H.trippingCbor (AsSigningKey AsStakePoolKey) x

prop_roundtrip_verification_key_vrf_CBOR :: Property
prop_roundtrip_verification_key_vrf_CBOR = H.property $ do
  x <- H.forAll $ genVerificationKey AsVrfKey
  H.trippingCbor (AsVerificationKey AsVrfKey) x

prop_roundtrip_signing_key_vrf_CBOR :: Property
prop_roundtrip_signing_key_vrf_CBOR = H.property $ do
  x <- H.forAll $ genSigningKey AsVrfKey
  H.trippingCbor (AsSigningKey AsVrfKey) x

prop_roundtrip_verification_key_kes_CBOR :: Property
prop_roundtrip_verification_key_kes_CBOR = H.property $ do
  x <- H.forAll $ genVerificationKey AsKesKey
  H.trippingCbor (AsVerificationKey AsKesKey) x

prop_roundtrip_signing_key_kes_CBOR :: Property
prop_roundtrip_signing_key_kes_CBOR = H.property $ do
  x <- H.forAll $ genSigningKey AsKesKey
  H.trippingCbor (AsSigningKey AsKesKey) x

prop_roundtrip_script_SimpleScriptV1_CBOR :: Property
prop_roundtrip_script_SimpleScriptV1_CBOR = H.property $ do
  x <- H.forAll $ genScript SimpleScriptLanguage
  H.trippingCbor (AsScript AsSimpleScript) x

prop_roundtrip_script_SimpleScriptV2_CBOR :: Property
prop_roundtrip_script_SimpleScriptV2_CBOR = H.property $ do
  x <- H.forAll $ genScript SimpleScriptLanguage
  H.trippingCbor (AsScript AsSimpleScript) x

prop_roundtrip_script_PlutusScriptV1_CBOR :: Property
prop_roundtrip_script_PlutusScriptV1_CBOR = H.property $ do
  x <- H.forAll $ genScript (PlutusScriptLanguage PlutusScriptV1)
  H.trippingCbor (AsScript AsPlutusScriptV1) x

prop_roundtrip_script_PlutusScriptV2_CBOR :: Property
prop_roundtrip_script_PlutusScriptV2_CBOR = H.property $ do
  x <- H.forAll $ genScript (PlutusScriptLanguage PlutusScriptV2)
  H.trippingCbor (AsScript AsPlutusScriptV2) x

prop_roundtrip_ScriptData_CBOR :: Property
prop_roundtrip_ScriptData_CBOR = H.property $ do
  x <- H.forAll genHashableScriptData
  H.trippingCbor AsHashableScriptData x

prop_roundtrip_UpdateProposal_CBOR :: Property
prop_roundtrip_UpdateProposal_CBOR = H.property $ do
  AnyCardanoEra era <- H.forAll $ Gen.element [minBound .. maxBound]
  proposal <- H.forAll $ genUpdateProposal era
  H.trippingCbor AsUpdateProposal proposal

prop_roundtrip_Tx_Cddl :: Property
prop_roundtrip_Tx_Cddl = H.property $ do
  AnyCardanoEra era <- H.forAll $ Gen.element [minBound..maxBound]
  x <- forAll $ genTx era
  H.tripping x serialiseTxLedgerCddl (deserialiseTxLedgerCddl era)

prop_roundtrip_TxWitness_Cddl :: Property
prop_roundtrip_TxWitness_Cddl = H.property $ do
  AnyShelleyBasedEra sbe <- H.forAll $ Gen.element [minBound..maxBound]
  x <- forAll $ genShelleyKeyWitness $ shelleyBasedToCardanoEra sbe
  tripping x (serialiseWitnessLedgerCddl sbe) (deserialiseWitnessLedgerCddl sbe)

prop_roundtrip_GovernancePoll_CBOR :: Property
prop_roundtrip_GovernancePoll_CBOR = property $ do
  trippingCbor AsGovernancePoll =<< forAll genGovernancePoll

prop_roundtrip_GovernancePollAnswer_CBOR :: Property
prop_roundtrip_GovernancePollAnswer_CBOR = property $ do
  trippingCbor AsGovernancePollAnswer =<< forAll genGovernancePollAnswer

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Test.Cardano.Api.Typed.CBOR"
  [ testProperty "roundtrip witness CBOR"                               prop_roundtrip_witness_CBOR
  , testProperty "roundtrip operational certificate CBOR"               prop_roundtrip_operational_certificate_CBOR
  , testProperty "roundtrip operational certificate issue counter CBOR" prop_roundtrip_operational_certificate_issue_counter_CBOR
  , testProperty "roundtrip verification key byron CBOR"                prop_roundtrip_verification_key_byron_CBOR
  , testProperty "roundtrip signing key byron CBOR"                     prop_roundtrip_signing_key_byron_CBOR
  , testProperty "roundtrip verification key payment CBOR"              prop_roundtrip_verification_key_payment_CBOR
  , testProperty "roundtrip signing key payment CBOR"                   prop_roundtrip_signing_key_payment_CBOR
  , testProperty "roundtrip verification key stake CBOR"                prop_roundtrip_verification_key_stake_CBOR
  , testProperty "roundtrip signing key stake CBOR"                     prop_roundtrip_signing_key_stake_CBOR
  , testProperty "roundtrip verification key genesis CBOR"              prop_roundtrip_verification_key_genesis_CBOR
  , testProperty "roundtrip signing key genesis CBOR"                   prop_roundtrip_signing_key_genesis_CBOR
  , testProperty "roundtrip verification key genesis delegate CBOR"     prop_roundtrip_verification_key_genesis_delegate_CBOR
  , testProperty "roundtrip signing key genesis delegate CBOR"          prop_roundtrip_signing_key_genesis_delegate_CBOR
  , testProperty "roundtrip verification key stake pool CBOR"           prop_roundtrip_verification_key_stake_pool_CBOR
  , testProperty "roundtrip signing key stake pool CBOR"                prop_roundtrip_signing_key_stake_pool_CBOR
  , testProperty "roundtrip verification key vrf CBOR"                  prop_roundtrip_verification_key_vrf_CBOR
  , testProperty "roundtrip signing key vrf CBOR"                       prop_roundtrip_signing_key_vrf_CBOR
  , testProperty "roundtrip verification key kes CBOR"                  prop_roundtrip_verification_key_kes_CBOR
  , testProperty "roundtrip signing key kes CBOR"                       prop_roundtrip_signing_key_kes_CBOR
  , testProperty "roundtrip script SimpleScriptV1 CBOR"                 prop_roundtrip_script_SimpleScriptV1_CBOR
  , testProperty "roundtrip script SimpleScriptV2 CBOR"                 prop_roundtrip_script_SimpleScriptV2_CBOR
  , testProperty "roundtrip script PlutusScriptV1 CBOR"                 prop_roundtrip_script_PlutusScriptV1_CBOR
  , testProperty "roundtrip script PlutusScriptV2 CBOR"                 prop_roundtrip_script_PlutusScriptV2_CBOR
  , testProperty "roundtrip UpdateProposal CBOR"                        prop_roundtrip_UpdateProposal_CBOR
  , testProperty "roundtrip ScriptData CBOR"                            prop_roundtrip_ScriptData_CBOR
  , testProperty "roundtrip txbody CBOR"                                prop_roundtrip_txbody_CBOR
  , testProperty "roundtrip Tx Cddl"                                    prop_roundtrip_Tx_Cddl
  , testProperty "roundtrip TxWitness Cddl"                             prop_roundtrip_TxWitness_Cddl
  , testProperty "roundtrip tx CBOR"                                    prop_roundtrip_tx_CBOR
  , testProperty "roundtrip GovernancePoll CBOR"                        prop_roundtrip_GovernancePoll_CBOR
  , testProperty "roundtrip GovernancePollAnswer CBOR"                  prop_roundtrip_GovernancePollAnswer_CBOR
  ]
