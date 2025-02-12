{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
-- TODO remove when serialiseTxLedgerCddl is removed
{-# OPTIONS_GHC -Wno-deprecations #-}

module Test.Cardano.Api.CBOR
  ( tests
  )
where

import           Cardano.Api
import           Cardano.Api.Internal.Script
import           Cardano.Api.Internal.SerialiseLedgerCddl (cddlTypeToEra)
import           Cardano.Api.Internal.SerialiseTextEnvelope (TextEnvelopeDescr (TextEnvelopeDescr))
import           Cardano.Api.Shelley (AsType (..))

import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Short as SBS
import           Data.Proxy (Proxy (..))
import qualified Data.Text as T

import           Test.Gen.Cardano.Api.Hardcoded
import           Test.Gen.Cardano.Api.Typed

import           Test.Cardano.Api.Orphans ()

import           Hedgehog (Property, forAll, property, tripping)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Gen as Gen
import qualified Test.Hedgehog.Roundtrip.CBOR as H
import           Test.Hedgehog.Roundtrip.CBOR
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

{- HLINT ignore "Use camelCase" -}

-- TODO: Need to add PaymentExtendedKey roundtrip tests however
-- we can't derive an Eq instance for Crypto.HD.XPrv

prop_text_envelope_roundtrip_txbody_CBOR :: Property
prop_text_envelope_roundtrip_txbody_CBOR = H.property $ do
  AnyShelleyBasedEra era <- H.noteShowM . H.forAll $ Gen.element [minBound .. maxBound]
  x <- H.forAll $ makeSignedTransaction [] . fst <$> genValidTxBody era
  shelleyBasedEraConstraints
    era
    ( H.tripping
        x
        (serialiseToTextEnvelope (Just (TextEnvelopeDescr "Ledger Cddl Format")))
        (deserialiseFromTextEnvelope (shelleyBasedEraConstraints era $ proxyToAsType Proxy))
    )

prop_text_envelope_roundtrip_tx_CBOR :: Property
prop_text_envelope_roundtrip_tx_CBOR = H.property $ do
  AnyShelleyBasedEra era <- H.noteShowM . H.forAll $ Gen.element [minBound .. maxBound]
  x <- H.forAll $ genTx era
  shelleyBasedEraConstraints
    era
    ( H.tripping
        x
        (serialiseToTextEnvelope (Just (TextEnvelopeDescr "Ledger Cddl Format")))
        (deserialiseFromTextEnvelope (shelleyBasedEraConstraints era $ proxyToAsType Proxy))
    )

prop_roundtrip_tx_CBOR :: Property
prop_roundtrip_tx_CBOR = H.property $ do
  AnyShelleyBasedEra era <- H.noteShowM . H.forAll $ Gen.element [minBound .. maxBound]
  x <- H.forAll $ genTx era
  shelleyBasedEraConstraints era $ H.trippingCbor (proxyToAsType Proxy) x

prop_roundtrip_witness_CBOR :: Property
prop_roundtrip_witness_CBOR = H.property $ do
  AnyShelleyBasedEra era <- H.noteShowM . H.forAll $ Gen.element [minBound .. maxBound]
  x <- H.forAll $ genCardanoKeyWitness era
  shelleyBasedEraConstraints era $ H.trippingCbor (AsKeyWitness (proxyToAsType Proxy)) x

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

{-
Plutus CBOR Encoding tests - Double decoding fix

Because the SerialiseAsCBOR instance for 'Plutus lang' was double encoding the plutus script bytes
we need to confirm that the removal of this double encoding does not break backwards compatibility.

The double encoding took the form of encoding the plutus script bytes as a CBOR in CBOR bytestring.
This has cropped up a number of times from users who wanted to directly use the plutus script payload
generated by the cardano-cli but first had to run a CBOR deserialization step on the bytes before they
could access the (unadultered) plutus script bytes.

As such we need to confirm the following:
1. Deserializing double encoded script bytes and "normal" script bytes deserialise to the same byte sequence.
2. The resulting bytes are both valid plutus scripts (confirmed via PlutusScriptInEra which calls ledger
   functions to confirm the validity of the plutus bytes).
3. The updated SerialiseAsCBOR instance for 'Plutus lang' does not double encode the plutus script bytes.

How are these properties confirmed?

- 1. and 2. are confirmed by the decodeOnlyPlutusScriptBytes tests.
- 3. Is confirmed by a roundtrip test using non-double endcoded plutus script bytes.

Note that the SerialiseAsCBOR instances for 'Plutus lang' and `PlutusScript lang` will forever
be asymmetric with respect to double encoded plutus scripts.
So CBOR roundtrip tests are not expected to pass in the double encoded plutus script case.
-}

-- This property will succeed because the bytes are not double encoded.
-- This property confirms that when it comes to non double encoded plutus script bytes
-- the SerialiseAsCBOR instance for 'Plutus lang' (and therefore Script lang) is symmetric.
prop_roundtrip_non_double_encoded_always_succeeds_plutus_V3_CBOR :: Property
prop_roundtrip_non_double_encoded_always_succeeds_plutus_V3_CBOR = H.property $ do
  let alwaysSucceedsUnwrapped = PlutusScriptSerialised $ SBS.toShort $ Base16.decodeLenient "450101002499"
  H.trippingCbor
    (AsPlutusScriptInEra @ConwayEra AsPlutusScriptV3)
    (PlutusScriptInEra alwaysSucceedsUnwrapped)

prop_decode_only_double_wrapped_plutus_script_bytes_CBOR :: Property
prop_decode_only_double_wrapped_plutus_script_bytes_CBOR = H.property $ do
  let alwaysSucceedsDoubleEncoded = Base16.decodeLenient "46450101002499"
  decodeOnlyPlutusScriptBytes
    ShelleyBasedEraConway
    PlutusScriptV3
    alwaysSucceedsDoubleEncoded
    (AsScript AsPlutusScriptV3)

prop_decode_only_wrapped_plutus_script_V1_CBOR :: Property
prop_decode_only_wrapped_plutus_script_V1_CBOR = H.property $ do
  PlutusScriptSerialised shortBs <- H.forAll $ genPlutusScript PlutusScriptV1
  decodeOnlyPlutusScriptBytes
    ShelleyBasedEraConway
    PlutusScriptV1
    (SBS.fromShort shortBs)
    (AsScript AsPlutusScriptV1)

prop_decode_only_wrapped_plutus_script_V2_CBOR :: Property
prop_decode_only_wrapped_plutus_script_V2_CBOR = H.property $ do
  PlutusScriptSerialised shortBs <- H.forAll $ genPlutusScript PlutusScriptV2
  decodeOnlyPlutusScriptBytes
    ShelleyBasedEraConway
    PlutusScriptV2
    (SBS.fromShort shortBs)
    (AsScript AsPlutusScriptV2)

prop_decode_only_wrapped_plutus_script_V3_CBOR :: Property
prop_decode_only_wrapped_plutus_script_V3_CBOR = H.property $ do
  PlutusScriptSerialised shortBs <- H.forAll $ genPlutusScript PlutusScriptV3
  decodeOnlyPlutusScriptBytes
    ShelleyBasedEraConway
    PlutusScriptV3
    (SBS.fromShort shortBs)
    (AsScript AsPlutusScriptV3)

prop_double_encoded_sanity_check :: Property
prop_double_encoded_sanity_check = H.propertyOnce $ do
  let fixed = removePlutusScriptDoubleEncoding exampleDoubleEncodedBytes

  LBS.fromStrict fixed H./== exampleDoubleEncodedBytes

prop_roundtrip_ScriptData_CBOR :: Property
prop_roundtrip_ScriptData_CBOR = H.property $ do
  x <- H.forAll genHashableScriptData
  H.trippingCbor AsHashableScriptData x

prop_roundtrip_UpdateProposal_CBOR :: Property
prop_roundtrip_UpdateProposal_CBOR = H.property $ do
  AnyCardanoEra era <- H.noteShowM . H.forAll $ Gen.element [minBound .. maxBound]
  proposal <- H.forAll $ genUpdateProposal era
  H.trippingCbor AsUpdateProposal proposal

prop_Tx_cddlTypeToEra :: Property
prop_Tx_cddlTypeToEra = H.property $ do
  AnyShelleyBasedEra era <- H.noteShowM . H.forAll $ Gen.element [minBound .. maxBound]
  x <- forAll $ genTx era
  shelleyBasedEraConstraints era $ do
    let TextEnvelopeType d = textEnvelopeType (proxyToAsType (getProxy x))
    H.note_ $ "Envelope type: " <> show d
    cddlTypeToEra (T.pack d) H.=== Right (AnyShelleyBasedEra era)
 where
  getProxy :: forall a. a -> Proxy a
  getProxy _ = Proxy

prop_TxWitness_cddlTypeToEra :: Property
prop_TxWitness_cddlTypeToEra = H.property $ do
  AnyShelleyBasedEra era <- H.noteShowM . H.forAll $ Gen.element [minBound .. maxBound]
  x <- forAll $ genCardanoKeyWitness era
  shelleyBasedEraConstraints era $ do
    let TextEnvelopeType d = textEnvelopeType (proxyToAsType (getProxy x))
    H.note_ $ "Envelope type: " <> show d
    cddlTypeToEra (T.pack d) H.=== Right (AnyShelleyBasedEra era)
 where
  getProxy :: forall a. a -> Proxy a
  getProxy _ = Proxy

prop_roundtrip_TxWitness_Cddl :: Property
prop_roundtrip_TxWitness_Cddl = H.property $ do
  AnyShelleyBasedEra sbe <- H.noteShowM . H.forAll $ Gen.element [minBound .. maxBound]
  x <- forAll $ genShelleyKeyWitness sbe
  tripping x (serialiseWitnessLedgerCddl sbe) (deserialiseWitnessLedgerCddl sbe)

prop_roundtrip_GovernancePoll_CBOR :: Property
prop_roundtrip_GovernancePoll_CBOR = property $ do
  trippingCbor AsGovernancePoll =<< forAll genGovernancePoll

prop_roundtrip_GovernancePollAnswer_CBOR :: Property
prop_roundtrip_GovernancePollAnswer_CBOR = property $ do
  trippingCbor AsGovernancePollAnswer =<< forAll genGovernancePollAnswer

-- -----------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Typed.CBOR"
    [ testProperty "rountrip txbody text envelope" prop_text_envelope_roundtrip_txbody_CBOR
    , testProperty "rountrip tx text envelope" prop_text_envelope_roundtrip_tx_CBOR
    , testProperty "roundtrip witness CBOR" prop_roundtrip_witness_CBOR
    , testProperty
        "roundtrip operational certificate CBOR"
        prop_roundtrip_operational_certificate_CBOR
    , testProperty
        "roundtrip operational certificate issue counter CBOR"
        prop_roundtrip_operational_certificate_issue_counter_CBOR
    , testProperty
        "roundtrip verification key byron CBOR"
        prop_roundtrip_verification_key_byron_CBOR
    , testProperty
        "roundtrip signing key byron CBOR"
        prop_roundtrip_signing_key_byron_CBOR
    , testProperty
        "roundtrip verification key payment CBOR"
        prop_roundtrip_verification_key_payment_CBOR
    , testProperty
        "roundtrip signing key payment CBOR"
        prop_roundtrip_signing_key_payment_CBOR
    , testProperty
        "roundtrip verification key stake CBOR"
        prop_roundtrip_verification_key_stake_CBOR
    , testProperty
        "roundtrip signing key stake CBOR"
        prop_roundtrip_signing_key_stake_CBOR
    , testProperty
        "roundtrip verification key genesis CBOR"
        prop_roundtrip_verification_key_genesis_CBOR
    , testProperty
        "roundtrip signing key genesis CBOR"
        prop_roundtrip_signing_key_genesis_CBOR
    , testProperty
        "roundtrip verification key genesis delegate CBOR"
        prop_roundtrip_verification_key_genesis_delegate_CBOR
    , testProperty
        "roundtrip signing key genesis delegate CBOR"
        prop_roundtrip_signing_key_genesis_delegate_CBOR
    , testProperty
        "roundtrip verification key stake pool CBOR"
        prop_roundtrip_verification_key_stake_pool_CBOR
    , testProperty
        "roundtrip signing key stake pool CBOR"
        prop_roundtrip_signing_key_stake_pool_CBOR
    , testProperty
        "roundtrip verification key vrf CBOR"
        prop_roundtrip_verification_key_vrf_CBOR
    , testProperty
        "roundtrip signing key vrf CBOR"
        prop_roundtrip_signing_key_vrf_CBOR
    , testProperty
        "roundtrip verification key kes CBOR"
        prop_roundtrip_verification_key_kes_CBOR
    , testProperty
        "roundtrip signing key kes CBOR"
        prop_roundtrip_signing_key_kes_CBOR
    , testProperty
        "roundtrip script SimpleScriptV1 CBOR"
        prop_roundtrip_script_SimpleScriptV1_CBOR
    , testProperty
        "roundtrip script SimpleScriptV2 CBOR"
        prop_roundtrip_script_SimpleScriptV2_CBOR
    , testProperty
        "roundtrip non double encoded always succeeds plutus V3 CBOR"
        prop_roundtrip_non_double_encoded_always_succeeds_plutus_V3_CBOR
    , testProperty
        "decode only double wrapped plutus script bytes CBOR"
        prop_decode_only_double_wrapped_plutus_script_bytes_CBOR
    , testProperty
        "decode only wrapped plutus script V1 CBOR"
        prop_decode_only_wrapped_plutus_script_V1_CBOR
    , testProperty
        "decode only wrapped plutus script V2 CBOR"
        prop_decode_only_wrapped_plutus_script_V2_CBOR
    , testProperty
        "decode only wrapped plutus script V3 CBOR"
        prop_decode_only_wrapped_plutus_script_V3_CBOR
    , testProperty
        "double encoded sanity check"
        prop_double_encoded_sanity_check
    , testProperty
        "cddlTypeToEra for Tx types"
        prop_Tx_cddlTypeToEra
    , testProperty
        "cddlTypeToEra for TxWitness types"
        prop_TxWitness_cddlTypeToEra
    , testProperty
        "roundtrip UpdateProposal CBOR"
        prop_roundtrip_UpdateProposal_CBOR
    , testProperty "roundtrip ScriptData CBOR" prop_roundtrip_ScriptData_CBOR
    , testProperty "roundtrip TxWitness Cddl" prop_roundtrip_TxWitness_Cddl
    , testProperty "roundtrip tx CBOR" prop_roundtrip_tx_CBOR
    , testProperty
        "roundtrip GovernancePoll CBOR"
        prop_roundtrip_GovernancePoll_CBOR
    , testProperty
        "roundtrip GovernancePollAnswer CBOR"
        prop_roundtrip_GovernancePollAnswer_CBOR
    ]
