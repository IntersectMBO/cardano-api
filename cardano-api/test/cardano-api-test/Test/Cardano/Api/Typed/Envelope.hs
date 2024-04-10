{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.Cardano.Api.Typed.Envelope
  ( tests
  ) where

import           Cardano.Api

import           Test.Gen.Cardano.Api.Typed

import           Test.Cardano.Api.Typed.Orphans ()

import           Hedgehog (Property)
import qualified Hedgehog as H
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

prop_roundtrip_ByronVerificationKey_envelope :: Property
prop_roundtrip_ByronVerificationKey_envelope =
  roundtripVerificationKeyEnvelope AsByronKey

prop_roundtrip_ByronSigningKey_envelope :: Property
prop_roundtrip_ByronSigningKey_envelope =
  roundtripSigningKeyEnvelope AsByronKey

prop_roundtrip_PaymentVerificationKey_envelope :: Property
prop_roundtrip_PaymentVerificationKey_envelope =
  roundtripVerificationKeyEnvelope AsPaymentKey

prop_roundtrip_PaymentSigningKey_envelope :: Property
prop_roundtrip_PaymentSigningKey_envelope =
  roundtripSigningKeyEnvelope AsPaymentKey


prop_roundtrip_StakeVerificationKey_envelope :: Property
prop_roundtrip_StakeVerificationKey_envelope =
  roundtripVerificationKeyEnvelope AsStakeKey

prop_roundtrip_StakeSigningKey_envelope :: Property
prop_roundtrip_StakeSigningKey_envelope =
  roundtripSigningKeyEnvelope AsStakeKey


prop_roundtrip_StakePoolVerificationKey_envelope :: Property
prop_roundtrip_StakePoolVerificationKey_envelope =
  roundtripVerificationKeyEnvelope AsStakePoolKey

prop_roundtrip_StakePoolSigningKey_envelope :: Property
prop_roundtrip_StakePoolSigningKey_envelope =
  roundtripSigningKeyEnvelope AsStakePoolKey


prop_roundtrip_GenesisVerificationKey_envelope :: Property
prop_roundtrip_GenesisVerificationKey_envelope =
  roundtripVerificationKeyEnvelope AsGenesisKey

prop_roundtrip_GenesisSigningKey_envelope :: Property
prop_roundtrip_GenesisSigningKey_envelope =
  roundtripSigningKeyEnvelope AsGenesisKey


prop_roundtrip_GenesisDelegateVerificationKey_envelope :: Property
prop_roundtrip_GenesisDelegateVerificationKey_envelope =
  roundtripVerificationKeyEnvelope AsGenesisDelegateKey

prop_roundtrip_GenesisDelegateSigningKey_envelope :: Property
prop_roundtrip_GenesisDelegateSigningKey_envelope =
  roundtripSigningKeyEnvelope AsGenesisDelegateKey


prop_roundtrip_KesVerificationKey_envelope :: Property
prop_roundtrip_KesVerificationKey_envelope =
  roundtripVerificationKeyEnvelope AsKesKey

prop_roundtrip_KesSigningKey_envelope :: Property
prop_roundtrip_KesSigningKey_envelope =
  roundtripSigningKeyEnvelope AsKesKey


prop_roundtrip_VrfVerificationKey_envelope :: Property
prop_roundtrip_VrfVerificationKey_envelope =
  roundtripVerificationKeyEnvelope AsVrfKey

prop_roundtrip_VrfSigningKey_envelope :: Property
prop_roundtrip_VrfSigningKey_envelope =
  roundtripSigningKeyEnvelope AsVrfKey

-- -----------------------------------------------------------------------------

roundtripVerificationKeyEnvelope :: ()
#if MIN_VERSION_base(4,17,0)
    -- GHC 8.10 considers the HasTypeProxy constraint redundant but ghc-9.2 and above complains if its
    -- not present.
    => HasTypeProxy keyrole
#endif
    => Key keyrole
    => AsType keyrole
    -> Property
roundtripVerificationKeyEnvelope roletoken =
  H.property $ do
    vkey <- H.forAll (genVerificationKey roletoken)
    H.tripping vkey (serialiseToTextEnvelope Nothing)
                    (deserialiseFromTextEnvelope (AsVerificationKey roletoken))

roundtripSigningKeyEnvelope :: (Key keyrole,
                                  Eq (SigningKey keyrole),
                                  Show (SigningKey keyrole))
                              => AsType keyrole -> Property
roundtripSigningKeyEnvelope roletoken =
  H.property $ do
    vkey <- H.forAll (genSigningKey roletoken)
    H.tripping vkey (serialiseToTextEnvelope Nothing)
                    (deserialiseFromTextEnvelope (AsSigningKey roletoken))

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Test.Cardano.Api.Typed.Envelope"
  [ testProperty "roundtrip ByronVerificationKey envelope"           prop_roundtrip_ByronVerificationKey_envelope
  , testProperty "roundtrip ByronSigningKey envelope"                prop_roundtrip_ByronSigningKey_envelope
  , testProperty "roundtrip PaymentVerificationKey envelope"         prop_roundtrip_PaymentVerificationKey_envelope
  , testProperty "roundtrip PaymentSigningKey envelope"              prop_roundtrip_PaymentSigningKey_envelope
  , testProperty "roundtrip StakeVerificationKey envelope"           prop_roundtrip_StakeVerificationKey_envelope
  , testProperty "roundtrip StakeSigningKey envelope"                prop_roundtrip_StakeSigningKey_envelope
  , testProperty "roundtrip StakePoolVerificationKey envelope"       prop_roundtrip_StakePoolVerificationKey_envelope
  , testProperty "roundtrip StakePoolSigningKey envelope"            prop_roundtrip_StakePoolSigningKey_envelope
  , testProperty "roundtrip GenesisVerificationKey envelope"         prop_roundtrip_GenesisVerificationKey_envelope
  , testProperty "roundtrip GenesisSigningKey envelope"              prop_roundtrip_GenesisSigningKey_envelope
  , testProperty "roundtrip GenesisDelegateVerificationKey envelope" prop_roundtrip_GenesisDelegateVerificationKey_envelope
  , testProperty "roundtrip GenesisDelegateSigningKey envelope"      prop_roundtrip_GenesisDelegateSigningKey_envelope
  , testProperty "roundtrip KesVerificationKey envelope"             prop_roundtrip_KesVerificationKey_envelope
  , testProperty "roundtrip KesSigningKey envelope"                  prop_roundtrip_KesSigningKey_envelope
  , testProperty "roundtrip VrfVerificationKey envelope"             prop_roundtrip_VrfVerificationKey_envelope
  , testProperty "roundtrip VrfSigningKey envelope"                  prop_roundtrip_VrfSigningKey_envelope
  ]
