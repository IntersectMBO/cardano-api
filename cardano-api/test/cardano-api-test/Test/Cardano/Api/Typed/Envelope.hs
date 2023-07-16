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

{- HLINT ignore "Use camelCase" -}

prop_roundtrip_ByronVerificationKey_envelope :: Property
prop_roundtrip_ByronVerificationKey_envelope =
  roundtrip_VerificationKey_envelope AsByronKey

prop_roundtrip_ByronSigningKey_envelope :: Property
prop_roundtrip_ByronSigningKey_envelope =
  roundtrip_SigningKey_envelope AsByronKey

prop_roundtrip_PaymentVerificationKey_envelope :: Property
prop_roundtrip_PaymentVerificationKey_envelope =
  roundtrip_VerificationKey_envelope AsPaymentKey

prop_roundtrip_PaymentSigningKey_envelope :: Property
prop_roundtrip_PaymentSigningKey_envelope =
  roundtrip_SigningKey_envelope AsPaymentKey


prop_roundtrip_StakeVerificationKey_envelope :: Property
prop_roundtrip_StakeVerificationKey_envelope =
  roundtrip_VerificationKey_envelope AsStakeKey

prop_roundtrip_StakeSigningKey_envelope :: Property
prop_roundtrip_StakeSigningKey_envelope =
  roundtrip_SigningKey_envelope AsStakeKey


prop_roundtrip_StakePoolVerificationKey_envelope :: Property
prop_roundtrip_StakePoolVerificationKey_envelope =
  roundtrip_VerificationKey_envelope AsStakePoolKey

prop_roundtrip_StakePoolSigningKey_envelope :: Property
prop_roundtrip_StakePoolSigningKey_envelope =
  roundtrip_SigningKey_envelope AsStakePoolKey


prop_roundtrip_GenesisVerificationKey_envelope :: Property
prop_roundtrip_GenesisVerificationKey_envelope =
  roundtrip_VerificationKey_envelope AsGenesisKey

prop_roundtrip_GenesisSigningKey_envelope :: Property
prop_roundtrip_GenesisSigningKey_envelope =
  roundtrip_SigningKey_envelope AsGenesisKey


prop_roundtrip_GenesisDelegateVerificationKey_envelope :: Property
prop_roundtrip_GenesisDelegateVerificationKey_envelope =
  roundtrip_VerificationKey_envelope AsGenesisDelegateKey

prop_roundtrip_GenesisDelegateSigningKey_envelope :: Property
prop_roundtrip_GenesisDelegateSigningKey_envelope =
  roundtrip_SigningKey_envelope AsGenesisDelegateKey


prop_roundtrip_KesVerificationKey_envelope :: Property
prop_roundtrip_KesVerificationKey_envelope =
  roundtrip_VerificationKey_envelope AsKesKey

prop_roundtrip_KesSigningKey_envelope :: Property
prop_roundtrip_KesSigningKey_envelope =
  roundtrip_SigningKey_envelope AsKesKey


prop_roundtrip_VrfVerificationKey_envelope :: Property
prop_roundtrip_VrfVerificationKey_envelope =
  roundtrip_VerificationKey_envelope AsVrfKey

prop_roundtrip_VrfSigningKey_envelope :: Property
prop_roundtrip_VrfSigningKey_envelope =
  roundtrip_SigningKey_envelope AsVrfKey

-- -----------------------------------------------------------------------------

roundtrip_VerificationKey_envelope ::
#if __GLASGOW_HASKELL__ >= 902
-- GHC 8.10 considers the HasTypeProxy constraint redundant but ghc-9.2 and above complains if its
-- not present.
    (Key keyrole, HasTypeProxy keyrole) =>
#else
    Key keyrole =>
#endif
    AsType keyrole -> Property
roundtrip_VerificationKey_envelope roletoken =
  H.property $ do
    vkey <- H.forAll (genVerificationKey roletoken)
    H.tripping vkey (serialiseToTextEnvelope Nothing)
                    (deserialiseFromTextEnvelope (AsVerificationKey roletoken))

roundtrip_SigningKey_envelope :: (Key keyrole,
                                  Eq (SigningKey keyrole),
                                  Show (SigningKey keyrole))
                              => AsType keyrole -> Property
roundtrip_SigningKey_envelope roletoken =
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
