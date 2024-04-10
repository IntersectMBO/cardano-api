{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module Test.Cardano.Api.Typed.RawBytes
  ( tests
  ) where

import           Cardano.Api

import           Test.Gen.Cardano.Api.Typed

import           Test.Cardano.Api.Typed.Orphans ()

import           Hedgehog (Property)
import qualified Hedgehog as H
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

-- Address CBOR round trips

prop_roundtrip_shelley_address_raw :: Property
prop_roundtrip_shelley_address_raw =
  roundtripRawBytes AsShelleyAddress genAddressShelley


prop_roundtrip_byron_address_raw :: Property
prop_roundtrip_byron_address_raw =
  roundtripRawBytes AsByronAddress genAddressByron

prop_roundtrip_stake_address_raw :: Property
prop_roundtrip_stake_address_raw =
  roundtripRawBytes AsStakeAddress genStakeAddress

prop_roundtrip_script_hash_raw :: Property
prop_roundtrip_script_hash_raw =
  roundtripRawBytes AsScriptHash genScriptHash

prop_roundtrip_verification_ByronKey_hash_raw :: Property
prop_roundtrip_verification_ByronKey_hash_raw =
  roundtripVerificationKeyHashRaw AsByronKey

prop_roundtrip_verification_PaymentKey_hash_raw :: Property
prop_roundtrip_verification_PaymentKey_hash_raw =
  roundtripVerificationKeyHashRaw AsPaymentKey

prop_roundtrip_verification_StakeKey_hash_raw :: Property
prop_roundtrip_verification_StakeKey_hash_raw =
  roundtripVerificationKeyHashRaw AsStakeKey

prop_roundtrip_verification_StakePoolKey_hash_raw :: Property
prop_roundtrip_verification_StakePoolKey_hash_raw =
  roundtripVerificationKeyHashRaw AsStakePoolKey

prop_roundtrip_verification_GenesisKey_hash_raw :: Property
prop_roundtrip_verification_GenesisKey_hash_raw =
  roundtripVerificationKeyHashRaw AsGenesisKey

prop_roundtrip_verification_GenesisDelegateKey_hash_raw :: Property
prop_roundtrip_verification_GenesisDelegateKey_hash_raw =
  roundtripVerificationKeyHashRaw AsGenesisDelegateKey

prop_roundtrip_verification_KesKey_hash_raw :: Property
prop_roundtrip_verification_KesKey_hash_raw =
  roundtripVerificationKeyHashRaw AsKesKey

prop_roundtrip_verification_VrfKey_hash_raw :: Property
prop_roundtrip_verification_VrfKey_hash_raw =
  roundtripVerificationKeyHashRaw AsVrfKey

prop_roundtrip_verification_GenesisUTxOKey_hash_raw :: Property
prop_roundtrip_verification_GenesisUTxOKey_hash_raw =
  roundtripVerificationKeyHashRaw AsGenesisUTxOKey

-- -----------------------------------------------------------------------------

roundtripRawBytes
  :: ( SerialiseAsRawBytes a
     , Eq a
     , Show a) => AsType a -> H.Gen a -> Property
roundtripRawBytes asType g =
  H.property $ do
    v <- H.forAll g
    H.tripping v serialiseToRawBytes (deserialiseFromRawBytes asType)

roundtripVerificationKeyHashRaw :: ()
#if MIN_VERSION_base(4,17,0)
  -- GHC 9.2 and above needs an extra constraint.
  => HasTypeProxy keyrole
#endif
  => Key keyrole
  => Eq (Hash keyrole)
  => Show (Hash keyrole)
  => AsType keyrole
  -> Property
roundtripVerificationKeyHashRaw roletoken =
  H.property $ do
    vKey <- H.forAll $ genVerificationKey roletoken
    let vKeyHash = verificationKeyHash vKey
    H.tripping vKeyHash serialiseToRawBytes (deserialiseFromRawBytes (AsHash roletoken))

-- -----------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Test.Cardano.Api.Typed.RawBytes"
  [ testProperty "roundtrip shelley address raw"                      prop_roundtrip_shelley_address_raw
  , testProperty "roundtrip byron address raw"                        prop_roundtrip_byron_address_raw
  , testProperty "roundtrip stake address raw"                        prop_roundtrip_stake_address_raw
  , testProperty "roundtrip script hash raw"                          prop_roundtrip_script_hash_raw
  , testProperty "roundtrip verification ByronKey hash raw"           prop_roundtrip_verification_ByronKey_hash_raw
  , testProperty "roundtrip verification PaymentKey hash raw"         prop_roundtrip_verification_PaymentKey_hash_raw
  , testProperty "roundtrip verification StakeKey hash raw"           prop_roundtrip_verification_StakeKey_hash_raw
  , testProperty "roundtrip verification StakePoolKey hash raw"       prop_roundtrip_verification_StakePoolKey_hash_raw
  , testProperty "roundtrip verification GenesisKey hash raw"         prop_roundtrip_verification_GenesisKey_hash_raw
  , testProperty "roundtrip verification GenesisDelegateKey hash raw" prop_roundtrip_verification_GenesisDelegateKey_hash_raw
  , testProperty "roundtrip verification KesKey hash raw"             prop_roundtrip_verification_KesKey_hash_raw
  , testProperty "roundtrip verification VrfKey hash raw"             prop_roundtrip_verification_VrfKey_hash_raw
  , testProperty "roundtrip verification GenesisUTxOKey hash raw"     prop_roundtrip_verification_GenesisUTxOKey_hash_raw
  ]
