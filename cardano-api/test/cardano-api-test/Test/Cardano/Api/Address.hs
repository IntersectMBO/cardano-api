{-# LANGUAGE FlexibleContexts #-}

module Test.Cardano.Api.Address
  ( tests
  )
where

import           Cardano.Api
import           Cardano.Api.Address (StakeCredential (StakeCredentialByKey))

import           Control.Monad (void)
import qualified Data.Aeson as Aeson
import           Data.Text (Text)
import           Data.Word (Word32)

import           Test.Gen.Cardano.Api.Typed (genAddressByron, genAddressShelley)

import           Test.Cardano.Api.Orphans ()

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Extras as H
import qualified Hedgehog.Gen as H
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

{- HLINT ignore "Use camelCase" -}

-- Address CBOR round trips

prop_roundtrip_shelley_address :: Property
prop_roundtrip_shelley_address =
  roundtrip_serialise_address (AsAddress AsShelleyAddr) genAddressShelley

prop_roundtrip_byron_address :: Property
prop_roundtrip_byron_address =
  roundtrip_serialise_address (AsAddress AsByronAddr) genAddressByron

prop_derive_key_from_mnemonic :: Property
prop_derive_key_from_mnemonic = H.property $ do
  ms <- H.forAll $ H.element [MS12, MS15, MS18, MS21, MS24]
  mnemonic <- liftIO $ generateMnemonic ms
  void $ H.evalEither $ signingKeyFromMnemonic AsStakeExtendedKey mnemonic 0 (0 :: Word32)
  H.success

exampleMnemonic :: [Text]
exampleMnemonic =
  [ "captain"
  , "kick"
  , "bundle"
  , "address"
  , "forest"
  , "cube"
  , "skirt"
  , "pepper"
  , "captain"
  , "now"
  , "crop"
  , "matrix"
  , "virus"
  , "shallow"
  , "bless"
  , "throw"
  , "spice"
  , "smoke"
  , "over"
  , "proud"
  , "minimum"
  , "coconut"
  , "virus"
  , "suspect"
  ]

prop_payment_derivation_is_accurate :: Property
prop_payment_derivation_is_accurate = H.propertyOnce $ do
  signingKey <-
    H.evalEither $ signingKeyFromMnemonic AsPaymentExtendedKey exampleMnemonic 0 (0 :: Word32)
  let verificationKey =
        getVerificationKey (signingKey :: SigningKey PaymentExtendedKey)
          :: VerificationKey PaymentExtendedKey
      addr =
        serialiseToBech32 $
          makeShelleyAddress
            Mainnet
            ( PaymentCredentialByKey $
                verificationKeyHash $
                  castVerificationKey verificationKey
            )
            NoStakeAddress
  addr H.=== "addr1v86y48z2h38ale0s9r2mfkaw7wp5ysyemnrp0azpy8z4ejg93879q"

prop_stake_derivation_is_accurate :: Property
prop_stake_derivation_is_accurate = H.propertyOnce $ do
  signingKey <-
    H.evalEither $ signingKeyFromMnemonic AsStakeExtendedKey exampleMnemonic 0 (0 :: Word32)
  let verificationKey =
        getVerificationKey (signingKey :: SigningKey StakeExtendedKey) :: VerificationKey StakeExtendedKey
      addr =
        serialiseToBech32 $
          makeStakeAddress Mainnet $
            StakeCredentialByKey $
              verificationKeyHash $
                castVerificationKey verificationKey
  addr H.=== "stake1u97tzhttvsz5n6fej6g05trus39x5uvl0y0k56dyhsc23xcexrk27"

prop_payment_with_stake_derivation_is_accurate :: Property
prop_payment_with_stake_derivation_is_accurate = H.propertyOnce $ do
  paymentSigningKey <-
    H.evalEither $ signingKeyFromMnemonic AsPaymentExtendedKey exampleMnemonic 0 (0 :: Word32)
  stakeSigningKey <-
    H.evalEither $ signingKeyFromMnemonic AsStakeExtendedKey exampleMnemonic 0 (0 :: Word32)
  let paymentVerificationKey =
        getVerificationKey (paymentSigningKey :: SigningKey PaymentExtendedKey)
          :: VerificationKey PaymentExtendedKey
      stakeVerificationKey =
        getVerificationKey (stakeSigningKey :: SigningKey StakeExtendedKey)
          :: VerificationKey StakeExtendedKey
      addr =
        serialiseToBech32 $
          makeShelleyAddress
            Mainnet
            ( PaymentCredentialByKey $
                verificationKeyHash $
                  castVerificationKey paymentVerificationKey
            )
            ( StakeAddressByValue $
                StakeCredentialByKey $
                  verificationKeyHash $
                    castVerificationKey stakeVerificationKey
            )
  addr
    H.=== "addr1q86y48z2h38ale0s9r2mfkaw7wp5ysyemnrp0azpy8z4ejtuk9wkkeq9f85nn95slgk8epz2dfce77gldf56f0ps4zds0dx2p0"

-- -----------------------------------------------------------------------------

roundtrip_serialise_address
  :: ( SerialiseAddress a
     , Eq a
     , Show a
     )
  => AsType a -> H.Gen a -> Property
roundtrip_serialise_address asType g =
  H.property $ do
    v <- H.forAll g
    H.tripping v serialiseAddress (deserialiseAddress asType)

prop_roundtrip_byron_address_JSON :: Property
prop_roundtrip_byron_address_JSON =
  H.property $ do
    mss <- H.forAll genAddressByron
    H.tripping mss Aeson.encode Aeson.eitherDecode

prop_roundtrip_shelley_address_JSON :: Property
prop_roundtrip_shelley_address_JSON =
  H.property $ do
    mss <- H.forAll genAddressShelley
    H.tripping mss Aeson.encode Aeson.eitherDecode

-- -----------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Typed.Address"
    [ testProperty "roundtrip shelley address" prop_roundtrip_shelley_address
    , testProperty "roundtrip byron address" prop_roundtrip_byron_address
    , testProperty "roundtrip byron address JSON" prop_roundtrip_byron_address_JSON
    , testProperty "roundtrip shelley address JSON" prop_roundtrip_shelley_address_JSON
    , testProperty "key derivation from random mnemonic" prop_derive_key_from_mnemonic
    , testProperty "payment address from key derivation is accurate" prop_payment_derivation_is_accurate
    , testProperty "stake address from key derivation is accurate" prop_stake_derivation_is_accurate
    , testProperty
        "payment address with stake from key derivation is accurate"
        prop_payment_with_stake_derivation_is_accurate
    ]
