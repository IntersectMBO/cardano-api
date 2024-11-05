{-# LANGUAGE FlexibleContexts #-}

module Test.Cardano.Api.Address
  ( tests
  )
where

import           Cardano.Api
import           Cardano.Api.Address (StakeCredential (StakeCredentialByKey))

import           Control.Monad (void)
import qualified Data.Aeson as Aeson

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
  roundtrip_serialise_address AsShelleyAddress genAddressShelley

prop_roundtrip_byron_address :: Property
prop_roundtrip_byron_address =
  roundtrip_serialise_address AsByronAddress genAddressByron

prop_derive_key_from_mnemonic :: Property
prop_derive_key_from_mnemonic = H.property $ do
  ms <- H.forAll $ H.element [MS_9, MS_12, MS_15, MS_18, MS_21, MS_24]
  mnemonic <- liftIO $ generateMnemonic ms
  void $ H.evalEither $ signingStakeKeyFromMnemonic mnemonic Nothing 0 0
  H.success

prop_derivation_is_accurate :: Property
prop_derivation_is_accurate = H.propertyOnce $ do
  let mnemonic =
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
  signingKey <- H.evalEither $ signingStakeKeyFromMnemonic mnemonic Nothing 0 0
  let addr =
        serialiseToBech32 $
          makeStakeAddress Mainnet $
            StakeCredentialByKey $
              verificationKeyHash $
                getVerificationKey signingKey
  addr H.=== "stake1u97tzhttvsz5n6fej6g05trus39x5uvl0y0k56dyhsc23xcexrk27"

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
    , testProperty "address from key derivation is accurate" prop_derivation_is_accurate
    ]
