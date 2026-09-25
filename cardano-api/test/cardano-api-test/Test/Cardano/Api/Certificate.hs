{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Api.Certificate
  ( tests
  )
where

import Cardano.Api (AsType (..), DijkstraEra, deserialiseFromCBOR, serialiseToCBOR)
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.Certificate
  ( AsType (AsCertificate)
  , Certificate (..)
  , fromShelleyPoolParams
  , makeStakePoolRegistrationCertificate
  , toShelleyPoolParams
  )
import Cardano.Api.Ledger qualified as Ledger

import Data.ByteString.Short qualified as SBS
import Data.Maybe (isJust, isNothing)
import Data.MemPack.Buffer (byteArrayFromShortByteString)

import Test.Cardano.Ledger.Core.Arbitrary ()

import Hedgehog (Gen, Property, PropertyT, cover, evalEither, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Gen.QuickCheck qualified as Q
import Hedgehog.Range qualified as Range
import Test.Hedgehog.Roundtrip.CBOR qualified as H
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
  testGroup
    "Cardano.Api.Certificate"
    [ testProperty
        "prop_roundtrip_stakePoolParams"
        prop_roundtrip_stakePoolParams
    , testProperty
        "prop_roundtrip_dijkstra_registration_certificate_CBOR"
        prop_roundtrip_dijkstra_registration_certificate_CBOR
    ]

-- | Ledger stake pool parameters are unchanged by a trip through the api
-- representation, so a field that is dropped or crossed over in either
-- direction is caught.
prop_roundtrip_stakePoolParams :: Property
prop_roundtrip_stakePoolParams = property $ do
  poolParams <- forAll genStakePoolParams
  coverBlsKey poolParams
  toShelleyPoolParams (fromShelleyPoolParams poolParams) === poolParams

-- | The parameters read back out of a Dijkstra registration certificate that
-- has been through CBOR still describe the pool that was registered. Building
-- the certificate from the api representation, and comparing the decoded
-- parameters rather than the certificate itself, is what makes this catch a
-- dropped @sppBlsKey@ as well as a mis-encoded one: a certificate built without
-- the key would round-trip through CBOR quite happily.
prop_roundtrip_dijkstra_registration_certificate_CBOR :: Property
prop_roundtrip_dijkstra_registration_certificate_CBOR = property $ do
  poolParams <- forAll genStakePoolParams
  coverBlsKey poolParams
  let cert :: Certificate (Exp.LedgerEra DijkstraEra)
      cert =
        makeStakePoolRegistrationCertificate @DijkstraEra
          (toShelleyPoolParams (fromShelleyPoolParams poolParams))
  H.trippingCbor AsCertificate cert
  decodedCert <-
    evalEither $
      deserialiseFromCBOR
        (AsCertificate :: AsType (Certificate (Exp.LedgerEra DijkstraEra)))
        (serialiseToCBOR cert)
  case decodedCert of
    Certificate decoded -> Ledger.getRegPoolTxCert decoded === Just poolParams

-- | Require both the registered and the absent BLS voting key to show up, so a
-- generator that stops producing either one fails the test rather than silently
-- leaving a conversion path unexercised.
coverBlsKey :: Ledger.StakePoolParams era -> PropertyT IO ()
coverBlsKey poolParams = do
  cover 30 "with a BLS voting key" $ isJust mBlsKey
  cover 30 "without a BLS voting key" $ isNothing mBlsKey
 where
  mBlsKey = Ledger.strictMaybeToMaybe (Ledger.sppBlsKey poolParams)

-- | The ledger's own generator, with two fields replaced afterwards.
--
-- Its 'Arbitrary' instance pins @sppBlsKey@ to 'Ledger.SNothing' because the key
-- only exists from Dijkstra onwards, which is precisely the case under test here.
--
-- It also generates a metadata hash of any length, whereas on-chain the field is
-- a 32 byte blake2b-256 digest. Anything else makes 'fromShelleyPoolParams' call
-- 'error', so the hash is regenerated at the size a real certificate carries.
genStakePoolParams :: Gen (Ledger.StakePoolParams (Exp.LedgerEra DijkstraEra))
genStakePoolParams = do
  poolParams <- Q.arbitrary
  blsKey <- Gen.choice [pure Ledger.SNothing, Ledger.SJust <$> Q.arbitrary]
  metadata <- traverse genMetadataHash (Ledger.sppMetadata poolParams)
  pure poolParams{Ledger.sppBlsKey = blsKey, Ledger.sppMetadata = metadata}
 where
  genMetadataHash poolMetadata = do
    hashBytes <- Gen.bytes (Range.singleton 32)
    pure poolMetadata{Ledger.pmHash = byteArrayFromShortByteString (SBS.toShort hashBytes)}
