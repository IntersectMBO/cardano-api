{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Cardano.Api.ProtocolParameters(tests) where

import           Cardano.Api (CardanoEra (..), FromJSON, ProtocolParametersConversionError, ToJSON,
                   inEonForEra, prettyPrintJSON)
import           Cardano.Api.Eon.ShelleyBasedEra (ShelleyBasedEra (..), ShelleyLedgerEra)
import           Cardano.Api.Ledger (PParams (..))
import           Cardano.Api.ProtocolParameters (LedgerProtocolParameters (..),
                   convertToLedgerProtocolParameters, fromLedgerPParams)

import           Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as LBS

import           Test.Gen.Cardano.Api.Typed (genProtocolParameters)

import           Hedgehog (Gen, Property, forAll, property, success, (===))
import           Test.Tasty (TestTree, testGroup)
import           Test.Tasty.Hedgehog (testProperty)

tests :: TestTree
tests =
    testGroup "ProtocolParameter tests"
              [ testGroup "ToJSON instances produce the same"
                           [ testProperty "ShelleyEra" $ protocolParametersSerializeTheSame ShelleyEra
                           , testProperty "AlonzoEra" $ protocolParametersSerializeTheSame AlonzoEra
                           , testProperty "BabbageEra" $ protocolParametersSerializeTheSame BabbageEra
                           ]
              , testGroup "ProtocolParameters ToJSON can be read by PParams FromJSON"
                          [ testProperty "ShelleyEra" $ protocolParametersAreCompatible ShelleyEra
                          , testProperty "AlonzoEra" $ protocolParametersAreCompatible AlonzoEra
                          , testProperty "BabbageEra" $ protocolParametersAreCompatible BabbageEra
                          ]
              , testGroup "PParams roundtrip"
                          [ testProperty "ShelleyEra" $ ppParamsRoundtrip ShelleyEra
                          , testProperty "AlonzoEra" $ ppParamsRoundtrip AlonzoEra
                          , testProperty "BabbageEra" $ ppParamsRoundtrip BabbageEra
                          ]
              ]


-- | Ensure serialization of PParams is the same as that of ProtocolParams
protocolParametersSerializeTheSame :: forall era. ToJSON (PParams (ShelleyLedgerEra era)) => CardanoEra era -> Property
protocolParametersSerializeTheSame era =
   property $ do ValidatedSerializedPair { serializedProtocolParams
                                         , serializedPParams
                                         } <- forAll $ genValidSerializedPair era
                 serializedPParams === serializedProtocolParams

-- | Ensure that ProtocolParameter serialization can be deserialized by PParams FromJSON instance
protocolParametersAreCompatible :: forall era. ( ToJSON (PParams (ShelleyLedgerEra era))
                                               , FromJSON (PParams (ShelleyLedgerEra era))
                                               ) => CardanoEra era -> Property
protocolParametersAreCompatible era =
   property $ do ValidatedSerializedPair { serializedProtocolParams
                                         , serializedPParams = _
                                         } <- forAll $ genValidSerializedPair era
                 case eitherDecode serializedProtocolParams :: Either String (PParams (ShelleyLedgerEra era)) of
                   Left err -> fail err
                   Right _ -> success

-- | Ensure that deserializing using PParams FromJSON instance and then serializing using PParams ToJSON
-- | instance results in the same thing
ppParamsRoundtrip :: forall era. ( FromJSON (PParams (ShelleyLedgerEra era))
                                 , ToJSON (PParams (ShelleyLedgerEra era))
                                 ) => CardanoEra era -> Property
ppParamsRoundtrip era =
   property $ do ValidatedSerializedPair { serializedProtocolParams
                                         , serializedPParams = _
                                         } <- forAll $ genValidSerializedPair era
                 case eitherDecode serializedProtocolParams :: Either String (PParams (ShelleyLedgerEra era)) of
                   Left err -> fail err
                   Right pParams -> prettyPrintJSON pParams === LBS.toStrict serializedProtocolParams

-------------------------
-- Auxiliary generator --
-------------------------

data ValidatedSerializedPair era = ValidatedSerializedPair { serializedProtocolParams ::  LBS.ByteString
                                                           , serializedPParams ::  LBS.ByteString
                                                           }
  deriving Show


-- | Produces a pair of a valid ProtocolParameters and corresponding PParams by doing a round trip
genValidSerializedPair :: forall era. ToJSON (PParams (ShelleyLedgerEra era)) => CardanoEra era -> Gen (ValidatedSerializedPair era)
genValidSerializedPair era = do
  unrefinedProtocolParameters <- genProtocolParameters era
  case (do unrefinedPParams <- convertToLedgerProtocolParameters sbe unrefinedProtocolParameters :: (Either ProtocolParametersConversionError (LedgerProtocolParameters era))
           let refinedProtocolParams = fromLedgerPParams sbe $ unLedgerProtocolParameters unrefinedPParams
           refinedPParams <- convertToLedgerProtocolParameters sbe refinedProtocolParams
           return $ ValidatedSerializedPair { serializedProtocolParams = LBS.fromStrict $ prettyPrintJSON refinedProtocolParams
                                            , serializedPParams = LBS.fromStrict $ prettyPrintJSON . unLedgerProtocolParameters $ refinedPParams
                                            }) of
     Right result -> return result
     Left _ -> genValidSerializedPair era
    where
      sbe :: ShelleyBasedEra era
      sbe = toShelleyBased era

      toShelleyBased :: CardanoEra era -> ShelleyBasedEra era
      toShelleyBased = inEonForEra (error "Not a Shelley-based era") id
