{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Additional generators for TxOut JSON testing
module Test.Gen.Cardano.Api.TxOut
  ( -- * Specific Datum Type Generators
    genTxOutWithNoDatum
  , genTxOutWithDatumHash
  , genTxOutWithSupplementalDatum
  , genTxOutWithInlineDatum

    -- * Invalid JSON Generators
  , genConflictingDatumJSON
  , genMismatchedInlineDatumHashJSON
  , genPartialInlineDatumJSON
  )
where

import Cardano.Api hiding (Value)

import Data.Aeson (Value (..), object, (.=))

import Test.Gen.Cardano.Api.Typed

import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen

-- | Generate a TxOut with no datum and no reference script
genTxOutWithNoDatum
  :: ShelleyBasedEra era
  -> Gen (TxOut CtxTx era)
genTxOutWithNoDatum era =
  TxOut
    <$> genAddressInEra era
    <*> genTxOutValue era
    <*> pure TxOutDatumNone
    <*> pure ReferenceScriptNone

-- | Generate a TxOut with a datum hash (Alonzo+)
genTxOutWithDatumHash
  :: forall era
   . AlonzoEraOnwards era
  -> Gen (TxOut CtxTx era)
genTxOutWithDatumHash w =
  alonzoEraOnwardsConstraints w $
    TxOut
      <$> genAddressInEra sbe
      <*> genTxOutValue sbe
      <*> (TxOutDatumHash w <$> genHashScriptData)
      <*> genReferenceScript sbe
 where
  sbe :: ShelleyBasedEra era
  sbe = convert w

-- | Generate a TxOut with a supplemental datum (Alonzo+, CtxTx only)
genTxOutWithSupplementalDatum
  :: forall era
   . AlonzoEraOnwards era
  -> Gen (TxOut CtxTx era)
genTxOutWithSupplementalDatum w =
  alonzoEraOnwardsConstraints w $
    TxOut
      <$> genAddressInEra sbe
      <*> genTxOutValue sbe
      <*> (TxOutSupplementalDatum w <$> genHashableScriptData)
      <*> genReferenceScript sbe
 where
  sbe :: ShelleyBasedEra era
  sbe = convert w

-- | Generate a TxOut with an inline datum (Babbage+)
genTxOutWithInlineDatum
  :: forall era
   . BabbageEraOnwards era
  -> Gen (TxOut CtxTx era)
genTxOutWithInlineDatum w =
  babbageEraOnwardsConstraints w $
    TxOut
      <$> genAddressInEra sbe
      <*> genTxOutValue sbe
      <*> (TxOutDatumInline w <$> genHashableScriptData)
      <*> genReferenceScript sbe
 where
  sbe :: ShelleyBasedEra era
  sbe = convert w

-- | Generate JSON with conflicting Alonzo and Babbage datum fields
--
-- Note: Uses Conway era for address/value generation because ToJSON
-- for TxOut requires Exp.IsEra constraint (Conway+).
genConflictingDatumJSON :: Gen Value
genConflictingDatumJSON = do
  addr <- genAddressInEra ShelleyBasedEraConway
  val <- genTxOutValue ShelleyBasedEraConway
  datum1 <- genHashableScriptData
  datum2 <- genHashableScriptData
  let hash1 = hashScriptDataBytes datum1
  let hash2 = hashScriptDataBytes datum2
  pure $
    object
      [ "address" .= addr
      , "value" .= val
      , "datumhash" .= hash1
      , "datum" .= scriptDataToJson ScriptDataJsonDetailedSchema datum1
      , "inlineDatumhash" .= hash2
      , "inlineDatum" .= scriptDataToJson ScriptDataJsonDetailedSchema datum2
      ]

-- | Generate JSON with inline datum that doesn't match its hash
genMismatchedInlineDatumHashJSON :: Gen Value
genMismatchedInlineDatumHashJSON = do
  addr <- genAddressInEra ShelleyBasedEraConway
  val <- genTxOutValue ShelleyBasedEraConway
  datum <- genHashableScriptData
  wrongDatum <- Gen.filter (/= datum) genHashableScriptData
  let wrongHash = hashScriptDataBytes wrongDatum
  pure $
    object
      [ "address" .= addr
      , "value" .= val
      , "inlineDatumhash" .= wrongHash
      , "inlineDatum" .= scriptDataToJson ScriptDataJsonDetailedSchema datum
      ]

-- | Generate JSON with only partial inline datum fields
genPartialInlineDatumJSON :: Gen Value
genPartialInlineDatumJSON = do
  addr <- genAddressInEra ShelleyBasedEraConway
  val <- genTxOutValue ShelleyBasedEraConway
  datum <- genHashableScriptData
  let hash = hashScriptDataBytes datum
  Gen.choice
    [ -- Only hash, no datum
      pure $
        object
          [ "address" .= addr
          , "value" .= val
          , "inlineDatumhash" .= hash
          ]
    , -- Only datum, no hash
      pure $
        object
          [ "address" .= addr
          , "value" .= val
          , "inlineDatum" .= scriptDataToJson ScriptDataJsonDetailedSchema datum
          ]
    ]
