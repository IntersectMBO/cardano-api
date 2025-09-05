{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Api.Transaction.Body.Plutus.Scripts
  ( tests
  )
where

import Cardano.Api (AlonzoEraOnwards (..))
import Cardano.Api qualified as Api
import Cardano.Api.Experimental
import Cardano.Api.Experimental.Plutus
import Cardano.Api.Experimental.Tx
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Tx (setTxIns, setTxMintValue, setTxProposalProcedures)

import Cardano.Binary qualified as CBOR
import Cardano.Ledger.Alonzo.TxWits qualified as L
import Cardano.Ledger.Conway qualified as L

import Prelude

import Data.Function
import Data.List qualified as List
import Data.Map.Strict qualified as Map

import Test.Gen.Cardano.Api.Typed
  ( genIndexedPlutusScriptWitness
  , genMintWitnessable
  , genSimpleScriptMintWitness
  , genTxBodyContent
  , genWitnessable
  , genWitnessedTxIn
  )

import Test.Cardano.Api.Orphans ()

import Hedgehog
import Hedgehog.Extras qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

-- | This property checks that the redeemer pointer map is constructed correctly.
-- Previously identical script purposes were being created and overwriting each other
-- in the redeemer pointer map.
prop_getAnyWitnessRedeemerPointerMap :: Property
prop_getAnyWitnessRedeemerPointerMap = property $ do
  let eon = AlonzoEraOnwardsConway
  l <- forAll $ Gen.int (Range.linear 2 5)
  witnessables <- forAll $ Gen.list (Range.singleton l) $ genWitnessable @L.ConwayEra
  wits <-
    forAll $
      Gen.list (Range.singleton l) $
        genIndexedPlutusScriptWitness @ConwayEra
  let anyWits =
        [ AnyPlutusScriptWitness swit
        | IndexedPlutusScriptWitness _ _ swit <- wits
        ]

      zipped = zip witnessables anyWits
      expectedRedeemerPointerMapLength = length zipped
      finalWits = take expectedRedeemerPointerMapLength wits

      L.Redeemers constructedRedeemerPointerMap = getAnyWitnessRedeemerPointerMap eon zipped

  annotate "Constructed redeemer pointer map"
  annotateShow constructedRedeemerPointerMap
  let redeemerPointerMapSize = Map.size constructedRedeemerPointerMap

  cover 30 "Redeemer pointer map size more than 1" $ redeemerPointerMapSize > 1

  -- Confirm we have the expected number of redeemers
  Map.size constructedRedeemerPointerMap === expectedRedeemerPointerMapLength

  let initialRedeemers =
        [ redeemer
        | IndexedPlutusScriptWitness _ _ swit <- finalWits
        , let PlutusScriptWitness _ _ _ redeemer _ = swit
        ]

      ledgerRedeemers :: [L.Data L.ConwayEra]
      ledgerRedeemers = map fst $ Map.elems constructedRedeemerPointerMap

      convertedRedeemers = map Api.fromAlonzoData ledgerRedeemers

  annotate "Initial Indexed Script Witnesses"
  annotateShow wits

  -- Confirm we have idential redeemers
  List.sort initialRedeemers === List.sort convertedRedeemers

-- | Previously toAnyWitness was not handling simple scripts correctly resulting
-- in their exclusion in the resultant transaction.
prop_toAnyWitness :: Property
prop_toAnyWitness =
  property $ do
    let eon = AlonzoEraOnwardsConway
    l <- forAll $ Gen.int (Range.linear 2 5)
    witnessables <-
      fmap List.nub $ forAll $ Gen.list (Range.singleton l) $ genMintWitnessable @L.ConwayEra

    simpleScripts <-
      fmap List.nub $
        forAll $
          Gen.list (Range.singleton l) $
            genSimpleScriptMintWitness Api.ShelleyBasedEraConway

    let excludeReferenceScripts = filter (not . isReferenceScript) simpleScripts
        finalLength = List.length zipped
        zipped = zip witnessables excludeReferenceScripts

    res <-
      evalEither $
        legacyWitnessConversion eon $
          zip witnessables (map Api.BuildTxWith excludeReferenceScripts)

    annotateShow (extractSimpleScripts res)

    cover 30 "More than one script generated" $ length res > 1

    finalLength === length (extractSimpleScripts res) + length (extractPlutusScripts res)

-- | Confirm `extractAllIndexedPlutusScriptWitnesses` extracts all expected script
-- witnesses from a given transaction.

{-
1. Generate a TxBodyContent with at least one script witnessed tx input
2. Use extractAllIndexedPlutusScriptWitnesses
3. Convert to a transaction
4. Use ledger lenses to extract the script witnesses
5. Compare

-}

prop_extractAllIndexedPlutusScriptWitnesses :: Property
prop_extractAllIndexedPlutusScriptWitnesses =
  property $ do
    -- TODO: Need to insert this into tx body
    plutusScriptwitnessedTxIns <- forAll $ genWitnessedTxIn Api.ShelleyBasedEraConway

    txBodyContent <- forAll $ genTxBodyContent Api.ShelleyBasedEraConway
    let txBodyContentWithPlutusWitnesses =
          plutusScriptwitnessedTxIns `setTxIns` txBodyContent
            & setTxProposalProcedures Nothing
            & setTxMintValue mempty
    indexedPlutusScriptWitnesses <-
      evalEither $ extractAllIndexedPlutusScriptWitnesses ConwayEra txBodyContentWithPlutusWitnesses

    -- Create required type for createIndexedPlutusScriptWitnesses
    allGeneratedWitnesses <-
      evalEither $ toWitnessable Api.AlonzoEraOnwardsConway plutusScriptwitnessedTxIns

    let allGeneratedScriptWitnesses = createIndexedPlutusScriptWitnesses allGeneratedWitnesses
    H.note_ "All generated script witnesses"
    H.noteShow_ allGeneratedScriptWitnesses
    H.note_ "Extracted indexed plutus script witnesses"
    H.noteShow_ indexedPlutusScriptWitnesses
    length allGeneratedScriptWitnesses === length indexedPlutusScriptWitnesses

-- txBody <-
--   evalEither $ Api.createTransactionBody Api.ShelleyBasedEraConway txBodyContentWithPlutusWitnesses
-- UnsignedTx t <- evalEither $ makeUnsignedTx ConwayEra txBodyContentWithPlutusWitnesses
--
-- -- Generated redeemer pointer map
-- let generatedRedeemerPtrMap = constructRedeeemerPointerMap Api.AlonzoEraOnwardsConway indexedPlutusScriptWitnesses
--
-- -- Redeemer pointer map from transaction
-- let txRedeemerPtrMap = constructRedeeemerPointerMap Api.AlonzoEraOnwardsConway indexedPlutusScriptWitnesses
-- undefined

toWitnessable
  :: forall era
   . Api.AlonzoEraOnwards era
  -> [(Api.TxIn, Api.BuildTxWith Api.BuildTx (Api.Witness Api.WitCtxTxIn era))]
  -> Either
       CBOR.DecoderError
       [ ( Witnessable TxInItem (Api.ShelleyLedgerEra era)
         , AnyWitness (Api.ShelleyLedgerEra era)
         )
       ]
toWitnessable aeon plutusScriptwitnessedTxIns = do
  let first
        :: [ ( Witnessable TxInItem (Api.ShelleyLedgerEra era)
             , Api.BuildTxWith Api.BuildTx (Api.Witness Api.WitCtxTxIn era)
             )
           ]
      first =
        Api.alonzoEraOnwardsConstraints aeon [(WitTxIn txin, b) | (txin, b) <- plutusScriptwitnessedTxIns]
  legacyWitnessConversion
    aeon
    first

-- | We exclude reference scripts because they do not end up in the resulting transaction.
isReferenceScript :: Api.Witness witctx era -> Bool
isReferenceScript (Api.ScriptWitness _ (Api.SimpleScriptWitness _ (Api.SReferenceScript{}))) = True
isReferenceScript (Api.ScriptWitness _ (Api.PlutusScriptWitness _ _ (Api.PReferenceScript{}) _ _ _)) = False
isReferenceScript _ = False

extractSimpleScripts
  :: [(Witnessable 'MintItem era, AnyWitness era)]
  -> [SimpleScriptOrReferenceInput era]
extractSimpleScripts wits =
  [s | (_, AnySimpleScriptWitness s) <- wits]

extractPlutusScripts :: [(Witnessable witctz era, AnyWitness era)] -> [AnyWitness era]
extractPlutusScripts wits =
  [AnyPlutusScriptWitness s | (_, AnyPlutusScriptWitness s) <- wits]

tests :: TestTree
tests =
  testGroup
    "Test.Cardano.Api.Transaction.Body.Plutus.Scripts"
    [ testProperty
        "prop_extractAllIndexedPlutusScriptWitnesses"
        prop_extractAllIndexedPlutusScriptWitnesses
    , testProperty "prop_getAnyWitnessRedeemerPointerMap" prop_getAnyWitnessRedeemerPointerMap
    , testProperty "prop_toAnyWitness" prop_toAnyWitness
    ]
