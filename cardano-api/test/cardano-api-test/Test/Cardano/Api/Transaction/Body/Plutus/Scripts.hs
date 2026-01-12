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
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.AnyScript
import Cardano.Api.Experimental.AnyScriptWitness
import Cardano.Api.Experimental.Plutus hiding (AnyPlutusScript (..))
import Cardano.Api.Experimental.Tx qualified as Exp
import Cardano.Api.Ledger qualified as L

import Cardano.Ledger.Conway qualified as L
import Cardano.Ledger.Core qualified as L

import Prelude

import Data.Function
import Data.List qualified as List
import Data.Map.Ordered qualified as OMap
import Data.Map.Strict qualified as Map

import Test.Gen.Cardano.Api.Experimental qualified as Exp
import Test.Gen.Cardano.Api.Typed
  ( genIndexedPlutusScriptWitness
  , genMintWitnessable
  , genPlutusScriptInEra
  , genSimpleScriptMintWitness
  , genWitnessable
  )

import Test.Cardano.Api.Orphans ()

import Hedgehog
import Hedgehog.Extras qualified as H
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Hedgehog (testProperty)

prop_compare_plutus_script_hashes :: Property
prop_compare_plutus_script_hashes = property $ do
  scriptInEra <- forAll genPlutusScriptInEra

  let anyScript = AnyPlutusScript scriptInEra
      anyScriptHash = hashAnyScript anyScript

  let script = plutusScriptInEraToScript scriptInEra
  let hash = L.hashScript script

  hash === anyScriptHash

-- | This property checks that the redeemer pointer map is constructed correctly.
-- Previously identical script purposes were being created and overwriting each other
-- in the redeemer pointer map.
prop_getAnyWitnessRedeemerPointerMap :: Property
prop_getAnyWitnessRedeemerPointerMap = property $ do
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

      L.Redeemers constructedRedeemerPointerMap = getAnyWitnessRedeemerPointerMap zipped

  annotate "Constructed redeemer pointer map"
  annotateShow constructedRedeemerPointerMap
  let redeemerPointerMapSize = Map.size constructedRedeemerPointerMap

  cover 30 "Redeemer pointer map size more than 1" $ redeemerPointerMapSize > 1

  -- Confirm we have the expected number of redeemers
  Map.size constructedRedeemerPointerMap === expectedRedeemerPointerMapLength

  let initialRedeemers =
        [ redeemer
        | IndexedPlutusScriptWitness _ _ swit <- finalWits
        , let redeemer = getAnyPlutusScriptWitnessRedeemer swit
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
prop_extractAllIndexedPlutusScriptWitnesses :: Property
prop_extractAllIndexedPlutusScriptWitnesses =
  property $ do
    let era :: Era ConwayEra = ConwayEra
    -- Generate plutus script witnesses for each possible plutus purpose
    plutusScriptwitnessedTxIns <-
      forAll $ Gen.list (Range.linear 0 3) $ Exp.genScriptWitnessedTxIn @(LedgerEra ConwayEra)
    plutusScriptWitnessedMint <- forAll $ Exp.genScriptWitnessedTxMintValue @(LedgerEra ConwayEra)
    plutusScriptWitnessedTxCerts <- forAll $ Exp.genScriptWitnessedTxCertificates @(LedgerEra ConwayEra)
    plutusScriptWitnessesTxWithdrawals <-
      forAll $ Exp.genScriptWitnessedTxWithdrawals @(LedgerEra ConwayEra)
    plutusScriptWitnesssedTxVotingProcedures <-
      forAll $ Exp.genScriptWitnesssedTxVotingProcedures @(LedgerEra ConwayEra)
    plutusScriptWitnessedTxProposalProcedures <-
      forAll $ Exp.genScriptWitnessedTxProposals @(LedgerEra ConwayEra)

    -- Populate the stripped `TxBodyContent` value with our generated plutus script
    -- witnesses
    let txBodyContentWithPlutusWitnesses =
          Exp.defaultTxBodyContent
            & Exp.setTxIns plutusScriptwitnessedTxIns
            & Exp.setTxMintValue plutusScriptWitnessedMint
            & Exp.setTxCertificates plutusScriptWitnessedTxCerts
            & Exp.setTxWithdrawals plutusScriptWitnessesTxWithdrawals
            & Exp.setTxVotingProcedures plutusScriptWitnesssedTxVotingProcedures
            & Exp.setTxProposalProcedures plutusScriptWitnessedTxProposalProcedures

    extractedPlutusScriptWitnesses <-
      evalEither $ Exp.extractAllIndexedPlutusScriptWitnesses era txBodyContentWithPlutusWitnesses

    let generatedTxInWits = plutusScriptwitnessedTxIns
        generatedTxMintWits = plutusScriptWitnessedMint
        generatedTxCertWits = plutusScriptWitnessedTxCerts
        generatedTxWithdrawals = plutusScriptWitnessesTxWithdrawals
        generatedTxVotingprocedures = plutusScriptWitnesssedTxVotingProcedures
        generatedTxProposalProcedures = plutusScriptWitnessedTxProposalProcedures

    let allGeneratedPlutusScriptWitnesses =
          mconcat
            [ createIndexedPlutusScriptWitnesses $ [(Exp.WitTxIn tIn, sWit) | (tIn, sWit) <- generatedTxInWits]
            , createIndexedPlutusScriptWitnesses $
                [ (Exp.WitMint pid pAssets, sWit)
                | (pid, (pAssets, sWit)) <- Map.toList $ Exp.unTxMintValue generatedTxMintWits
                ]
            , createIndexedPlutusScriptWitnesses
                [ (Exp.WitTxCert c scred, wit)
                | (Certificate c, Just (scred, wit)) <-
                    OMap.toAscList $ Exp.unTxCertificates generatedTxCertWits
                ]
            , createIndexedPlutusScriptWitnesses
                [ (Exp.WitWithdrawal sAddr deposit, wit)
                | (sAddr, deposit, wit) <- Exp.unTxWithdrawals generatedTxWithdrawals
                ]
            , createIndexedPlutusScriptWitnesses
                [ (Exp.WitVote v, wit)
                | let Exp.TxVotingProcedures _ vMap = generatedTxVotingprocedures
                , (v, wit) <- Map.toList vMap
                ]
            , createIndexedPlutusScriptWitnesses
                [ (Exp.WitProposal p, wit)
                | let Exp.TxProposalProcedures pMap = generatedTxProposalProcedures
                , (p, wit) <- OMap.toAscList pMap
                ]
            ]

    H.note_ "All generated script witnesses"
    H.noteShow_ allGeneratedPlutusScriptWitnesses
    H.note_ "Extracted indexed plutus script witnesses"
    H.noteShow_ extractedPlutusScriptWitnesses

    length allGeneratedPlutusScriptWitnesses === length extractedPlutusScriptWitnesses

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
    [ testProperty "prop_compare_plutus_script_hashes" prop_compare_plutus_script_hashes
    , testProperty
        "prop_extractAllIndexedPlutusScriptWitnesses"
        prop_extractAllIndexedPlutusScriptWitnesses
    , testProperty "prop_getAnyWitnessRedeemerPointerMap" prop_getAnyWitnessRedeemerPointerMap
    , testProperty "prop_toAnyWitness" prop_toAnyWitness
    ]
