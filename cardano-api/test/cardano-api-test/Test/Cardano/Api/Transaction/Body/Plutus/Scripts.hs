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
import Cardano.Api.Tx
  ( extractWitnessableCertificates
  , extractWitnessableMints
  , extractWitnessableProposals
  , extractWitnessableTxIns
  , extractWitnessableVotes
  , extractWitnessableWithdrawals
  , setTxCertificates
  , setTxIns
  , setTxMintValue
  , setTxProposalProcedures
  , setTxVotingProcedures
  , setTxWithdrawals
  )

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
  , genScriptWitnessedTxCertificates
  , genScriptWitnessedTxIn
  , genScriptWitnessedTxMintValue
  , genScriptWitnessedTxProposals
  , genScriptWitnessedTxWithdrawals
  , genScriptWitnesssedTxVotingProcedures
  , genSimpleScriptMintWitness
  , genTxBodyContent
  , genWitnessable
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
prop_extractAllIndexedPlutusScriptWitnesses :: Property
prop_extractAllIndexedPlutusScriptWitnesses =
  property $ do
    let era = ConwayEra
    -- Generate plutus script witnesses for each possible plutus purpose
    plutusScriptwitnessedTxIns <- forAll $ genScriptWitnessedTxIn era
    plutusScriptWitnessedMint <- forAll $ genScriptWitnessedTxMintValue era
    plutusScriptWitnessedTxCerts <- forAll $ genScriptWitnessedTxCertificates era
    plutusScriptWitnessesTxWithdrawals <- forAll $ genScriptWitnessedTxWithdrawals era
    plutusScriptWitnesssedTxVotingProcedures <-
      Api.mkFeatured <$> forAll (genScriptWitnesssedTxVotingProcedures era)
    plutusScriptWitnessedTxProposalProcedures <-
      Api.mkFeatured <$> forAll (genScriptWitnessedTxProposals era)
    txBodyContent <- forAll $ genTxBodyContent (Api.convert era)

    -- Populate the stripped `TxBodyContent` value with our generated plutus script
    -- witnesses
    let txBodyContentWithPlutusWitnesses =
          txBodyContent
            & setTxIns plutusScriptwitnessedTxIns
            & setTxMintValue plutusScriptWitnessedMint
            & setTxCertificates plutusScriptWitnessedTxCerts
            & setTxWithdrawals plutusScriptWitnessesTxWithdrawals
            & setTxVotingProcedures plutusScriptWitnesssedTxVotingProcedures
            & setTxProposalProcedures plutusScriptWitnessedTxProposalProcedures

    extractedPlutusScriptWitnesses <-
      evalEither $ extractAllIndexedPlutusScriptWitnesses era txBodyContentWithPlutusWitnesses

    -- This type transformation is not needed however this property test will be
    -- improved when we define an Eq instance for `AnyIndexedPlutusScriptWitness`.
    -- This necessitates changes to the experimental api so for now we settle for comparing the number
    -- of plutus script witnesses present in the `TxBodyContent`.
    generatedTxInWits <-
      evalEither $ fromLegacyTxInWitness Api.AlonzoEraOnwardsConway plutusScriptwitnessedTxIns
    generatedTxMintWits <-
      evalEither $ fromLegacyMintWitness Api.AlonzoEraOnwardsConway plutusScriptWitnessedMint

    generatedTxCertWits <-
      evalEither $ fromLegacyTxCertificates Api.AlonzoEraOnwardsConway plutusScriptWitnessedTxCerts

    generatedTxWithdrawals <-
      evalEither $ fromLegacyTxWithdrawals Api.AlonzoEraOnwardsConway plutusScriptWitnessesTxWithdrawals

    generatedTxVotingprocedures <-
      evalEither $
        fromLegacyTxVotingProcedures Api.ConwayEraOnwardsConway plutusScriptWitnesssedTxVotingProcedures

    generatedTxProposalProcedures <-
      evalEither $
        fromLegacyTxProposalProcedures Api.ConwayEraOnwardsConway plutusScriptWitnessedTxProposalProcedures

    let allGeneratedPlutusScriptWitnesses =
          mconcat
            [ createIndexedPlutusScriptWitnesses generatedTxInWits
            , createIndexedPlutusScriptWitnesses generatedTxMintWits
            , createIndexedPlutusScriptWitnesses generatedTxCertWits
            , createIndexedPlutusScriptWitnesses generatedTxWithdrawals
            , createIndexedPlutusScriptWitnesses generatedTxVotingprocedures
            , createIndexedPlutusScriptWitnesses generatedTxProposalProcedures
            ]

    H.note_ "All generated script witnesses"
    H.noteShow_ allGeneratedPlutusScriptWitnesses
    H.note_ "Extracted indexed plutus script witnesses"
    H.noteShow_ extractedPlutusScriptWitnesses

    length allGeneratedPlutusScriptWitnesses === length extractedPlutusScriptWitnesses

fromLegacyMintWitness
  :: Api.AlonzoEraOnwards era
  -> Api.TxMintValue Api.BuildTx era
  -> Either
       CBOR.DecoderError
       [ ( Witnessable MintItem (Api.ShelleyLedgerEra era)
         , AnyWitness (Api.ShelleyLedgerEra era)
         )
       ]
fromLegacyMintWitness aeon = do
  legacyWitnessConversion
    aeon
    . extractWitnessableMints aeon

fromLegacyTxCertificates
  :: forall era
   . Api.AlonzoEraOnwards era
  -> Api.TxCertificates Api.BuildTx era
  -> Either
       CBOR.DecoderError
       [ ( Witnessable CertItem (Api.ShelleyLedgerEra era)
         , AnyWitness (Api.ShelleyLedgerEra era)
         )
       ]
fromLegacyTxCertificates aeon = do
  legacyWitnessConversion
    aeon
    . extractWitnessableCertificates aeon

fromLegacyTxWithdrawals
  :: Api.AlonzoEraOnwards era
  -> Api.TxWithdrawals Api.BuildTx era
  -> Either
       CBOR.DecoderError
       [ ( Witnessable WithdrawalItem (Api.ShelleyLedgerEra era)
         , AnyWitness (Api.ShelleyLedgerEra era)
         )
       ]
fromLegacyTxWithdrawals aeon =
  legacyWitnessConversion
    aeon
    . extractWitnessableWithdrawals aeon

fromLegacyTxVotingProcedures
  :: Api.ConwayEraOnwards era
  -> Maybe
       ( Api.Featured
           eon
           era
           (Api.TxVotingProcedures Api.BuildTx era)
       )
  -> Either
       CBOR.DecoderError
       [ ( Witnessable VoterItem (Api.ShelleyLedgerEra era)
         , AnyWitness (Api.ShelleyLedgerEra era)
         )
       ]
fromLegacyTxVotingProcedures aeon = do
  legacyWitnessConversion
    (Api.convert aeon)
    . extractWitnessableVotes aeon

fromLegacyTxProposalProcedures
  :: Api.ConwayEraOnwards era
  -> Maybe
       ( Api.Featured
           eon
           era
           (Api.TxProposalProcedures Api.BuildTx era)
       )
  -> Either
       CBOR.DecoderError
       [ ( Witnessable ProposalItem (Api.ShelleyLedgerEra era)
         , AnyWitness (Api.ShelleyLedgerEra era)
         )
       ]
fromLegacyTxProposalProcedures aeon = do
  legacyWitnessConversion
    (Api.convert aeon)
    . extractWitnessableProposals aeon

fromLegacyTxInWitness
  :: Api.AlonzoEraOnwards era
  -> [(Api.TxIn, Api.BuildTxWith Api.BuildTx (Api.Witness Api.WitCtxTxIn era))]
  -> Either
       CBOR.DecoderError
       [ ( Witnessable TxInItem (Api.ShelleyLedgerEra era)
         , AnyWitness (Api.ShelleyLedgerEra era)
         )
       ]
fromLegacyTxInWitness aeon = do
  legacyWitnessConversion
    (Api.convert aeon)
    . extractWitnessableTxIns aeon

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
