module Test.Gen.Cardano.Api.Experimental
  ( genScriptWitnessedTxCertificates
  , genScriptWitnessedTxIn
  , genScriptWitnessedTxMintValue
  , genScriptWitnessedTxProposals
  , genScriptWitnesssedTxVotingProcedures
  , genScriptWitnessedTxWithdrawals
  )
where

import Cardano.Api (TxIn)
import Cardano.Api.Experimental
import Cardano.Api.Experimental.Tx
import Cardano.Api.Ledger qualified as L

import Data.Map.Ordered.Strict qualified as OMap

import Test.Gen.Cardano.Api.Typed (genExecutionUnits, genHashableScriptData, genTxIn)

import Hedgehog (Gen)
import Hedgehog.Gen qualified as Gen

genAnyWitness :: Gen (AnyWitness era)
genAnyWitness =
  Gen.choice
    [ return AnyKeyWitnessPlaceholder
    , AnySimpleScriptWitness <$> genAnySimpleScriptWitness
    , Gen.choice
        [ genAnyPlutusScriptWitnessV1
        , genAnyPlutusScriptWitnessV2
        , genAnyPlutusScriptWitnessV3
        , genAnyPlutusScriptWitnessV4
        ]
    ]

genAnyPlutusScriptWitnessV1 :: Gen (AnyWitness era)
genAnyPlutusScriptWitnessV1 =
  AnyPlutusScriptWitness <$> genPlutusScriptWitness L.SPlutusV1

genAnyPlutusScriptWitnessV2 :: Gen (AnyWitness era)
genAnyPlutusScriptWitnessV2 =
  AnyPlutusScriptWitness <$> genPlutusScriptWitness L.SPlutusV2

genAnyPlutusScriptWitnessV3 :: Gen (AnyWitness era)
genAnyPlutusScriptWitnessV3 =
  AnyPlutusScriptWitness <$> genPlutusScriptWitness L.SPlutusV3

genAnyPlutusScriptWitnessV4 :: Gen (AnyWitness era)
genAnyPlutusScriptWitnessV4 =
  AnyPlutusScriptWitness <$> genPlutusScriptWitness L.SPlutusV4

genAnySimpleScriptWitness :: Gen (SimpleScriptOrReferenceInput era)
genAnySimpleScriptWitness = SReferenceScript <$> genTxIn

-- TODO: <|> (SScript <$> genSimpleScriptWitness)

genPlutusScriptWitness :: L.SLanguage lang -> Gen (PlutusScriptWitness lang purpose era)
genPlutusScriptWitness l =
  PlutusScriptWitness l
    <$> genPlutusScript
    <*> genMaybeDatum
    <*> genHashableScriptData
    <*> genExecutionUnits

genPlutusScript :: Gen (PlutusScriptOrReferenceInput era lang)
genPlutusScript = PReferenceScript <$> genTxIn

genMaybeDatum :: Gen (PlutusScriptDatum lang purpose)
genMaybeDatum = return NoScriptDatum -- TODO: Write proper generator

genScriptWitnessedTxIn :: Gen (TxIn, AnyWitness era)
genScriptWitnessedTxIn = do
  (,) <$> genTxIn <*> genAnyWitness

genScriptWitnessedTxMintValue :: Gen (TxMintValue era)
genScriptWitnessedTxMintValue = return $ TxMintValue mempty

genScriptWitnessedTxCertificates :: Gen (TxCertificates era)
genScriptWitnessedTxCertificates = return $ TxCertificates OMap.empty

genScriptWitnessedTxWithdrawals :: Gen (TxWithdrawals era)
genScriptWitnessedTxWithdrawals = return $ TxWithdrawals mempty

genScriptWitnesssedTxVotingProcedures :: Gen (TxVotingProcedures era)
genScriptWitnesssedTxVotingProcedures = return $ TxVotingProcedures (L.VotingProcedures mempty) mempty

genScriptWitnessedTxProposals :: Gen (TxProposalProcedures era)
genScriptWitnessedTxProposals = return $ TxProposalProcedures OMap.empty
