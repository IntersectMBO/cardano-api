{-# LANGUAGE DataKinds #-}

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
import Cardano.Api.Experimental.AnyScriptWitness
import Cardano.Api.Experimental.Tx
import Cardano.Api.Ledger qualified as L

import Data.Map.Ordered.Strict qualified as OMap
import Data.Typeable

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

genAnyPlutusMintingScriptWitness
  :: Typeable lang => L.SLanguage lang -> Gen (AnyWitness era)
genAnyPlutusMintingScriptWitness l =
  AnyPlutusScriptWitness . AnyPlutusMintingScriptWitness <$> genPlutusScriptWitness l

genAnyPlutusWithdrawingScriptWitness
  :: Typeable lang => L.SLanguage lang -> Gen (AnyWitness era)
genAnyPlutusWithdrawingScriptWitness l =
  AnyPlutusScriptWitness . AnyPlutusWithdrawingScriptWitness <$> genPlutusScriptWitness l

genAnyPlutusCertifyingScriptWitness
  :: Typeable lang => L.SLanguage lang -> Gen (AnyWitness era)
genAnyPlutusCertifyingScriptWitness l =
  AnyPlutusScriptWitness . AnyPlutusCertifyingScriptWitness <$> genPlutusScriptWitness l

genAnyPlutusProposingScriptWitness
  :: Typeable lang => L.SLanguage lang -> Gen (AnyWitness era)
genAnyPlutusProposingScriptWitness l =
  AnyPlutusScriptWitness . AnyPlutusProposingScriptWitness <$> genPlutusScriptWitness l

genAnyPlutusVotingScriptWitness
  :: Typeable lang => L.SLanguage lang -> Gen (AnyWitness era)
genAnyPlutusVotingScriptWitness l =
  AnyPlutusScriptWitness . AnyPlutusVotingScriptWitness <$> genPlutusScriptWitness l

genAnyPlutusScriptWitness :: Typeable lang => L.SLanguage lang -> Gen (AnyWitness era)
genAnyPlutusScriptWitness l =
  Gen.choice
    [ genAnyPlutusMintingScriptWitness l
    , genAnyPlutusWithdrawingScriptWitness l
    , genAnyPlutusCertifyingScriptWitness l
    , genAnyPlutusProposingScriptWitness l
    , genAnyPlutusVotingScriptWitness l
    ]

genAnyPlutusScriptWitnessV1 :: Gen (AnyWitness era)
genAnyPlutusScriptWitnessV1 =
  genAnyPlutusScriptWitness L.SPlutusV1

genAnyPlutusScriptWitnessV2 :: Gen (AnyWitness era)
genAnyPlutusScriptWitnessV2 =
  genAnyPlutusScriptWitness L.SPlutusV2

genAnyPlutusScriptWitnessV3 :: Gen (AnyWitness era)
genAnyPlutusScriptWitnessV3 =
  genAnyPlutusScriptWitness L.SPlutusV3

genAnyPlutusScriptWitnessV4 :: Gen (AnyWitness era)
genAnyPlutusScriptWitnessV4 =
  genAnyPlutusScriptWitness L.SPlutusV4

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
