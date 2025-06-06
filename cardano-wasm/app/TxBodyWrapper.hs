{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module TxBodyWrapper where

import Cardano.Api
import Cardano.Api.Ledger
  ( Coin
  , EraPParams (..)
  , GovAction
  , GovActionId
  , GovActionIx
  , GovPurposeId
  , PParamsUpdate (..)
  , ProposalProcedure
  , StandardCrypto
  , StrictMaybe
  , Voter
  )
import Cardano.Api.Shelley

import Cardano.Ledger.BaseTypes (ProtVer)
import qualified Cardano.Ledger.Conway as Ledger
import qualified Cardano.Ledger.Conway.Governance as Ledger
import Cardano.Ledger.Conway.PParams (ConwayPParams (..))
import Cardano.Ledger.HKD (NoUpdate)

import Data.Aeson
  ( FromJSON
  , FromJSONKey
  , Object
  , ToJSON
  , parseJSON
  , withArray
  , withObject
  , withText
  , (.:)
  , (.:?)
  )
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Parser)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as Char8
import Data.Foldable (toList)
import Data.Functor.Identity
import Data.Map (Map)
import Data.Map.Ordered (OMap, fromList)
import Data.Set (Set)
import qualified Data.Text as Text
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

newtype TxBodyContentWrapper = TxBodyContentWrapper (TxBodyContent BuildTx ConwayEra)

deriving instance FromJSON TxBodyContentWrapper

deriving instance Generic TxBodyContentWrapper

deriving instance Generic (TxBodyContent BuildTx ConwayEra)

instance FromJSON (BabbageEraOnwards ConwayEra) where
  parseJSON = withObject "BabbageEraOnwards ConwayEra" $ \o -> do
    era <- o .: "era"
    case era :: String of
      "Conway" -> return BabbageEraOnwardsConway
      wrongEra -> fail ("Wrong era, expected Conway, but got " ++ wrongEra)

deriving instance FromJSON (TxTotalCollateral ConwayEra)

deriving instance Generic (TxTotalCollateral ConwayEra)

deriving instance FromJSON (TxBodyContent BuildTx ConwayEra)

deriving instance FromJSON (TxInsReference BuildTx ConwayEra)

deriving instance Generic (TxInsReference BuildTx ConwayEra)

instance FromJSON (BuildTxWith BuildTx (Set HashableScriptData)) where
  parseJSON = withObject "BuildTxWith BuildTx (Set HashableScriptData)" $ \v -> do
    mHashableScriptData <- v .: "hashableScriptData"
    case mHashableScriptData of
      Aeson.Null -> return $ BuildTxWith mempty
      _ -> do
        hashableScriptData <- parseJSON mHashableScriptData
        return $ BuildTxWith hashableScriptData

deriving instance
  FromJSON
    ( Featured
        ConwayEraOnwards
        ConwayEra
        (TxVotingProcedures BuildTx ConwayEra)
    )

deriving instance
  Generic
    ( Featured
        ConwayEraOnwards
        ConwayEra
        (TxVotingProcedures BuildTx ConwayEra)
    )

deriving instance FromJSON (TxVotingProcedures BuildTx ConwayEra)

deriving instance Generic (TxVotingProcedures BuildTx ConwayEra)

deriving instance
  FromJSON
    ( Ledger.VotingProcedures
        Ledger.ConwayEra
    )

deriving instance
  FromJSON
    (Ledger.VotingProcedure Ledger.ConwayEra)

deriving instance FromJSON Ledger.Vote

deriving instance FromJSONKey Voter

deriving instance FromJSON Voter

deriving instance FromJSONKey GovActionId

deriving instance FromJSON GovActionId

deriving instance FromJSON GovActionIx

instance
  FromJSON
    ( BuildTxWith
        BuildTx
        ( Map
            Voter
            (ScriptWitness WitCtxStake ConwayEra)
        )
    )
  where
  parseJSON = withObject "BuildTxWith BuildTx (Map Voter (ScriptWitness WitCtxStake ConwayEra))" $ \v -> do
    mVoters <- v .: "voters"
    case mVoters of
      Aeson.Null -> return $ BuildTxWith mempty
      _ -> do
        voters <- parseJSON mVoters
        return $ BuildTxWith voters

deriving instance FromJSON (TxScriptValidity ConwayEra)

deriving instance Generic (TxScriptValidity ConwayEra)

deriving instance FromJSON ScriptValidity

deriving instance Generic ScriptValidity

instance FromJSON (AlonzoEraOnwards ConwayEra) where
  parseJSON = withObject "AlonzoEraOnwards ConwayEra" $ \o -> do
    era <- o .: "era"
    case era :: String of
      "Conway" -> return AlonzoEraOnwardsConway
      wrongEra -> fail ("Wrong era, expected Conway, but got " ++ wrongEra)

deriving instance FromJSON (TxUpdateProposal ConwayEra)

deriving instance Generic (TxUpdateProposal ConwayEra)

deriving instance FromJSON UpdateProposal

deriving instance Generic UpdateProposal

deriving instance FromJSONKey (Hash GenesisKey)

deriving instance FromJSON ProtocolParametersUpdate

deriving instance Generic ProtocolParametersUpdate

deriving instance FromJSON CostModel

deriving instance Generic CostModel

instance FromJSON (ShelleyToBabbageEra ConwayEra) where
  parseJSON = withObject "ShelleyToBabbageEra ConwayEra" $ \o -> do
    era <- o .: "era"
    case era :: String of
      wrongEra -> fail ("Wrong era, expected Conway, but got " ++ wrongEra)

deriving instance FromJSON (TxExtraKeyWitnesses ConwayEra)

deriving instance Generic (TxExtraKeyWitnesses ConwayEra)

deriving instance FromJSON (TxAuxScripts ConwayEra)

deriving instance Generic (TxAuxScripts ConwayEra)

instance FromJSON (ScriptInEra ConwayEra) where
  parseJSON v = do
    scriptInAnyLang <- parseJSON v
    case toScriptInEra ShelleyBasedEraConway scriptInAnyLang of
      Just scriptInEra -> pure scriptInEra
      Nothing -> fail "Script language not supported in Conway era"

instance FromJSON (AllegraEraOnwards ConwayEra) where
  parseJSON = withObject "AllegraEraOnwards ConwayEra" $ \o -> do
    era <- o .: "era"
    case era :: String of
      "Conway" -> return AllegraEraOnwardsConway
      wrongEra -> fail ("Wrong era, expected Conway, but got " ++ wrongEra)

deriving instance FromJSON (TxMetadataInEra ConwayEra)

deriving instance Generic (TxMetadataInEra ConwayEra)

instance FromJSON (ShelleyBasedEra ConwayEra) where
  parseJSON = withObject "ShelleyBasedEra ConwayEra" $ \o -> do
    era <- o .: "era"
    case era :: String of
      "Conway" -> return ShelleyBasedEraConway
      wrongEra -> fail ("Wrong era, expected Conway, but got " ++ wrongEra)

deriving instance FromJSON TxMetadata

deriving instance Generic TxMetadata

deriving instance FromJSON TxMetadataValue

deriving instance Generic TxMetadataValue

-- We deserialise ByteString from a JSON string encoded in base16
instance FromJSON ByteString where
  parseJSON = withText "ByteString" $ \t ->
    case Base16.decode (Char8.pack (Text.unpack t)) of
      Left err -> fail $ "Failed to decode ByteString from base16: " ++ err
      Right bs -> return bs

deriving instance FromJSON (TxValidityUpperBound ConwayEra)

deriving instance Generic (TxValidityUpperBound ConwayEra)

deriving instance FromJSON (TxValidityLowerBound ConwayEra)

deriving instance Generic (TxValidityLowerBound ConwayEra)

deriving instance FromJSON (TxFee ConwayEra)

deriving instance Generic (TxFee ConwayEra)

deriving instance FromJSON (TxInsCollateral ConwayEra)

deriving instance Generic (TxInsCollateral ConwayEra)

deriving instance FromJSON (TxMintValue BuildTx ConwayEra)

deriving instance Generic (TxMintValue BuildTx ConwayEra)

deriving instance FromJSONKey PolicyId

deriving instance FromJSON PolicyAssets

deriving instance Generic PolicyAssets

instance FromJSON (MaryEraOnwards ConwayEra) where
  parseJSON = withObject "MaryEraOnwards ConwayEra" $ \o -> do
    era <- o .: "era"
    case era :: String of
      "Conway" -> return MaryEraOnwardsConway
      wrongEra -> fail ("Wrong era, expected Conway, but got " ++ wrongEra)

instance FromJSON (BuildTxWith BuildTx (ScriptWitness WitCtxMint ConwayEra)) where
  parseJSON v = BuildTxWith <$> parseScriptWitness WitCtxMint v

witCtxToString :: WitCtx witctx -> String
witCtxToString WitCtxMint = "WitCtxMint"
witCtxToString WitCtxStake = "WitCtxStake"
witCtxToString WitCtxTxIn = "WitCtxTxIn"

parseScriptWitness
  :: forall witctx
   . FromJSON (ScriptDatum witctx)
  => WitCtx witctx -> Aeson.Value -> Parser (ScriptWitness witctx ConwayEra)
parseScriptWitness witCtx = withObject ("BuildTxWith BuildTx (ScriptWitness WitCtxMint " ++ witCtxToString witCtx ++ ")") $ \v -> do
  (mRefTxIn :: Maybe TxIn) <- v .:? "refTxIn"
  case mRefTxIn of
    Nothing -> do
      scriptInAnyLang <- v .: "scriptInAnyLang"
      case scriptInAnyLang of
        ScriptInAnyLang SimpleScriptLanguage (SimpleScript simpleScript) ->
          return $ SimpleScriptWitness SimpleScriptInConway (SScript simpleScript)
        ScriptInAnyLang (PlutusScriptLanguage plutusLang) (PlutusScript plutusVer plutusScript) ->
          parsePlutusScript v plutusVer (PScript plutusScript)
    Just txIn -> do
      AnyScriptLanguage lang <- toEnum <$> (v .: "scriptLanguage")
      case lang of
        SimpleScriptLanguage ->
          return $ SimpleScriptWitness SimpleScriptInConway (SReferenceScript txIn)
        PlutusScriptLanguage plutusVer -> do
          parsePlutusScript v plutusVer (PReferenceScript txIn)
 where
  parsePlutusScript
    :: forall witctx lang
     . FromJSON (ScriptDatum witctx)
    => IsPlutusScriptLanguage lang
    => Object
    -> PlutusScriptVersion lang
    -> PlutusScriptOrReferenceInput lang
    -> Parser (ScriptWitness witctx ConwayEra)
  parsePlutusScript o plutusVer scriptOrRef = do
    let lang = case plutusVer of
          PlutusScriptV1 -> PlutusScriptV1InConway
          PlutusScriptV2 -> PlutusScriptV2InConway
          PlutusScriptV3 -> PlutusScriptV3InConway
    scriptDatum <- o .: "scriptDatum"
    scriptRedeemer <- o .: "scriptRedeemer"
    executionUnits <- o .: "executionUnits"
    return $
      PlutusScriptWitness lang plutusVer scriptOrRef scriptDatum scriptRedeemer executionUnits

instance FromJSON HashableScriptData where
  parseJSON v = do
    eHashableScriptData <- deserialiseFromCBOR AsHashableScriptData <$> parseJSON v
    case eHashableScriptData of
      Left err -> fail $ "Failed to deserilaise HashableScriptData: " ++ show err
      Right scriptData -> return scriptData

instance FromJSON (ScriptDatum WitCtxMint) where
  parseJSON = withObject "ScriptDatum WitCtxMint" $ \v -> do
    scriptDatumType <- v .: "scriptDatumType"
    case scriptDatumType :: String of
      "NoScriptDatumForMint" -> return NoScriptDatumForMint
      _ -> fail "Unknown script datum type for ScriptDatum WitCtxMint"

instance FromJSON (ScriptLanguageInEra PlutusScriptV1 ConwayEra) where
  parseJSON = withText "ScriptLanguageInEra PlutusScriptV1 ConwayEra" $ \s -> do
    if s == "PlutusScriptV1InConway"
      then return PlutusScriptV1InConway
      else fail "Expected 'PlutusScriptV1InConway'"

instance FromJSON (ScriptLanguageInEra PlutusScriptV2 ConwayEra) where
  parseJSON = withText "ScriptLanguageInEra PlutusScriptV2 ConwayEra" $ \s -> do
    if s == "PlutusScriptV2InConway"
      then return PlutusScriptV2InConway
      else fail "Expected 'PlutusScriptV2InConway'"

instance FromJSON (ScriptLanguageInEra PlutusScriptV3 ConwayEra) where
  parseJSON = withText "ScriptLanguageInEra PlutusScriptV3 ConwayEra" $ \s -> do
    if s == "PlutusScriptV3InConway"
      then return PlutusScriptV3InConway
      else fail "Expected 'PlutusScriptV3InConway'"

deriving instance FromJSON (TxCertificates BuildTx ConwayEra)

instance FromJSON (Certificate ConwayEra) where
  parseJSON o = do
    textEnvelope <- parseJSON o
    case deserialiseFromTextEnvelope textEnvelope of
      Left err -> fail $ "Failed to parse (Certificate ConwayEra): " ++ show err
      Right cert -> return cert

instance FromJSON (BuildTxWith BuildTx (Maybe (StakeCredential, Witness WitCtxStake ConwayEra))) where
  parseJSON = withObject "BuildTxWith BuildTx (Maybe (StakeCredential, Witness WitCtxStake ConwayEra))" $ \v -> do
    mStakeCredential <- v .: "stakeCredential"
    case mStakeCredential of
      Nothing -> return $ BuildTxWith Nothing
      Just stakeCredential -> do
        witness <- v .: "witness"
        return $ BuildTxWith (Just (stakeCredential, witness))

deriving instance FromJSON (Witness WitCtxStake ConwayEra)

deriving instance Generic (Witness WitCtxStake ConwayEra)

instance FromJSON (ScriptWitnessInCtx WitCtxStake) where
  parseJSON = withText "ScriptWitnessInCtx WitCtxStake" $ \s -> do
    if s == Text.pack "ScriptWitnessForStakeAddr"
      then return ScriptWitnessForStakeAddr
      else fail "Expected 'ScriptWitnessForStakeAddr'"

instance FromJSON (KeyWitnessInCtx WitCtxStake) where
  parseJSON = withText "KeyWitnessInCtx WitCtxStake" $ \s -> do
    if s == Text.pack "KeyWitnessForStakeAddr"
      then return KeyWitnessForStakeAddr
      else fail "Expected 'KeyWitnessInCtx WitCtxStake'"

instance FromJSON (ScriptWitness WitCtxStake ConwayEra) where
  parseJSON = parseScriptWitness WitCtxStake

instance FromJSON (ScriptDatum WitCtxStake) where
  parseJSON = withObject "ScriptDatum WitCtxStake" $ \v -> do
    scriptDatumType <- v .: "scriptDatumType"
    case scriptDatumType :: String of
      "NoScriptDatumForStake" -> return NoScriptDatumForStake
      _ -> fail "Unknown script datum type for ScriptDatum WitCtxStake"

deriving instance FromJSON StakeCredential

deriving instance Generic StakeCredential

deriving instance FromJSON (Hash StakeKey)

deriving instance Generic (Hash StakeKey)

instance (Ord a, FromJSON a, FromJSON b) => (FromJSON (OMap a b)) where
  parseJSON = withArray "OMap" $ \arr -> do
    pairs <-
      mapM
        ( withObject "OMap entry" $ \v -> do
            key <- v .: "key"
            val <- v .: "value"
            return (key, val)
        )
        arr
    return $ fromList (toList pairs)

deriving instance Generic (TxCertificates BuildTx ConwayEra)

deriving instance FromJSON (TxWithdrawals BuildTx ConwayEra)

deriving instance Generic (TxWithdrawals BuildTx ConwayEra)

instance
  FromJSON
    (BuildTxWith BuildTx (Witness WitCtxStake ConwayEra))
  where
  parseJSON = withObject "BuildTxWith BuildTx (Witness WitCtxStake ConwayEra)" $ \v -> do
    witnessType <- v .: "witnessType"
    case witnessType :: String of
      "KeyWitness" -> do
        keyWitness <- v .: "keyWitness"
        return $ BuildTxWith (KeyWitness keyWitness)
      "ScriptWitness" -> do
        witnessContext <- v .: "witnessContext"
        scriptWitness <- v .: "scriptWitness"
        return $ BuildTxWith (ScriptWitness witnessContext scriptWitness)
      _ -> fail "Unknown witness type for BuildTxWith BuildTx (Witness WitCtxStake ConwayEra)"

instance
  FromJSON
    ( BuildTxWith
        BuildTx
        ( Maybe
            ( LedgerProtocolParameters
                ConwayEra
            )
        )
    )
  where
  parseJSON = withObject "BuildTxWith BuildTx (Maybe LedgerProtocolParameters ConwayEra)" $ \v -> do
    mParams <- v .: "ledgerProtocolParameters"
    case mParams of
      Nothing -> return $ BuildTxWith Nothing
      Just params -> do
        parsedParams <- parseJSON params
        return $ BuildTxWith (Just parsedParams)

deriving instance FromJSON (LedgerProtocolParameters ConwayEra)

deriving instance Generic (LedgerProtocolParameters ConwayEra)

instance
  FromJSON
    (BuildTxWith BuildTx (Witness WitCtxTxIn ConwayEra))
  where
  parseJSON = withObject "BuildTxWith BuildTx (Witness WitCtxTxIn ConwayEra)" $ \o -> do
    witnessType <- o .: "witnessType"
    case witnessType :: String of
      "KeyWitness" -> do
        keyWitness <- o .: "keyWitness"
        return $ BuildTxWith (KeyWitness keyWitness)
      "ScriptWitness" -> do
        scriptWitnessObject <- o .: "scriptWitness"
        scriptWitness <- parseScriptWitness WitCtxTxIn scriptWitnessObject
        return $ BuildTxWith (ScriptWitness ScriptWitnessForSpending scriptWitness)
      _ -> fail "Unknown witness type for BuildTxWith BuildTx (Witness WitCtxTxIn ConwayEra)"

instance FromJSON (KeyWitnessInCtx WitCtxTxIn) where
  parseJSON = withText "KeyWitnessInCtx WitCtxTxIn" $ \s -> do
    if s == Text.pack "KeyWitnessForSpending"
      then return KeyWitnessForSpending
      else fail "Expected 'KeyWitnessForSpending'"

instance FromJSON (ScriptDatum WitCtxTxIn) where
  parseJSON = withObject "ScriptDatum WitCtxTxIn" $ \v -> do
    scriptDatumType <- v .: "scriptDatumType"
    hashableScriptData <- v .: "hashableScriptData"
    case scriptDatumType :: String of
      "NoScriptDatumForTxIn" -> return (ScriptDatumForTxIn hashableScriptData)
      _ -> fail "Unknown script datum type for ScriptDatum WitCtxTxIn"

deriving instance FromJSON (TxReturnCollateral CtxTx ConwayEra)

deriving instance Generic (TxReturnCollateral CtxTx ConwayEra)

deriving instance
  Generic
    (Featured ConwayEraOnwards ConwayEra Coin)

deriving instance
  FromJSON
    (Featured ConwayEraOnwards ConwayEra Coin)

deriving instance
  FromJSON
    (Featured ConwayEraOnwards ConwayEra (Maybe Coin))

deriving instance
  Generic
    (Featured ConwayEraOnwards ConwayEra (Maybe Coin))

instance FromJSON (ConwayEraOnwards ConwayEra) where
  parseJSON = withObject "ConwayEraOnwards ConwayEra" $ \o -> do
    era <- o .: "era"
    case era :: String of
      "Conway" -> return ConwayEraOnwardsConway
      wrongEra -> fail ("Wrong era, expected Conway, but got " ++ wrongEra)

deriving instance
  FromJSON
    ( Featured
        ConwayEraOnwards
        ConwayEra
        (TxProposalProcedures BuildTx ConwayEra)
    )

deriving instance
  Generic
    ( Featured
        ConwayEraOnwards
        ConwayEra
        (TxProposalProcedures BuildTx ConwayEra)
    )

instance
  FromJSON
    (TxProposalProcedures BuildTx ConwayEra)
  where
  parseJSON = withObject "TxProposalProcedures BuildTx ConwayEra" $ \o -> do
    procedures <- o .: "procedures"
    case procedures of
      Aeson.Null -> return TxProposalProceduresNone
      _ -> do
        proposals <- parseJSON procedures
        return $ TxProposalProcedures proposals

deriving instance FromJSON (ProposalProcedure Ledger.ConwayEra)

deriving instance FromJSON (GovAction Ledger.ConwayEra)

deriving instance
  FromJSON
    (GovPurposeId Ledger.ConstitutionPurpose Ledger.ConwayEra)

deriving instance
  FromJSON
    (GovPurposeId Ledger.CommitteePurpose Ledger.ConwayEra)

deriving instance
  FromJSON
    (GovPurposeId Ledger.PParamUpdatePurpose Ledger.ConwayEra)

deriving instance
  FromJSON
    (GovPurposeId Ledger.HardForkPurpose Ledger.ConwayEra)

deriving instance
  FromJSON
    (ConwayPParams StrictMaybe Ledger.ConwayEra)

deriving instance FromJSON (NoUpdate ProtVer)

instance
  FromJSON
    ( BuildTxWith
        BuildTx
        (Maybe (ScriptWitness WitCtxStake ConwayEra))
    )
  where
  parseJSON = withObject "BuildTxWith BuildTx (Maybe (ScriptWitness WitCtxStake ConwayEra))" $ \o -> do
    mScriptWitness <- o .: "scriptWitness"
    case mScriptWitness of
      Nothing -> return $ BuildTxWith Nothing
      Just scriptWitnessObject -> do
        scriptWitness <- parseScriptWitness WitCtxStake scriptWitnessObject
        return $ BuildTxWith (Just scriptWitness)
