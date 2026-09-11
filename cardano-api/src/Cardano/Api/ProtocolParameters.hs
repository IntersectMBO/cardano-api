{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

-- | The various Cardano protocol parameters, including:
--
-- * updates to protocol parameters: 'ProtocolParametersUpdate'
-- * update proposals that can be embedded in transactions: 'UpdateProposal'
-- * parameters fixed in the genesis file: 'GenesisParameters'
module Cardano.Api.ProtocolParameters
  ( -- * The updatable protocol parameters
    EpochNo

    -- * The updatable protocol parameters
  , LedgerProtocolParameters (..)
  , EraBasedProtocolParametersUpdate (..)
  , AlonzoOnwardsPParams (..)
  , CommonProtocolParametersUpdate (..)
  , DeprecatedAfterBabbagePParams (..)
  , DeprecatedAfterMaryPParams (..)
  , ShelleyToAlonzoPParams (..)
  , IntroducedInBabbagePParams (..)
  , IntroducedInConwayPParams (..)
  , IntroducedInDijkstraPParams (..)
  , createEraBasedProtocolParamUpdate
  , createPParams

    -- * Errors
  , ProtocolParametersError (..)
  , ProtocolParametersConversionError (..)

    -- * PraosNonce
  , PraosNonce
  , makePraosNonce

    -- * Execution units, prices and cost models,
  , ExecutionUnits (..)
  , ExecutionUnitPrices (..)
  , CostModels (..)
  , CostModel (..)
  , fromAlonzoCostModels

    -- * Update proposals to change the protocol parameters
  , UpdateProposal (..)
  , makeShelleyUpdateProposal

    -- * Internal conversion functions
  , toLedgerNonce
  , toLedgerUpdate
  , fromLedgerUpdate
  , toLedgerProposedPPUpdates
  , fromLedgerProposedPPUpdates
  , fromLedgerPParamsUpdate
  , toAlonzoPrices
  , fromAlonzoPrices
  , toAlonzoScriptLanguage
  , fromAlonzoScriptLanguage
  , toAlonzoCostModel
  , fromAlonzoCostModel
  , toAlonzoCostModels

    -- * Data family instances
  , AsType (..)

    -- ** Era-dependent protocol features
  )
where

import Cardano.Api.Address
import Cardano.Api.Byron.Internal.Key
import Cardano.Api.Certificate.Internal.StakePoolMetadata
import Cardano.Api.Compatible.ProtocolParametersUpdate
import Cardano.Api.Era
import Cardano.Api.Error
import Cardano.Api.HasTypeProxy
import Cardano.Api.Internal.Orphans ()
import Cardano.Api.Plutus.Internal.Script
import Cardano.Api.Pretty
import Cardano.Api.Serialise.Cbor
import Cardano.Api.Serialise.Json (toRationalJSON)
import Cardano.Api.Serialise.Raw
import Cardano.Api.Serialise.SerialiseUsing
import Cardano.Api.Tx.Internal.TxMetadata
import Cardano.Api.Value.Internal

import Cardano.Binary qualified as CBOR
import Cardano.Crypto.Hash qualified as Hash
import Cardano.Crypto.Hash.Class qualified as Crypto
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo
import Cardano.Ledger.Babbage.Core qualified as Ledger
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Hashes (HASH)
import Cardano.Ledger.Plutus.CostModels qualified as Plutus
import Cardano.Ledger.Plutus.Language qualified as Plutus
import Cardano.Slotting.Slot (EpochNo (..))
import PlutusLedgerApi.Common (CostModelApplyError)

import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , object
  , withObject
  , (.:)
  , (.=)
  )
import Data.Bifunctor (bimap, first)
import Data.ByteString (ByteString)
import Data.Data (Data)
import Data.Either.Combinators (maybeToRight)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import GHC.Exts (IsList (..))
import GHC.Generics
import Numeric.Natural
import Text.PrettyBy.Default (display)

-- -----------------------------------------------------------------------------
-- Era based ledger protocol parameters
--
newtype LedgerProtocolParameters era = LedgerProtocolParameters
  { unLedgerProtocolParameters :: Ledger.PParams (ShelleyLedgerEra era)
  }

instance IsShelleyBasedEra era => Show (LedgerProtocolParameters era) where
  show (LedgerProtocolParameters pp) =
    shelleyBasedEraConstraints (shelleyBasedEra @era) $
      show pp

instance IsShelleyBasedEra era => Eq (LedgerProtocolParameters era) where
  LedgerProtocolParameters a == LedgerProtocolParameters b =
    shelleyBasedEraConstraints (shelleyBasedEra @era) $
      a == b

-- ----------------------------------------------------------------------------
-- Praos nonce
--

newtype PraosNonce = PraosNonce {unPraosNonce :: Hash.Hash HASH ByteString}
  deriving stock (Eq, Ord, Generic)
  deriving (Show, Pretty) via UsingRawBytesHex PraosNonce
  deriving (ToJSON, FromJSON) via UsingRawBytesHex PraosNonce
  deriving (ToCBOR, FromCBOR) via UsingRawBytes PraosNonce

instance HasTypeProxy PraosNonce where
  data AsType PraosNonce = AsPraosNonce
  proxyToAsType _ = AsPraosNonce

instance SerialiseAsRawBytes PraosNonce where
  serialiseToRawBytes (PraosNonce h) =
    Crypto.hashToBytes h

  deserialiseFromRawBytes AsPraosNonce bs =
    maybeToRight (SerialiseAsRawBytesError "Unable to deserialise PraosNonce") $
      PraosNonce <$> Crypto.hashFromBytes bs

makePraosNonce :: ByteString -> PraosNonce
makePraosNonce = PraosNonce . Crypto.hashWith id

toLedgerNonce :: Maybe PraosNonce -> Ledger.Nonce
toLedgerNonce Nothing = Ledger.NeutralNonce
toLedgerNonce (Just (PraosNonce h)) = Ledger.Nonce (Crypto.castHash h)

-- ----------------------------------------------------------------------------
-- Script execution unit prices and cost models
--

-- | The prices for 'ExecutionUnits' as a fraction of a 'L.Coin'.
--
-- These are used to determine the fee for the use of a script within a
-- transaction, based on the 'ExecutionUnits' needed by the use of the script.
data ExecutionUnitPrices
  = ExecutionUnitPrices
  { priceExecutionSteps :: Rational
  , priceExecutionMemory :: Rational
  }
  deriving (Eq, Show)

instance ToCBOR ExecutionUnitPrices where
  toCBOR ExecutionUnitPrices{priceExecutionSteps, priceExecutionMemory} =
    CBOR.encodeListLen 2
      <> toCBOR priceExecutionSteps
      <> toCBOR priceExecutionMemory

instance FromCBOR ExecutionUnitPrices where
  fromCBOR = do
    CBOR.enforceSize "ExecutionUnitPrices" 2
    ExecutionUnitPrices
      <$> fromCBOR
      <*> fromCBOR

instance ToJSON ExecutionUnitPrices where
  toJSON ExecutionUnitPrices{priceExecutionSteps, priceExecutionMemory} =
    object
      [ "priceSteps" .= toRationalJSON priceExecutionSteps
      , "priceMemory" .= toRationalJSON priceExecutionMemory
      ]

instance FromJSON ExecutionUnitPrices where
  parseJSON =
    withObject "ExecutionUnitPrices" $ \o ->
      ExecutionUnitPrices
        <$> o .: "priceSteps"
        <*> o .: "priceMemory"

toAlonzoPrices :: ExecutionUnitPrices -> Either ProtocolParametersConversionError Alonzo.Prices
toAlonzoPrices
  ExecutionUnitPrices
    { priceExecutionSteps
    , priceExecutionMemory
    } = do
    prSteps <- boundRationalEither "Steps" priceExecutionSteps
    prMem <- boundRationalEither "Mem" priceExecutionMemory
    return
      Alonzo.Prices
        { Alonzo.prSteps
        , Alonzo.prMem
        }

fromAlonzoPrices :: Alonzo.Prices -> ExecutionUnitPrices
fromAlonzoPrices Alonzo.Prices{Alonzo.prSteps, Alonzo.prMem} =
  ExecutionUnitPrices
    { priceExecutionSteps = Ledger.unboundRational prSteps
    , priceExecutionMemory = Ledger.unboundRational prMem
    }

-- ----------------------------------------------------------------------------
-- Script cost models
--

newtype CostModel = CostModel [Int64]
  deriving (Eq, Show, Data)
  deriving newtype (ToCBOR, FromCBOR)

newtype CostModels = CostModels {unCostModels :: Map AnyPlutusScriptVersion CostModel}
  deriving (Eq, Show)

instance FromJSON CostModels where
  parseJSON v = CostModels . fromAlonzoCostModels <$> parseJSON v

instance ToJSON CostModels where
  toJSON (CostModels costModels) =
    case toAlonzoCostModels costModels of
      Left err -> error $ displayError err
      Right ledgerCostModels -> toJSON ledgerCostModels

toAlonzoCostModels
  :: Map AnyPlutusScriptVersion CostModel
  -> Either ProtocolParametersConversionError Alonzo.CostModels
toAlonzoCostModels m = do
  f <- mapM conv $ toList m
  Right $ Plutus.mkCostModels $ fromList f
 where
  conv
    :: (AnyPlutusScriptVersion, CostModel)
    -> Either ProtocolParametersConversionError (Plutus.Language, Alonzo.CostModel)
  conv (anySVer, cModel) = do
    alonzoCostModel <- toAlonzoCostModel cModel (toAlonzoScriptLanguage anySVer)
    Right (toAlonzoScriptLanguage anySVer, alonzoCostModel)

fromAlonzoCostModels
  :: Plutus.CostModels
  -> Map AnyPlutusScriptVersion CostModel
fromAlonzoCostModels cModels =
  fromList
    . map (bimap fromAlonzoScriptLanguage fromAlonzoCostModel)
    $ toList
    $ Plutus.costModelsValid cModels

toAlonzoScriptLanguage :: AnyPlutusScriptVersion -> Plutus.Language
toAlonzoScriptLanguage (AnyPlutusScriptVersion PlutusScriptV1) = Plutus.PlutusV1
toAlonzoScriptLanguage (AnyPlutusScriptVersion PlutusScriptV2) = Plutus.PlutusV2
toAlonzoScriptLanguage (AnyPlutusScriptVersion PlutusScriptV3) = Plutus.PlutusV3
toAlonzoScriptLanguage (AnyPlutusScriptVersion PlutusScriptV4) = Plutus.PlutusV4

fromAlonzoScriptLanguage :: Plutus.Language -> AnyPlutusScriptVersion
fromAlonzoScriptLanguage Plutus.PlutusV1 = AnyPlutusScriptVersion PlutusScriptV1
fromAlonzoScriptLanguage Plutus.PlutusV2 = AnyPlutusScriptVersion PlutusScriptV2
fromAlonzoScriptLanguage Plutus.PlutusV3 = AnyPlutusScriptVersion PlutusScriptV3
fromAlonzoScriptLanguage Plutus.PlutusV4 = AnyPlutusScriptVersion PlutusScriptV4

toAlonzoCostModel
  :: CostModel -> Plutus.Language -> Either ProtocolParametersConversionError Alonzo.CostModel
toAlonzoCostModel (CostModel m) l = first (PpceInvalidCostModel (CostModel m)) $ Alonzo.mkCostModel l m

fromAlonzoCostModel :: Alonzo.CostModel -> CostModel
fromAlonzoCostModel m = CostModel $ Alonzo.getCostModelParams m

boundRationalEither
  :: Ledger.BoundedRational b
  => String
  -> Rational
  -> Either ProtocolParametersConversionError b
boundRationalEither name r = maybeToRight (PpceOutOfBounds name r) $ Ledger.boundRational r

data ProtocolParametersError
  = PParamsErrorMissingMinUTxoValue !AnyCardanoEra
  | PParamsErrorMissingAlonzoProtocolParameter
  deriving Show

instance Error ProtocolParametersError where
  prettyError = \case
    PParamsErrorMissingMinUTxoValue (AnyCardanoEra era) ->
      mconcat
        [ "The " <> pretty era <> " protocol parameters value is missing the following "
        , "field: MinUTxoValue. Did you intend to use a " <> pretty era <> " protocol "
        , "parameters value?"
        ]
    PParamsErrorMissingAlonzoProtocolParameter ->
      mconcat
        [ "The Alonzo era protocol parameters in use is missing one or more of the "
        , "following fields: UTxOCostPerWord, CostModels, Prices, MaxTxExUnits, "
        , "MaxBlockExUnits, MaxValueSize, CollateralPercent, MaxCollateralInputs. Did "
        , "you intend to use an Alonzo era protocol parameters value?"
        ]

data ProtocolParametersConversionError
  = PpceOutOfBounds !ProtocolParameterName !Rational
  | PpceVersionInvalid !ProtocolParameterVersion
  | PpceInvalidCostModel !CostModel !CostModelApplyError
  | PpceMissingParameter !ProtocolParameterName
  deriving (Eq, Show, Data)

type ProtocolParameterName = String

type ProtocolParameterVersion = Natural

instance Error ProtocolParametersConversionError where
  prettyError = \case
    PpceOutOfBounds name r ->
      "Value for '" <> pretty name <> "' is outside of bounds: " <> pretty (fromRational r :: Double)
    PpceVersionInvalid majorProtVer ->
      "Major protocol version is invalid: " <> pretty majorProtVer
    PpceInvalidCostModel cm err ->
      "Invalid cost model: " <> pretty @Text (display err) <> " Cost model: " <> pshow cm
    PpceMissingParameter name ->
      "Missing parameter: " <> pretty name
