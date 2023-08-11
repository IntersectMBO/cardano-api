{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Domain.CostModel
  ( CostModel(..)
  , CostModels(..)
  , ProtocolParametersConversionError(..)
  , toAlonzoCostModels
  , fromAlonzoCostModels
  , toAlonzoScriptLanguage
  , fromAlonzoScriptLanguage
  , toAlonzoCostModel
  , fromAlonzoCostModel
  ) where

import           Cardano.Api.Domain.Common
import           Cardano.Api.Error
import           Cardano.Api.Orphans ()
import           Cardano.Api.Script
import           Cardano.Api.SerialiseCBOR

import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo

import           Data.Aeson (FromJSON (..), ToJSON (..))
import           Data.Bifunctor (bimap, first)
import           Data.Data (Data)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Text.PrettyBy.Default (display)

-- | Script cost models
newtype CostModel = CostModel [Integer]
  deriving (Eq, Show, Data)
  deriving newtype (ToCBOR, FromCBOR)

newtype CostModels = CostModels { unCostModels :: Map AnyPlutusScriptVersion CostModel }
  deriving (Eq, Show)

instance FromJSON CostModels where
  parseJSON v = CostModels . fromAlonzoCostModels <$> parseJSON v

instance ToJSON CostModels where
  toJSON (CostModels costModels) =
    case toAlonzoCostModels costModels of
      Left err -> error $ displayError err
      Right ledgerCostModels -> toJSON ledgerCostModels

data ProtocolParametersConversionError
  = PpceOutOfBounds !ProtocolParameterName !Rational
  | PpceVersionInvalid !ProtocolParameterVersion
  | PpceInvalidCostModel !CostModel !Alonzo.CostModelApplyError
  | PpceMissingParameter !ProtocolParameterName
  deriving (Eq, Show, Data)

instance Error ProtocolParametersConversionError where
  displayError = \case
    PpceOutOfBounds name r -> "Value for '" <> name <> "' is outside of bounds: " <> show (fromRational r :: Double)
    PpceVersionInvalid majorProtVer -> "Major protocol version is invalid: " <> show majorProtVer
    PpceInvalidCostModel cm err -> "Invalid cost model: " <> display err <> " Cost model: " <> show cm
    PpceMissingParameter name -> "Missing parameter: " <> name

toAlonzoCostModels
  :: Map AnyPlutusScriptVersion CostModel
  -> Either ProtocolParametersConversionError Alonzo.CostModels
toAlonzoCostModels m = do
  f <- mapM conv $ Map.toList m
  Right (Alonzo.emptyCostModels { Alonzo.costModelsValid = Map.fromList f })
 where
  conv :: (AnyPlutusScriptVersion, CostModel) -> Either ProtocolParametersConversionError (Alonzo.Language, Alonzo.CostModel)
  conv (anySVer, cModel) = do
    alonzoCostModel <- toAlonzoCostModel cModel (toAlonzoScriptLanguage anySVer)
    Right (toAlonzoScriptLanguage anySVer, alonzoCostModel)

fromAlonzoCostModels
  :: Alonzo.CostModels
  -> Map AnyPlutusScriptVersion CostModel
fromAlonzoCostModels (Alonzo.CostModels m _ _) =
    Map.fromList
  . map (bimap fromAlonzoScriptLanguage fromAlonzoCostModel)
  $ Map.toList m

toAlonzoScriptLanguage :: AnyPlutusScriptVersion -> Alonzo.Language
toAlonzoScriptLanguage (AnyPlutusScriptVersion PlutusScriptV1) = Alonzo.PlutusV1
toAlonzoScriptLanguage (AnyPlutusScriptVersion PlutusScriptV2) = Alonzo.PlutusV2
toAlonzoScriptLanguage (AnyPlutusScriptVersion PlutusScriptV3) = Alonzo.PlutusV3

fromAlonzoScriptLanguage :: Alonzo.Language -> AnyPlutusScriptVersion
fromAlonzoScriptLanguage Alonzo.PlutusV1 = AnyPlutusScriptVersion PlutusScriptV1
fromAlonzoScriptLanguage Alonzo.PlutusV2 = AnyPlutusScriptVersion PlutusScriptV2
fromAlonzoScriptLanguage Alonzo.PlutusV3 = AnyPlutusScriptVersion PlutusScriptV3

toAlonzoCostModel :: CostModel -> Alonzo.Language -> Either ProtocolParametersConversionError Alonzo.CostModel
toAlonzoCostModel (CostModel m) l = first (PpceInvalidCostModel (CostModel m)) $ Alonzo.mkCostModel l m

fromAlonzoCostModel :: Alonzo.CostModel -> CostModel
fromAlonzoCostModel m = CostModel $ Alonzo.getCostModelParams m
