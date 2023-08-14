{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | Script execution unit prices and cost models
module Cardano.Api.Domain.LegacyExecutionUnitPrices
  ( LegacyExecutionUnitPrices(..)
  , toAlonzoPrices
  , fromAlonzoPrices
  ) where

import           Cardano.Api.Domain.CostModel
import           Cardano.Api.Json (toRationalJSON)
import           Cardano.Api.Orphans ()
import           Cardano.Api.SerialiseCBOR

import qualified Cardano.Binary as CBOR
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo
import qualified Cardano.Ledger.BaseTypes as Ledger

import           Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))


-- | The prices for 'ExecutionUnits' as a fraction of a 'Lovelace'.
--
-- These are used to determine the fee for the use of a script within a
-- transaction, based on the 'ExecutionUnits' needed by the use of the script.
--
-- This type is considered legacy because it does not directly wrap ledger
-- version of this type, which is 'Alonzo.Prices' and is not isomorphic to it.
--
-- Conversion from the ledger type can fail.
data LegacyExecutionUnitPrices = LegacyExecutionUnitPrices
  { priceExecutionSteps  :: Rational
  , priceExecutionMemory :: Rational
  }
  deriving (Eq, Show)

instance ToCBOR LegacyExecutionUnitPrices where
  toCBOR LegacyExecutionUnitPrices{priceExecutionSteps, priceExecutionMemory} =
      CBOR.encodeListLen 2
   <> toCBOR priceExecutionSteps
   <> toCBOR priceExecutionMemory

instance FromCBOR LegacyExecutionUnitPrices where
  fromCBOR = do
    CBOR.enforceSize "LegacyExecutionUnitPrices" 2
    LegacyExecutionUnitPrices
      <$> fromCBOR
      <*> fromCBOR

instance ToJSON LegacyExecutionUnitPrices where
  toJSON LegacyExecutionUnitPrices{priceExecutionSteps, priceExecutionMemory} =
    object
      [ "priceSteps"  .= toRationalJSON priceExecutionSteps
      , "priceMemory" .= toRationalJSON priceExecutionMemory
      ]

instance FromJSON LegacyExecutionUnitPrices where
  parseJSON =
    withObject "LegacyExecutionUnitPrices" $ \o ->
      LegacyExecutionUnitPrices
        <$> o .: "priceSteps"
        <*> o .: "priceMemory"

toAlonzoPrices :: LegacyExecutionUnitPrices -> Either ProtocolParametersConversionError Alonzo.Prices
toAlonzoPrices
    LegacyExecutionUnitPrices
    { priceExecutionSteps
    , priceExecutionMemory
    } = do
  prSteps <- boundRationalEither "Steps" priceExecutionSteps
  prMem   <- boundRationalEither "Mem" priceExecutionMemory

  return Alonzo.Prices
    { Alonzo.prSteps
    , Alonzo.prMem
    }

fromAlonzoPrices :: Alonzo.Prices -> LegacyExecutionUnitPrices
fromAlonzoPrices Alonzo.Prices{Alonzo.prSteps, Alonzo.prMem} =
  LegacyExecutionUnitPrices
  { priceExecutionSteps  = Ledger.unboundRational prSteps
  , priceExecutionMemory = Ledger.unboundRational prMem
  }
