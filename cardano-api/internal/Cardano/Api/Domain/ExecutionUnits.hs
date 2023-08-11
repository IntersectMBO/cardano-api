{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Domain.ExecutionUnits
  ( ExecutionUnits (..)
  , toAlonzoExUnits
  , fromAlonzoExUnits
  ) where

import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseJSON

import qualified Cardano.Binary as CBOR
import qualified Cardano.Ledger.Alonzo.Scripts as Alonzo

import           Data.Aeson (object, (.:), (.=))
import qualified Data.Aeson as Aeson
import           Numeric.Natural (Natural)

-- | Script execution units.
--
-- The units for how long a script executes for and how much memory it uses.
-- This is used to declare the resources used by a particular use of a script.
--
-- This type is also used to describe the limits for the maximum overall
-- execution units per transaction or per block.
--
data ExecutionUnits = ExecutionUnits
  { -- | This corresponds roughly to the time to execute a script.
    executionSteps  :: Natural
  , -- | This corresponds roughly to the peak memory used during script
    -- execution.
    executionMemory :: Natural
  }
  deriving (Eq, Show)

instance ToCBOR ExecutionUnits where
  toCBOR ExecutionUnits {executionSteps, executionMemory} =
      CBOR.encodeListLen 2
   <> toCBOR executionSteps
   <> toCBOR executionMemory

instance FromCBOR ExecutionUnits where
  fromCBOR = do
    CBOR.enforceSize "ExecutionUnits" 2
    ExecutionUnits
      <$> fromCBOR
      <*> fromCBOR

instance ToJSON ExecutionUnits where
  toJSON ExecutionUnits{executionSteps, executionMemory} =
    object [ "steps"  .= executionSteps
           , "memory" .= executionMemory ]

instance FromJSON ExecutionUnits where
  parseJSON =
    Aeson.withObject "ExecutionUnits" $ \o ->
      ExecutionUnits
        <$> o .: "steps"
        <*> o .: "memory"

toAlonzoExUnits :: ExecutionUnits -> Alonzo.ExUnits
toAlonzoExUnits ExecutionUnits{executionSteps, executionMemory} =
  Alonzo.ExUnits
  { Alonzo.exUnitsSteps = executionSteps
  , Alonzo.exUnitsMem   = executionMemory
  }

fromAlonzoExUnits :: Alonzo.ExUnits -> ExecutionUnits
fromAlonzoExUnits Alonzo.ExUnits{Alonzo.exUnitsSteps, Alonzo.exUnitsMem} =
  ExecutionUnits
  { executionSteps  = exUnitsSteps
  , executionMemory = exUnitsMem
  }
