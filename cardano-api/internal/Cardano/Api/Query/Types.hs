{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Api.Query.Types
  ( DebugLedgerState(..)
  , toDebugLedgerStatePair
  ) where

import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eras.Constraints
import           Cardano.Api.Orphans ()

import           Cardano.Binary
import qualified Cardano.Ledger.Binary.Plain as Plain
import qualified Cardano.Ledger.Shelley.API as Shelley

import           Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Aeson as Aeson

newtype DebugLedgerState era = DebugLedgerState
  { unDebugLedgerState :: Shelley.NewEpochState (ShelleyLedgerEra era)
  }

instance IsShelleyBasedEra era => FromCBOR (DebugLedgerState era) where
  fromCBOR =
    shelleyBasedEraConstraints (shelleyBasedEra @era) $
      DebugLedgerState <$>
        (fromCBOR :: Plain.Decoder s (Shelley.NewEpochState (ShelleyLedgerEra era)))

instance IsShelleyBasedEra era => ToJSON (DebugLedgerState era) where
  toJSON =
    let sbe = shelleyBasedEra @era in
    shelleyBasedEraConstraints sbe $ object . toDebugLedgerStatePair sbe
  toEncoding =
    let sbe = shelleyBasedEra @era in
    shelleyBasedEraConstraints sbe $ Aeson.pairs . mconcat . toDebugLedgerStatePair sbe

toDebugLedgerStatePair :: ()
  => Aeson.KeyValue a
  => ShelleyBasedEra era
  -> DebugLedgerState era
  -> [a]
toDebugLedgerStatePair sbe (DebugLedgerState newEpochS) =
  shelleyBasedEraConstraints sbe $
    let !nesEL = Shelley.nesEL newEpochS
        !nesBprev = Shelley.nesBprev newEpochS
        !nesBcur = Shelley.nesBcur newEpochS
        !nesEs = Shelley.nesEs newEpochS
        !nesRu = Shelley.nesRu newEpochS
        !nesPd = Shelley.nesPd newEpochS
    in  [ "lastEpoch" .= nesEL
        , "blocksBefore" .= nesBprev
        , "blocksCurrent" .= nesBcur
        , "stateBefore" .= nesEs
        , "possibleRewardUpdate" .= nesRu
        , "stakeDistrib" .= nesPd
        ]
