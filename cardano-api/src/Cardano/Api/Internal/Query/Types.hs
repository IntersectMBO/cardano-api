{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Cardano.Api.Internal.Query.Types
  ( DebugLedgerState (..)
  , toDebugLedgerStatePair
  )
where

import Cardano.Api.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Internal.Orphans ()

import Cardano.Binary
import Cardano.Ledger.Binary.Plain qualified as Plain
import Cardano.Ledger.Shelley.API qualified as Shelley

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Aeson qualified as Aeson

newtype DebugLedgerState era = DebugLedgerState
  { unDebugLedgerState :: Shelley.NewEpochState (ShelleyLedgerEra era)
  }

instance IsShelleyBasedEra era => FromCBOR (DebugLedgerState era) where
  fromCBOR =
    shelleyBasedEraConstraints (shelleyBasedEra @era) $
      DebugLedgerState
        <$> (fromCBOR :: Plain.Decoder s (Shelley.NewEpochState (ShelleyLedgerEra era)))

instance IsShelleyBasedEra era => ToJSON (DebugLedgerState era) where
  toJSON =
    let sbe = shelleyBasedEra @era
     in object . toDebugLedgerStatePair sbe
  toEncoding =
    let sbe = shelleyBasedEra @era
     in Aeson.pairs . mconcat . toDebugLedgerStatePair sbe

toDebugLedgerStatePair
  :: ()
  => Aeson.KeyValue e a
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
     in [ "lastEpoch" .= nesEL
        , "blocksBefore" .= nesBprev
        , "blocksCurrent" .= nesBcur
        , "stateBefore" .= nesEs
        , "possibleRewardUpdate" .= nesRu
        , "stakeDistrib" .= nesPd
        ]
