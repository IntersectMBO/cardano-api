{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Query.Types
  ( DebugLedgerState(..)
  , toDebugLedgerStatePair
  ) where

import           Cardano.Api.Eras.Core

import           Cardano.Binary
import           Cardano.Ledger.Binary
import qualified Cardano.Ledger.Binary.Plain as Plain
import qualified Cardano.Ledger.Shelley.API as Shelley
import qualified Cardano.Ledger.Shelley.Core as Core
import qualified Cardano.Ledger.Shelley.LedgerState as Shelley
import qualified Ouroboros.Consensus.Cardano.Block as Consensus

import           Data.Aeson (ToJSON (..), object, (.=))
import qualified Data.Aeson as Aeson
import           Data.Typeable

data DebugLedgerState era where
  DebugLedgerState ::
    ( ShelleyLedgerEra era ~ ledgerera
    )
    => Shelley.NewEpochState ledgerera
    -> DebugLedgerState era

instance
    ( Typeable era
    , Core.EraTxOut (ShelleyLedgerEra era)
    , Core.EraGovernance (ShelleyLedgerEra era)
    , DecCBOR (Shelley.StashedAVVMAddresses (ShelleyLedgerEra era))
    ) => FromCBOR (DebugLedgerState era) where
  fromCBOR = DebugLedgerState <$>
    (fromCBOR :: Plain.Decoder s (Shelley.NewEpochState (ShelleyLedgerEra era)))

-- TODO: Shelley based era class!
instance ( IsShelleyBasedEra era
         , ShelleyLedgerEra era ~ ledgerera
         , Consensus.ShelleyBasedEra ledgerera
         ) => ToJSON (DebugLedgerState era) where
  toJSON = object . toDebugLedgerStatePair
  toEncoding = Aeson.pairs . mconcat . toDebugLedgerStatePair

toDebugLedgerStatePair ::
  ( ShelleyLedgerEra era ~ ledgerera
  , Consensus.ShelleyBasedEra ledgerera
  , Aeson.KeyValue a
  ) => DebugLedgerState era -> [a]
toDebugLedgerStatePair (DebugLedgerState newEpochS) =
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
