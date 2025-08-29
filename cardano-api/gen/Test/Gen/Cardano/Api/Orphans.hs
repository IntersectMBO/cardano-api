{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Gen.Cardano.Api.Orphans(obtainArb) where

import Cardano.Api hiding (txIns)
-- import Test.Cardano.Ledger.Conway.Arbitrary ()
-- import Test.Cardano.Ledger.Core.Arbitrary ()
import Cardano.Api.Genesis (unsafeBoundedRational)
import Cardano.Ledger.Api qualified as L

import Cardano.Ledger.Alonzo qualified as Ledger
import Cardano.Ledger.BaseTypes (promoteRatio)
import Cardano.Ledger.BaseTypes qualified as Ledger
import Cardano.Ledger.Conway qualified as Ledger
import Cardano.Ledger.Core qualified as Ledger
import Cardano.Ledger.Shelley qualified as Ledger
import Cardano.Ledger.Shelley.PParams (ShelleyPParams)
import Cardano.Ledger.Plutus.Language qualified as L
import Cardano.Api.Ledger qualified as L

import Data.Functor.Identity qualified as Ledger
import Data.Ratio
import Data.Word
import Cardano.Ledger.Keys qualified as Ledger

import GHC.Num
import GHC.Stack (HasCallStack)
import Test.QuickCheck
  ( Arbitrary (..)
  , Gen
  , choose
  , chooseInt
  , genericShrink
  , getNonNegative
  , oneof
  )
import Cardano.Ledger.Alonzo.Scripts qualified as Alonzo

import Generic.Random (genericArbitraryU)

instance Arbitrary (L.VotingProcedures (L.ShelleyEra)) where 
instance Arbitrary (L.VotingProcedures (L.MaryEra)) where 
instance Arbitrary (L.VotingProcedures (L.AllegraEra)) where 
instance Arbitrary (L.VotingProcedures (L.AlonzoEra)) where 
instance Arbitrary (L.VotingProcedures (L.BabbageEra)) where 
instance Arbitrary (L.VotingProcedures (L.ConwayEra)) where 

instance Arbitrary (Ledger.PParams Ledger.ShelleyEra) 
instance Arbitrary (Ledger.PParams L.MaryEra) 
instance Arbitrary (Ledger.PParams L.AllegraEra) 
instance Arbitrary (Ledger.PParams L.AlonzoEra) 
instance Arbitrary (Ledger.PParams L.BabbageEra) 
instance Arbitrary (Ledger.PParams L.ConwayEra) 


instance Arbitrary (L.ProposalProcedure L.ShelleyEra) 
instance Arbitrary (L.ProposalProcedure L.AllegraEra)
instance Arbitrary (L.ProposalProcedure L.MaryEra)
instance Arbitrary (L.ProposalProcedure L.AlonzoEra)
instance Arbitrary (L.ProposalProcedure L.BabbageEra)
instance Arbitrary (L.ProposalProcedure L.ConwayEra) 


instance Arbitrary (Alonzo.CostModel) where
instance Arbitrary L.MultiAsset where 
instance Arbitrary L.Delegatee
instance Arbitrary Ledger.Anchor
instance Arbitrary L.PoolParams
instance Arbitrary EpochNo
instance Arbitrary (L.Credential Ledger.ColdCommitteeRole)
instance Arbitrary (L.Credential Ledger.DRepRole)
instance Arbitrary (L.Credential Ledger.HotCommitteeRole)
instance Arbitrary MIRTarget
instance Arbitrary MIRPot
instance Arbitrary Alonzo.CostModels
instance Arbitrary L.Voter
instance Arbitrary L.Prices
instance Arbitrary L.ExUnits
instance Arbitrary L.CoinPerByte
instance Arbitrary L.PoolVotingThresholds
instance Arbitrary L.DRepVotingThresholds


obtainArb :: ShelleyBasedEra era 
          -> ((Arbitrary (Ledger.PParams (ShelleyLedgerEra era))
               , Arbitrary (L.VotingProcedures (ShelleyLedgerEra era))
               , Arbitrary (L.ProposalProcedure (ShelleyLedgerEra era))
               
             ) => a)
          -> a
obtainArb era f = case era of
    ShelleyBasedEraShelley -> f
    ShelleyBasedEraAllegra -> f
    ShelleyBasedEraMary    -> f
    ShelleyBasedEraAlonzo  -> f
    ShelleyBasedEraBabbage -> f
    ShelleyBasedEraConway  -> f