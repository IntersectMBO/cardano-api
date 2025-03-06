{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Api.Internal.Experimental.Witness.TxScriptWitnessRequirements
  ( -- * All the parts that constitute a plutus script witness but also including simple scripts
    TxScriptWitnessRequirements (..)

    -- * Collecting plutus script witness related transaction requirements.
  , getTxScriptWitnessesRequirements
  , obtainMonoidConstraint

    -- * For testing only
  , extractExecutionUnits
  )
where

import Cardano.Api.Internal.Eon.AlonzoEraOnwards
import Cardano.Api.Internal.Eon.Convert (Convert (convert))
import Cardano.Api.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Internal.Experimental.Plutus.IndexedPlutusScriptWitness
import Cardano.Api.Internal.Experimental.Witness.AnyWitness
import Cardano.Api.Internal.Script (ExecutionUnits, fromAlonzoExUnits)
import Cardano.Api.Ledger qualified as L

import Cardano.Ledger.Alonzo.TxWits qualified as L
import Ouroboros.Consensus.Shelley.Eras qualified as Consensus

import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- | This type collects all the requirements for script witnesses in a transaction.
data TxScriptWitnessRequirements era
  = TxScriptWitnessRequirements
      (Set L.Language)
      [L.Script era]
      (L.TxDats era)
      (L.Redeemers era)

instance Semigroup (TxScriptWitnessRequirements Consensus.StandardAlonzo) where
  (<>) (TxScriptWitnessRequirements l1 s1 d1 r1) (TxScriptWitnessRequirements l2 s2 d2 r2) =
    TxScriptWitnessRequirements (l1 <> l2) (s1 <> s2) (d1 <> d2) (r1 <> r2)

instance Monoid (TxScriptWitnessRequirements Consensus.StandardAlonzo) where
  mempty = TxScriptWitnessRequirements mempty mempty mempty mempty

instance Semigroup (TxScriptWitnessRequirements Consensus.StandardBabbage) where
  (<>) (TxScriptWitnessRequirements l1 s1 d1 r1) (TxScriptWitnessRequirements l2 s2 d2 r2) =
    TxScriptWitnessRequirements (l1 <> l2) (s1 <> s2) (d1 <> d2) (r1 <> r2)

instance Monoid (TxScriptWitnessRequirements Consensus.StandardBabbage) where
  mempty = TxScriptWitnessRequirements mempty mempty mempty mempty

instance Semigroup (TxScriptWitnessRequirements Consensus.StandardConway) where
  (<>) (TxScriptWitnessRequirements l1 s1 d1 r1) (TxScriptWitnessRequirements l2 s2 d2 r2) =
    TxScriptWitnessRequirements (l1 <> l2) (s1 <> s2) (d1 <> d2) (r1 <> r2)

instance Monoid (TxScriptWitnessRequirements Consensus.StandardConway) where
  mempty = TxScriptWitnessRequirements mempty mempty mempty mempty

getTxScriptWitnessRequirements
  :: AlonzoEraOnwards era
  -> (Witnessable witnessable (ShelleyLedgerEra era), AnyWitness (ShelleyLedgerEra era))
  -> TxScriptWitnessRequirements (ShelleyLedgerEra era)
getTxScriptWitnessRequirements era (thing, anyWit) =
  TxScriptWitnessRequirements
    (maybe mempty Set.singleton $ getAnyWitnessPlutusLanguage anyWit)
    (maybe mempty return $ getAnyWitnessScript (convert era) anyWit)
    (getAnyWitnessScriptData era anyWit)
    (getAnyWitnessRedeemerPointerMap era (thing, anyWit))

getTxScriptWitnessesRequirements
  :: AlonzoEraOnwards era
  -> [(Witnessable witnessable (ShelleyLedgerEra era), AnyWitness (ShelleyLedgerEra era))]
  -> TxScriptWitnessRequirements (ShelleyLedgerEra era)
getTxScriptWitnessesRequirements eon wits =
  obtainMonoidConstraint eon $ mconcat $ map (getTxScriptWitnessRequirements eon) wits

obtainMonoidConstraint
  :: AlonzoEraOnwards era
  -> (Monoid (TxScriptWitnessRequirements (ShelleyLedgerEra era)) => a)
  -> a
obtainMonoidConstraint eon = case eon of
  AlonzoEraOnwardsAlonzo -> id
  AlonzoEraOnwardsBabbage -> id
  AlonzoEraOnwardsConway -> id

extractExecutionUnits :: TxScriptWitnessRequirements era -> [ExecutionUnits]
extractExecutionUnits (TxScriptWitnessRequirements _ _ _ redeemers) =
  let m = L.unRedeemers redeemers
   in [fromAlonzoExUnits exUnits | (_, exUnits) <- Map.elems m]
