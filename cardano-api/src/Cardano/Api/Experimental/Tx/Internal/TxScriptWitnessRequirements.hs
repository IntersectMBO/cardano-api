{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Api.Experimental.Tx.Internal.TxScriptWitnessRequirements
  ( -- * All the parts that constitute a plutus script witness but also including simple scripts
    TxScriptWitnessRequirements (..)

    -- * Collecting plutus script witness related transaction requirements.
  , getTxScriptWitnessesRequirements
  , obtainMonoidConstraint

    -- * For testing only
  , extractExecutionUnits
  , getTxScriptWitnessRequirements
  )
where

import Cardano.Api.Era.Internal.Eon.AlonzoEraOnwards
import Cardano.Api.Era.Internal.Eon.Convert (Convert (convert))
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Experimental.Plutus.Internal.IndexedPlutusScriptWitness
import Cardano.Api.Experimental.Tx.Internal.AnyWitness
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Plutus.Internal.Script (ExecutionUnits, fromAlonzoExUnits)

import Cardano.Ledger.Alonzo.TxWits qualified as L
import Cardano.Ledger.Api.Era as L

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

instance Semigroup (TxScriptWitnessRequirements L.AlonzoEra) where
  (<>) (TxScriptWitnessRequirements l1 s1 d1 r1) (TxScriptWitnessRequirements l2 s2 d2 r2) =
    TxScriptWitnessRequirements (l1 <> l2) (s1 <> s2) (d1 <> d2) (r1 <> r2)

instance Monoid (TxScriptWitnessRequirements L.AlonzoEra) where
  mempty = TxScriptWitnessRequirements mempty mempty mempty mempty

instance Semigroup (TxScriptWitnessRequirements L.BabbageEra) where
  (<>) (TxScriptWitnessRequirements l1 s1 d1 r1) (TxScriptWitnessRequirements l2 s2 d2 r2) =
    TxScriptWitnessRequirements (l1 <> l2) (s1 <> s2) (d1 <> d2) (r1 <> r2)

instance Monoid (TxScriptWitnessRequirements L.BabbageEra) where
  mempty = TxScriptWitnessRequirements mempty mempty mempty mempty

instance Semigroup (TxScriptWitnessRequirements L.ConwayEra) where
  (<>) (TxScriptWitnessRequirements l1 s1 d1 r1) (TxScriptWitnessRequirements l2 s2 d2 r2) =
    TxScriptWitnessRequirements (l1 <> l2) (s1 <> s2) (d1 <> d2) (r1 <> r2)

instance Monoid (TxScriptWitnessRequirements L.ConwayEra) where
  mempty = TxScriptWitnessRequirements mempty mempty mempty mempty

instance Semigroup (TxScriptWitnessRequirements L.DijkstraEra) where
  (<>) (TxScriptWitnessRequirements l1 s1 d1 r1) (TxScriptWitnessRequirements l2 s2 d2 r2) =
    TxScriptWitnessRequirements (l1 <> l2) (s1 <> s2) (d1 <> d2) (r1 <> r2)

instance Monoid (TxScriptWitnessRequirements L.DijkstraEra) where
  mempty = TxScriptWitnessRequirements mempty mempty mempty mempty

getTxScriptWitnessRequirements
  :: AlonzoEraOnwards era
  -> [(Witnessable witnessable (ShelleyLedgerEra era), AnyWitness (ShelleyLedgerEra era))]
  -> TxScriptWitnessRequirements (ShelleyLedgerEra era)
getTxScriptWitnessRequirements era wits =
  let TxScriptWitnessRequirements l s d _ =
        obtainMonoidConstraint era $
          mconcat
            [ TxScriptWitnessRequirements
                (maybe mempty Set.singleton $ getAnyWitnessPlutusLanguage anyWit)
                (maybe mempty return $ getAnyWitnessScript (convert era) anyWit)
                (getAnyWitnessScriptData era anyWit)
                (alonzoEraOnwardsConstraints era mempty)
            | (_, anyWit) <- wits
            ]
   in TxScriptWitnessRequirements l s d (getAnyWitnessRedeemerPointerMap era wits)

getTxScriptWitnessesRequirements
  :: AlonzoEraOnwards era
  -> [(Witnessable witnessable (ShelleyLedgerEra era), AnyWitness (ShelleyLedgerEra era))]
  -> TxScriptWitnessRequirements (ShelleyLedgerEra era)
getTxScriptWitnessesRequirements eon wits =
  obtainMonoidConstraint eon $ getTxScriptWitnessRequirements eon wits

obtainMonoidConstraint
  :: AlonzoEraOnwards era
  -> (Monoid (TxScriptWitnessRequirements (ShelleyLedgerEra era)) => a)
  -> a
obtainMonoidConstraint eon = case eon of
  AlonzoEraOnwardsAlonzo -> id
  AlonzoEraOnwardsBabbage -> id
  AlonzoEraOnwardsConway -> id
  AlonzoEraOnwardsDijkstra -> id

extractExecutionUnits :: TxScriptWitnessRequirements era -> [ExecutionUnits]
extractExecutionUnits (TxScriptWitnessRequirements _ _ _ redeemers) =
  let m = L.unRedeemers redeemers
   in [fromAlonzoExUnits exUnits | (_, exUnits) <- Map.elems m]
