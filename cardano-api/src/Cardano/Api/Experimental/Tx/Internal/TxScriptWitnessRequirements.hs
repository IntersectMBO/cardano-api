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

import Cardano.Api.Experimental.Era qualified as Exp
import Cardano.Api.Experimental.Plutus.Internal.IndexedPlutusScriptWitness
import Cardano.Api.Experimental.Tx.Internal.AnyWitness
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Plutus.Internal.Script (ExecutionUnits, fromAlonzoExUnits)

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
  :: L.AlonzoEraScript era
  => Monoid (TxScriptWitnessRequirements era)
  => [(Witnessable witnessable era, AnyWitness era)]
  -> TxScriptWitnessRequirements era
getTxScriptWitnessRequirements wits =
  let TxScriptWitnessRequirements l s d _ =
        mconcat
          [ TxScriptWitnessRequirements
              (maybe mempty Set.singleton $ getAnyWitnessPlutusLanguage anyWit)
              (maybe mempty return $ getAnyWitnessScript anyWit)
              (getAnyWitnessScriptData anyWit)
              mempty
          | (_, anyWit) <- wits
          ]
   in TxScriptWitnessRequirements l s d (getAnyWitnessRedeemerPointerMap wits)

getTxScriptWitnessesRequirements
  :: L.AlonzoEraScript era
  => Monoid (TxScriptWitnessRequirements era)
  => [(Witnessable witnessable era, AnyWitness era)]
  -> TxScriptWitnessRequirements era
getTxScriptWitnessesRequirements wits =
  getTxScriptWitnessRequirements wits

obtainMonoidConstraint
  :: Exp.Era era
  -> (Monoid (TxScriptWitnessRequirements (Exp.LedgerEra era)) => a)
  -> a
obtainMonoidConstraint eon = case eon of
  Exp.ConwayEra -> id
  Exp.DijkstraEra -> id

extractExecutionUnits :: TxScriptWitnessRequirements era -> [ExecutionUnits]
extractExecutionUnits (TxScriptWitnessRequirements _ _ _ redeemers) =
  let m = L.unRedeemers redeemers
   in [fromAlonzoExUnits exUnits | (_, exUnits) <- Map.elems m]
