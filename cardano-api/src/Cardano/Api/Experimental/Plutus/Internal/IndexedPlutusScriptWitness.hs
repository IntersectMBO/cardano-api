{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Api.Experimental.Plutus.Internal.IndexedPlutusScriptWitness
  ( -- * Constuct an indexed plutus script witness.
    AnyIndexedPlutusScriptWitness (..)
  , IndexedPlutusScriptWitness (..)

    -- * Witnessable things.
  , Witnessable (..)
  , WitnessableItem (..)

    -- * Create the index for a witnessable thing.
  , toPlutusScriptPurpose
  , toPlutusScriptPurposeIndex
  , createIndexedPlutusScriptWitnesses
  , getAnyWitnessRedeemerPointerMap
  , obtainAlonzoScriptPurposeConstraints

    -- * Exposed for testing
  , constructRedeeemerPointerMap
  )
where

import Cardano.Api.Address
import Cardano.Api.Era.Internal.Eon.AlonzoEraOnwards
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Experimental.AnyScriptWitness
import Cardano.Api.Experimental.Plutus.Internal.ScriptWitness
import Cardano.Api.Experimental.Tx.Internal.AnyWitness
import Cardano.Api.Ledger qualified as L
import Cardano.Api.Plutus.Internal.Script (toAlonzoExUnits)
import Cardano.Api.Plutus.Internal.ScriptData
import Cardano.Api.Tx.Internal.TxIn
import Cardano.Api.Value.Internal

import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Conway.Scripts qualified as L

import Data.Function
import Data.List qualified as List
import Data.Word
import GHC.Exts

-- | A Plutus script witness along the thing it is witnessing and the index of that thing.
-- E.g transaction input, certificate, withdrawal, minting policy, etc.
-- A Plutus script witness only makes sense in the context of what it is witnessing
-- and the index of the thing it is witnessing.
data IndexedPlutusScriptWitness witnessable (lang :: L.Language) (purpose :: PlutusScriptPurpose) era where
  IndexedPlutusScriptWitness
    :: L.AlonzoEraScript era
    => Witnessable witnessable era
    -> (L.PlutusPurpose L.AsIx era)
    -> AnyPlutusScriptWitness lang purpose era
    -> IndexedPlutusScriptWitness witnessable lang purpose era

deriving instance Show (IndexedPlutusScriptWitness witnessable lang purpose era)

data AnyIndexedPlutusScriptWitness era where
  AnyIndexedPlutusScriptWitness
    :: IndexedPlutusScriptWitness witnessable lang purpose era
    -> AnyIndexedPlutusScriptWitness era

deriving instance Show (AnyIndexedPlutusScriptWitness era)

-- | These are all of the "things" a plutus script can witness. We include the relevant
-- type class constraint to avoid boilerplate when creating the 'PlutusPurpose' in the 'toPlutusScriptPurpose'.
data Witnessable (thing :: WitnessableItem) era where
  WitTxIn
    :: L.AlonzoEraScript era
    => TxIn
    -> Witnessable TxInItem era
  WitTxCert
    :: (L.EraTxCert era, L.AlonzoEraScript era)
    => L.TxCert era
    -> StakeCredential
    -> Witnessable CertItem era
  WitMint
    :: L.AlonzoEraScript era
    => PolicyId
    -> PolicyAssets
    -> Witnessable MintItem era
  WitWithdrawal
    :: L.AlonzoEraScript era
    => StakeAddress
    -> L.Coin
    -> Witnessable WithdrawalItem era
  WitVote
    :: L.ConwayEraScript era
    => L.Voter
    -> Witnessable VoterItem era
  WitProposal
    :: (L.ConwayEraScript era, L.EraPParams era)
    => L.ProposalProcedure era
    -> Witnessable ProposalItem era

deriving instance Show (Witnessable thing era)

deriving instance Eq (Witnessable thing era)

-- | We have to enforce the same ordering of witnessable things that exists in the ledger.
-- If we don't our redeemer pointer map will be incorrect and the transaction will be invalid.
-- See section 4.1 Combining Scripts with Their Inputs of the Alonzo ledger specification.
compareWitnesses :: Witnessable thing era -> Witnessable thing era -> Ordering
compareWitnesses a b =
  case (a, b) of
    (WitTxIn txinA, WitTxIn txinB) -> compare txinA txinB
    (WitTxCert{}, WitTxCert{}) -> LT -- Certificates in the ledger are in an `OSet` therefore we preserve the order.
    (WitMint polIdA _, WitMint polIdB _) -> compare polIdA polIdB
    (WitWithdrawal stakeAddrA _, WitWithdrawal stakeAddrB _) -> compare stakeAddrA stakeAddrB
    (WitVote voterA, WitVote voterB) -> compare voterA voterB
    (WitProposal propA, WitProposal propB) -> compare propA propB

data WitnessableItem
  = TxInItem
  | CertItem
  | MintItem
  | WithdrawalItem
  | VoterItem
  | ProposalItem

-- | To reduce boilerplate, we reuse the `PlutusPurpose` type from `cardano-ledger`.
-- This type is utilized in constructing the redeemer pointers map, which
-- links the redeemer and execution units with the entity being witnessed.
-- The map is indexed by the redeemer pointer.
--
-- A natural question arises: How do Plutus scripts determine which
-- execution units and redeemer are paired with them? The ledger constructs a redeemer pointer
-- for every Plutus script, and this pointer corresponds to the one in the transaction's
-- redeemer pointers map. For more details, refer to `collectPlutusScriptsWithContext`
-- in `cardano-ledger`.
toPlutusScriptPurpose
  :: Word32
  -> Witnessable thing era
  -> L.PlutusPurpose L.AsIx era
toPlutusScriptPurpose index WitTxIn{} = L.mkSpendingPurpose (L.AsIx index)
toPlutusScriptPurpose index WitWithdrawal{} = L.mkRewardingPurpose (L.AsIx index)
toPlutusScriptPurpose index WitMint{} = L.mkMintingPurpose (L.AsIx index)
toPlutusScriptPurpose index WitTxCert{} = L.mkCertifyingPurpose (L.AsIx index)
toPlutusScriptPurpose index WitVote{} = L.mkVotingPurpose (L.AsIx index)
toPlutusScriptPurpose index WitProposal{} = L.mkProposingPurpose (L.AsIx index)

-- | Classify a ledger redeemer pointer ('L.PlutusPurpose' 'L.AsIx') into the
-- 'PlutusScriptPurpose' category it belongs to and the index within that
-- category. This is the read direction, the inverse of 'toPlutusScriptPurpose'.
toPlutusScriptPurposeIndex
  :: ShelleyBasedEra era
  -> L.PlutusPurpose L.AsIx (ShelleyLedgerEra era)
  -> (PlutusScriptPurpose, Word32)
toPlutusScriptPurposeIndex = \case
  -- eras before Alonzo have no plutus purposes at all - 'L.PlutusPurpose' is
  -- a stuck type family there, so no argument can ever be supplied to these
  -- arms; GHC cannot prove a stuck family uninhabited (an empty case trips
  -- '-Wincomplete-patterns'), hence the error stubs
  ShelleyBasedEraShelley -> \case
    _ -> error "toPlutusScriptPurposeIndex: impossible"
  ShelleyBasedEraAllegra -> \case
    _ -> error "toPlutusScriptPurposeIndex: impossible"
  ShelleyBasedEraMary -> \case
    _ -> error "toPlutusScriptPurposeIndex: impossible"
  ShelleyBasedEraAlonzo -> \case
    L.AlonzoSpending (L.AsIx i) -> (SpendingScript, i)
    L.AlonzoMinting (L.AsIx i) -> (MintingScript, i)
    L.AlonzoCertifying (L.AsIx i) -> (CertifyingScript, i)
    L.AlonzoRewarding (L.AsIx i) -> (WithdrawingScript, i)
  ShelleyBasedEraBabbage -> \case
    L.AlonzoSpending (L.AsIx i) -> (SpendingScript, i)
    L.AlonzoMinting (L.AsIx i) -> (MintingScript, i)
    L.AlonzoCertifying (L.AsIx i) -> (CertifyingScript, i)
    L.AlonzoRewarding (L.AsIx i) -> (WithdrawingScript, i)
  ShelleyBasedEraConway -> \case
    L.ConwaySpending (L.AsIx i) -> (SpendingScript, i)
    L.ConwayMinting (L.AsIx i) -> (MintingScript, i)
    L.ConwayCertifying (L.AsIx i) -> (CertifyingScript, i)
    L.ConwayRewarding (L.AsIx i) -> (WithdrawingScript, i)
    L.ConwayVoting (L.AsIx i) -> (VotingScript, i)
    L.ConwayProposing (L.AsIx i) -> (ProposingScript, i)
  ShelleyBasedEraDijkstra -> \case
    L.DijkstraSpending (L.AsIx i) -> (SpendingScript, i)
    L.DijkstraMinting (L.AsIx i) -> (MintingScript, i)
    L.DijkstraCertifying (L.AsIx i) -> (CertifyingScript, i)
    L.DijkstraRewarding (L.AsIx i) -> (WithdrawingScript, i)
    L.DijkstraVoting (L.AsIx i) -> (VotingScript, i)
    L.DijkstraProposing (L.AsIx i) -> (ProposingScript, i)
    L.DijkstraGuarding (L.AsIx i) -> (GuardingScript, i)

createIndexedPlutusScriptWitness
  :: L.AlonzoEraScript era
  => Word32
  -> Witnessable witnessable era
  -> AnyPlutusScriptWitness lang purpose era
  -> IndexedPlutusScriptWitness witnessable lang purpose era
createIndexedPlutusScriptWitness index witnessable =
  IndexedPlutusScriptWitness witnessable (toPlutusScriptPurpose index witnessable)

-- | Create a list of indexed plutus script witnesses from anything witnessable that has been
-- witnesseed by a plutus script.
createIndexedPlutusScriptWitnesses
  :: L.AlonzoEraScript era
  => [(Witnessable witnessable era, AnyWitness era)]
  -> [AnyIndexedPlutusScriptWitness era]
createIndexedPlutusScriptWitnesses witnessableThings =
  [ AnyIndexedPlutusScriptWitness $ createIndexedPlutusScriptWitness index thing sWit
  | (index, (thing, AnyPlutusScriptWitness sWit)) <- zip [0 ..] $ enforceOrdering witnessableThings
  ]
 where
  enforceOrdering = List.sortBy (compareWitnesses `on` fst)

-- | The transaction's redeemer pointer map allows the ledger to connect a redeemer and execution unit pairing to the relevant
-- script. The ledger basically reconstructs the indicies (redeemer pointers) of this map can then look up the relevant
-- execution units/redeemer pairing. NB: the redeemer pointer has been renamed to 'PlutusPurpose AsIndex' in the ledger.
getAnyWitnessRedeemerPointerMap
  :: L.AlonzoEraScript era
  => [(Witnessable witnessable era, AnyWitness era)]
  -> L.Redeemers era
getAnyWitnessRedeemerPointerMap anyWit =
  constructRedeeemerPointerMap $
    createIndexedPlutusScriptWitnesses anyWit

-- | An 'IndexedPlutusScriptWitness' contains everything we need to construct a single
-- entry in the redeemer pointer map.
constructRedeemerPointer
  :: L.Era era
  => AnyIndexedPlutusScriptWitness era
  -> L.Redeemers era
constructRedeemerPointer (AnyIndexedPlutusScriptWitness (IndexedPlutusScriptWitness _ purpose scriptWit)) =
  let redeemer = getAnyPlutusScriptWitnessRedeemer scriptWit
      execUnits = getAnyPlutusScriptWitnessExecutionUnits scriptWit
   in L.Redeemers $
        fromList [(purpose, (toAlonzoData redeemer, toAlonzoExUnits execUnits))]

constructRedeeemerPointerMap
  :: L.AlonzoEraScript era
  => [AnyIndexedPlutusScriptWitness era]
  -> L.Redeemers era
constructRedeeemerPointerMap scriptWits =
  let redeemerPointers = map constructRedeemerPointer scriptWits
   in mconcat redeemerPointers

obtainAlonzoScriptPurposeConstraints
  :: AlonzoEraOnwards era
  -> (L.AlonzoEraScript (ShelleyLedgerEra era) => a)
  -> a
obtainAlonzoScriptPurposeConstraints v =
  case v of
    AlonzoEraOnwardsAlonzo -> id
    AlonzoEraOnwardsBabbage -> id
    AlonzoEraOnwardsConway -> id
    AlonzoEraOnwardsDijkstra -> id
