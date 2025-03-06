{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Cardano.Api.Internal.Experimental.Plutus.IndexedPlutusScriptWitness
  ( -- * Constuct an indexed plutus script witness.
    AnyIndexedPlutusScriptWitness (..)
  , IndexedPlutusScriptWitness (..)

    -- * Witnessable things.
  , Witnessable (..)
  , WitnessableItem (..)

    -- * Create the index for a witnessable thing.
  , GetPlutusScriptPurpose (..)

  , createIndexedPlutusScriptWitnesses
  , getAnyWitnessRedeemerPointerMap
  , obtainAlonzoScriptPurposeConstraints
  )
where

import Data.Word
import Cardano.Ledger.Conway.Scripts qualified as L
import Cardano.Api.Internal.TxIn
import Cardano.Api.Internal.Value
import Cardano.Api.Internal.Address
import Cardano.Api.Internal.Eon.AlonzoEraOnwards
import Cardano.Api.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Internal.Experimental.Plutus.ScriptWitness
import Cardano.Api.Internal.Experimental.Witness.AnyWitness
import Cardano.Api.Internal.Script (toAlonzoExUnits)
import Cardano.Api.Internal.ScriptData
import Cardano.Api.Ledger qualified as L

import Cardano.Ledger.Alonzo.TxWits qualified as L

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
    :: Witnessable witnessable era
    -> (L.PlutusPurpose L.AsIx era)
    -> (PlutusScriptWitness lang purpose era)
    -> IndexedPlutusScriptWitness witnessable lang purpose era

data AnyIndexedPlutusScriptWitness era where
  AnyIndexedPlutusScriptWitness
    :: GetPlutusScriptPurpose era
    => IndexedPlutusScriptWitness witnessable lang purpose era
    -> AnyIndexedPlutusScriptWitness era


-- | These are all of the "things" a plutus script can witness. We include the relevant
-- type class constraint to avoid boilerplate when creating the 'PlutusPurpose' in
-- the 'GetPlutusScriptPurpose' instances.
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
    => L.Voter (L.EraCrypto era)
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
class GetPlutusScriptPurpose era where
  toPlutusScriptPurpose
    :: Word32
    -> Witnessable thing era
    -> L.PlutusPurpose L.AsIx era

instance GetPlutusScriptPurpose era where
  toPlutusScriptPurpose index WitTxIn{} = L.mkSpendingPurpose (L.AsIx index)
  toPlutusScriptPurpose index WitWithdrawal{} = L.mkRewardingPurpose (L.AsIx index)
  toPlutusScriptPurpose index WitMint{} = L.mkMintingPurpose (L.AsIx index)
  toPlutusScriptPurpose index WitTxCert{} = L.mkCertifyingPurpose (L.AsIx index)
  toPlutusScriptPurpose index WitVote{} = L.mkVotingPurpose (L.AsIx index)
  toPlutusScriptPurpose index WitProposal{} = L.mkProposingPurpose (L.AsIx index)



createIndexedPlutusScriptWitness
  :: Word32
  -> Witnessable witnessable era
  -> PlutusScriptWitness lang purpose era
  -> IndexedPlutusScriptWitness witnessable lang purpose era
createIndexedPlutusScriptWitness index witnessable =
  IndexedPlutusScriptWitness witnessable (toPlutusScriptPurpose index witnessable)

-- | Create a list of indexed plutus script witnesses from anything witnessable that has been
-- witnesseed by a plutus script.
createIndexedPlutusScriptWitnesses
  :: [(Witnessable witnessable era, AnyWitness era)]
  -> [AnyIndexedPlutusScriptWitness era]
createIndexedPlutusScriptWitnesses witnessableThings =
  [ AnyIndexedPlutusScriptWitness $ createIndexedPlutusScriptWitness index thing sWit
  | (index, (thing, AnyPlutusScriptWitness sWit)) <- zip [0 ..] $ enforceOrdering witnessableThings
  ]
 where
  enforceOrdering = List.sortBy (compareWitnesses `on` fst)

-- | The transaction's redeemer pointer map allows the ledger to connect a redeemer and execution unit pairing to the relevant
-- script. The ledger basically reconstructs the indicies (redeemer pointers) of this map can then look up the relevant
-- execution units/redeemer pairing. NB the redeemer pointer has been renamed to 'PlutusPurpose AsIndex' in the ledger.
getAnyWitnessRedeemerPointerMap
  :: AlonzoEraOnwards era
  -> (Witnessable witnessable (ShelleyLedgerEra era), AnyWitness (ShelleyLedgerEra era))
  -> L.Redeemers (ShelleyLedgerEra era)
getAnyWitnessRedeemerPointerMap eon (_, AnyKeyWitnessPlaceholder) = alonzoEraOnwardsConstraints eon mempty
getAnyWitnessRedeemerPointerMap eon (_, AnySimpleScriptWitness{}) = alonzoEraOnwardsConstraints eon mempty
getAnyWitnessRedeemerPointerMap eon anyWit =
  constructRedeeemerPointerMap eon $
    createIndexedPlutusScriptWitnesses [anyWit]

-- | An 'IndexedPlutusScriptWitness' contains everything we need to construct a single
-- entry in the redeemer pointer map.
constructRedeemerPointer
  :: AlonzoEraOnwards era
  -> AnyIndexedPlutusScriptWitness (ShelleyLedgerEra era)
  -> L.Redeemers (ShelleyLedgerEra era)
constructRedeemerPointer eon (AnyIndexedPlutusScriptWitness (IndexedPlutusScriptWitness _ purpose scriptWit)) =
  let PlutusScriptWitness _ _ _ redeemer execUnits = scriptWit
   in alonzoEraOnwardsConstraints eon $
        L.Redeemers $
          fromList [(purpose, (toAlonzoData redeemer, toAlonzoExUnits execUnits))]

constructRedeeemerPointerMap
  :: AlonzoEraOnwards era
  -> [AnyIndexedPlutusScriptWitness (ShelleyLedgerEra era)]
  -> L.Redeemers (ShelleyLedgerEra era)
constructRedeeemerPointerMap eon scriptWits =
  let redeemerPointers = map (constructRedeemerPointer eon) scriptWits
   in alonzoEraOnwardsConstraints eon $ mconcat redeemerPointers

obtainAlonzoScriptPurposeConstraints
  :: AlonzoEraOnwards era
  -> ((GetPlutusScriptPurpose era, L.AlonzoEraScript (ShelleyLedgerEra era)) => a)
  -> a
obtainAlonzoScriptPurposeConstraints v =
  case v of
    AlonzoEraOnwardsAlonzo -> id
    AlonzoEraOnwardsBabbage -> id
    AlonzoEraOnwardsConway -> id