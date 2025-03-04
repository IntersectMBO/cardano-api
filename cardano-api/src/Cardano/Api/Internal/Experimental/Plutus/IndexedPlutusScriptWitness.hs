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
  , TxIn
  , Mint
  , Withdrawal
  , Cert
  , Voter
  , Proposal
    
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
import Cardano.Api.Internal.Certificate
import Cardano.Api.Internal.Eon.AlonzoEraOnwards
import Cardano.Api.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Internal.Governance.Actions.ProposalProcedure qualified as Api
import Cardano.Api.Internal.Governance.Actions.VotingProcedure qualified as Api
import Cardano.Api.Internal.Experimental.Plutus.ScriptWitness
import Cardano.Api.Internal.Experimental.Witness.AnyWitness
import Cardano.Api.Internal.Script (toAlonzoExUnits)
import Cardano.Api.Internal.ScriptData
import Cardano.Api.Ledger qualified as L

import Cardano.Ledger.Alonzo.TxWits qualified as L

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
data Witnessable thing era where
  WitTxIn :: L.AlonzoEraScript era => TxIn -> Witnessable TxIn era
  WitTxCert :: L.AlonzoEraScript era => Cert -> Witnessable Cert era
  WitMint :: L.AlonzoEraScript era => Mint -> Witnessable Mint era
  WitWithdrawal
    :: L.AlonzoEraScript era => Withdrawal -> Witnessable Withdrawal era
  WitVote
    :: L.ConwayEraScript era
    => Voter -> Witnessable Voter era
  WitProposal :: L.ConwayEraScript era => Proposal -> Witnessable Proposal era

deriving instance Show (Witnessable thing era)

deriving instance Eq (Witnessable thing era)

type Mint = (PolicyId, AssetName, Quantity)

type Withdrawal = (StakeAddress, L.Coin)

type Cert = (AnyCertificate, StakeCredential)

type Voter = Api.AnyVoter

type Proposal = Api.AnyProposal



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

createIndexedPlutusScriptWitnesses
  :: [(Witnessable witnessable era, AnyWitness era)]
  -> [AnyIndexedPlutusScriptWitness era]
createIndexedPlutusScriptWitnesses witnessableThings =
  [ AnyIndexedPlutusScriptWitness $ createIndexedPlutusScriptWitness index thing sWit
  | (index, (thing, AnyPlutusScriptWitness sWit)) <- zip [0 ..] witnessableThings
  ]

-- | The transaction's redeemer pointer map allows the ledger to connect a redeemer and execution unit pairing to the relevant
-- script. The ledger basically reconstructs the indicies (redeemer pointers) of this map can then look up the relevant
-- execution units/redeemer pairing. NB the redeemer pointer has been renamed to 'PlutusPurpose AsIndex' in the ledger.
getAnyWitnessRedeemerPointerMap
  :: AlonzoEraOnwards era
  -> (Witnessable witnessable (ShelleyLedgerEra era), AnyWitness (ShelleyLedgerEra era))
  -> L.Redeemers (ShelleyLedgerEra era)
getAnyWitnessRedeemerPointerMap eon (_, AnyKeyWitness) = alonzoEraOnwardsConstraints eon mempty
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
  -> [AnyIndexedPlutusScriptWitness ((ShelleyLedgerEra era))]
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