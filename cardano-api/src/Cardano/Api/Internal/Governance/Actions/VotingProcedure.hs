{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Internal.Governance.Actions.VotingProcedure where

import Cardano.Api.Internal.Address
import Cardano.Api.Internal.Eon.ConwayEraOnwards
import Cardano.Api.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Internal.Governance.Actions.ProposalProcedure
import Cardano.Api.Internal.HasTypeProxy
import Cardano.Api.Internal.ReexposeLedger qualified as Ledger
import Cardano.Api.Internal.SerialiseCBOR
import Cardano.Api.Internal.SerialiseTextEnvelope

import Cardano.Binary qualified as CBOR
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.Core (EraCrypto)
import Cardano.Ledger.Core qualified as L

import Control.Monad (foldM)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Data.Type.Equality (TestEquality (..))
import Data.Typeable
import GHC.Generics

newtype GovernanceActionId era = GovernanceActionId
  { unGovernanceActionId :: Ledger.GovActionId (EraCrypto (ShelleyLedgerEra era))
  }
  deriving (Show, Eq, Ord)

instance IsShelleyBasedEra era => ToCBOR (GovernanceActionId era) where
  toCBOR = \case
    GovernanceActionId v ->
      shelleyBasedEraConstraints (shelleyBasedEra @era) $ Ledger.toEraCBOR @(ShelleyLedgerEra era) v

instance IsShelleyBasedEra era => FromCBOR (GovernanceActionId era) where
  fromCBOR = do
    !v <- shelleyBasedEraConstraints (shelleyBasedEra @era) $ Ledger.fromEraCBOR @(ShelleyLedgerEra era)
    return $ GovernanceActionId v

data Voter era where
  Voter :: Typeable era => (Ledger.Voter (L.EraCrypto (ShelleyLedgerEra era))) -> Voter era

deriving instance Show (Voter era)

deriving instance Eq (Voter era)

deriving instance Ord (Voter era)

instance TestEquality Voter where
  testEquality (Voter v) (Voter v') =
    voterTypeEquality v v'

voterTypeEquality
  :: (Typeable eraA, Typeable eraB)
  => Ledger.Voter (L.EraCrypto (ShelleyLedgerEra eraA))
  -> Ledger.Voter (L.EraCrypto (ShelleyLedgerEra eraB))
  -> Maybe (eraA :~: eraB)
voterTypeEquality _ _ = eqT

instance IsShelleyBasedEra era => ToCBOR (Voter era) where
  toCBOR (Voter v) = shelleyBasedEraConstraints (shelleyBasedEra @era) $ Ledger.toEraCBOR @(ShelleyLedgerEra era) v

instance IsShelleyBasedEra era => FromCBOR (Voter era) where
  fromCBOR = do
    !v <- shelleyBasedEraConstraints (shelleyBasedEra @era) $ Ledger.fromEraCBOR @(ShelleyLedgerEra era)
    pure $ Voter v

data Vote
  = No
  | Yes
  | Abstain
  deriving (Show, Eq)

toVote :: Vote -> Ledger.Vote
toVote = \case
  No -> Ledger.VoteNo
  Yes -> Ledger.VoteYes
  Abstain -> Ledger.Abstain

createVotingProcedure
  :: ()
  => ConwayEraOnwards era
  -> Vote
  -> Maybe (Ledger.Url, Text)
  -- ^ Anchor
  -> VotingProcedure era
createVotingProcedure eon vChoice mProposalAnchor =
  let proposalAnchor = fmap Text.encodeUtf8 <$> mProposalAnchor
   in conwayEraOnwardsConstraints eon $
        VotingProcedure $
          Ledger.VotingProcedure
            { Ledger.vProcVote = toVote vChoice
            , Ledger.vProcAnchor = Ledger.maybeToStrictMaybe $ uncurry createAnchor <$> proposalAnchor
            }

newtype VotingProcedure era = VotingProcedure
  { unVotingProcedure :: Ledger.VotingProcedure (ShelleyLedgerEra era)
  }
  deriving (Show, Eq)

instance IsShelleyBasedEra era => ToCBOR (VotingProcedure era) where
  toCBOR (VotingProcedure vp) = shelleyBasedEraConstraints sbe $ L.toEraCBOR @(ShelleyLedgerEra era) vp
   where
    sbe = shelleyBasedEra @era

instance IsShelleyBasedEra era => FromCBOR (VotingProcedure era) where
  fromCBOR =
    shelleyBasedEraConstraints (shelleyBasedEra @era) $
      VotingProcedure <$> L.fromEraCBOR @(ShelleyLedgerEra era)

instance IsShelleyBasedEra era => SerialiseAsCBOR (VotingProcedure era) where
  serialiseToCBOR = shelleyBasedEraConstraints (shelleyBasedEra @era) CBOR.serialize'
  deserialiseFromCBOR _proxy = shelleyBasedEraConstraints (shelleyBasedEra @era) CBOR.decodeFull'

instance IsShelleyBasedEra era => HasTextEnvelope (VotingProcedure era) where
  textEnvelopeType _ = "Governance vote"

instance HasTypeProxy era => HasTypeProxy (VotingProcedure era) where
  data AsType (VotingProcedure era) = AsVote
  proxyToAsType _ = AsVote

newtype VotingProcedures era = VotingProcedures
  { unVotingProcedures :: L.VotingProcedures (ShelleyLedgerEra era)
  }

deriving instance Eq (VotingProcedures era)

deriving instance Generic (VotingProcedures era)

deriving instance Show (VotingProcedures era)

instance IsShelleyBasedEra era => ToCBOR (VotingProcedures era) where
  toCBOR = \case
    VotingProcedures vp ->
      shelleyBasedEraConstraints (shelleyBasedEra @era) $
        L.toEraCBOR @(ShelleyLedgerEra era) vp

instance IsShelleyBasedEra era => FromCBOR (VotingProcedures era) where
  fromCBOR =
    shelleyBasedEraConstraints (shelleyBasedEra @era) $
      VotingProcedures <$> L.fromEraCBOR @(ShelleyLedgerEra era)

instance IsShelleyBasedEra era => SerialiseAsCBOR (VotingProcedures era) where
  serialiseToCBOR = shelleyBasedEraConstraints (shelleyBasedEra @era) CBOR.serialize'
  deserialiseFromCBOR _proxy = shelleyBasedEraConstraints (shelleyBasedEra @era) CBOR.decodeFull'

instance IsShelleyBasedEra era => HasTextEnvelope (VotingProcedures era) where
  textEnvelopeType _ = "Governance voting procedures"

instance HasTypeProxy era => HasTypeProxy (VotingProcedures era) where
  data AsType (VotingProcedures era) = AsVotingProcedures
  proxyToAsType _ = AsVotingProcedures

emptyVotingProcedures :: VotingProcedures era
emptyVotingProcedures = VotingProcedures $ L.VotingProcedures Map.empty

singletonVotingProcedures
  :: ()
  => ConwayEraOnwards era
  -> L.Voter (L.EraCrypto (ShelleyLedgerEra era))
  -> L.GovActionId (L.EraCrypto (ShelleyLedgerEra era))
  -> L.VotingProcedure (ShelleyLedgerEra era)
  -> VotingProcedures era
singletonVotingProcedures _ voter govActionId votingProcedure =
  VotingProcedures $
    L.VotingProcedures $
      Map.singleton voter $
        Map.singleton govActionId votingProcedure

-- | A voter, and the conflicting votes of this voter (i.e. votes with the same governance action identifier)
newtype VotesMergingConflict era
  = VotesMergingConflict
      ( L.Voter (L.EraCrypto (ShelleyLedgerEra era))
      , [L.GovActionId (L.EraCrypto (ShelleyLedgerEra era))]
      )
  deriving Show

-- | @mergeVotingProcedures vote1 vote2@ merges @vote1@ and @vote2@ into a single vote,
-- or fails if the votes are incompatible.
mergeVotingProcedures
  :: ()
  => VotingProcedures era
  -- ^ Votes to merge
  -> VotingProcedures era
  -- ^ Votes to merge
  -> Either (VotesMergingConflict era) (VotingProcedures era)
  -- ^ Either the conflict found, or the merged votes
mergeVotingProcedures vpsa vpsb =
  VotingProcedures . L.VotingProcedures <$> foldM mergeVotesOfOneVoter Map.empty allVoters
 where
  mapa = L.unVotingProcedures (unVotingProcedures vpsa)
  mapb = L.unVotingProcedures (unVotingProcedures vpsb)
  allVoters = Set.union (Map.keysSet mapa) (Map.keysSet mapb)
  mergeVotesOfOneVoter acc voter =
    Map.union acc <$> case (Map.lookup voter mapa, Map.lookup voter mapb) of
      (Just v, Nothing) -> Right $ Map.singleton voter v -- Take only available value
      (Nothing, Just v) -> Right $ Map.singleton voter v -- Take only available value
      (Nothing, Nothing) -> Right Map.empty -- No value
      (Just va, Just vb) ->
        -- Here's the case where we're unioning different votes for the same voter
        if null intersection -- No conflict: sets of keys from left and right is disjoint
          then Right $ Map.singleton voter (Map.union va vb)
          else Left $ VotesMergingConflict (voter, intersection) -- Ooops, a conflict! Let's report it!
       where
        intersection = Map.keys $ Map.intersection va vb
