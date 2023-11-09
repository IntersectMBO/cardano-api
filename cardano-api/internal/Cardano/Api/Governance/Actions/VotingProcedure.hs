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
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Governance.Actions.VotingProcedure where

import           Cardano.Api.Address
import           Cardano.Api.Eon.ConwayEraOnwards
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Governance.Actions.ProposalProcedure
import           Cardano.Api.HasTypeProxy
import qualified Cardano.Api.ReexposeLedger as Ledger
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseTextEnvelope

import qualified Cardano.Binary as CBOR
import qualified Cardano.Ledger.Api as L
import           Cardano.Ledger.Core (EraCrypto)
import qualified Cardano.Ledger.Core as L

import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           GHC.Generics

newtype GovernanceActionId era = GovernanceActionId
  { unGovernanceActionId :: Ledger.GovActionId (EraCrypto (LedgerEra era))
  }
  deriving (Show, Eq, Ord)

instance IsShelleyBasedEra era => ToCBOR (GovernanceActionId era) where
  toCBOR = \case
    GovernanceActionId v ->
      shelleyBasedEraConstraints (shelleyBasedEra @era) $ Ledger.toEraCBOR @(LedgerEra era) v

instance IsShelleyBasedEra era => FromCBOR (GovernanceActionId era) where
  fromCBOR = do
    !v <- shelleyBasedEraConstraints (shelleyBasedEra @era) $ Ledger.fromEraCBOR @(LedgerEra era)
    return $ GovernanceActionId v

newtype Voter era = Voter (Ledger.Voter (L.EraCrypto (LedgerEra era)))
  deriving (Show, Eq, Ord)

instance IsShelleyBasedEra era => ToCBOR (Voter era) where
  toCBOR (Voter v) = shelleyBasedEraConstraints (shelleyBasedEra @era) $ Ledger.toEraCBOR @(LedgerEra era) v

instance IsShelleyBasedEra era => FromCBOR (Voter era) where
  fromCBOR = do
    !v <- shelleyBasedEraConstraints (shelleyBasedEra @era) $ Ledger.fromEraCBOR @(LedgerEra era)
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

createVotingProcedure :: ()
  => ConwayEraOnwards era
  -> Vote
  -> Maybe (Ledger.Url, Text) -- ^ Anchor
  -> VotingProcedure era
createVotingProcedure eon vChoice mProposalAnchor =
  let proposalAnchor = fmap Text.encodeUtf8 <$>  mProposalAnchor
  in conwayEraOnwardsConstraints eon
        $ VotingProcedure $ Ledger.VotingProcedure
          { Ledger.vProcVote = toVote vChoice
          , Ledger.vProcAnchor = Ledger.maybeToStrictMaybe $ uncurry createAnchor <$> proposalAnchor
          }

newtype VotingProcedure era = VotingProcedure
  { unVotingProcedure :: Ledger.VotingProcedure (LedgerEra era)
  } deriving (Show, Eq)

instance IsShelleyBasedEra era => ToCBOR (VotingProcedure era) where
  toCBOR (VotingProcedure vp) = shelleyBasedEraConstraints sbe $ L.toEraCBOR @(LedgerEra era) vp
    where sbe = shelleyBasedEra @era

instance IsShelleyBasedEra era => FromCBOR (VotingProcedure era) where
  fromCBOR = shelleyBasedEraConstraints (shelleyBasedEra @era) $ VotingProcedure <$> L.fromEraCBOR @(LedgerEra era)

instance IsShelleyBasedEra era => SerialiseAsCBOR (VotingProcedure era) where
  serialiseToCBOR = shelleyBasedEraConstraints (shelleyBasedEra @era) CBOR.serialize'
  deserialiseFromCBOR _proxy = shelleyBasedEraConstraints (shelleyBasedEra @era) CBOR.decodeFull'

instance IsShelleyBasedEra era => HasTextEnvelope (VotingProcedure era) where
  textEnvelopeType _ = "Governance vote"

instance HasTypeProxy era => HasTypeProxy (VotingProcedure era) where
  data AsType (VotingProcedure era) = AsVote
  proxyToAsType _ = AsVote

newtype VotingProcedures era = VotingProcedures
  { unVotingProcedures  :: L.VotingProcedures (LedgerEra era)
  }

deriving instance Eq (VotingProcedures era)
deriving instance Generic (VotingProcedures era)
deriving instance Show (VotingProcedures era)

instance IsShelleyBasedEra era => ToCBOR (VotingProcedures era) where
  toCBOR = \case
    VotingProcedures vp ->
      shelleyBasedEraConstraints (shelleyBasedEra @era)
        $ L.toEraCBOR @(LedgerEra era) vp

instance IsShelleyBasedEra era => FromCBOR (VotingProcedures era) where
  fromCBOR =
    shelleyBasedEraConstraints (shelleyBasedEra @era)
      $ VotingProcedures <$> L.fromEraCBOR @(LedgerEra era)

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

singletonVotingProcedures :: ()
  => ConwayEraOnwards era
  -> L.Voter (L.EraCrypto (LedgerEra era))
  -> L.GovActionId (L.EraCrypto (LedgerEra era))
  -> L.VotingProcedure (LedgerEra era)
  -> VotingProcedures era
singletonVotingProcedures _ voter govActionId votingProcedure =
  VotingProcedures
    $ L.VotingProcedures
    $ Map.singleton voter
    $ Map.singleton govActionId votingProcedure

-- | Right biased merge of Voting procedures.
-- TODO Conway we need an alternative version of this function that can report conflicts as it is
-- not safe to just throw away votes.
unsafeMergeVotingProcedures :: ()
  => VotingProcedures era
  -> VotingProcedures era
  -> VotingProcedures era
unsafeMergeVotingProcedures vpsa vpsb =
  VotingProcedures
    $ L.VotingProcedures
    $ Map.unionWith (Map.unionWith const)
        (L.unVotingProcedures (unVotingProcedures vpsa))
        (L.unVotingProcedures (unVotingProcedures vpsb))
