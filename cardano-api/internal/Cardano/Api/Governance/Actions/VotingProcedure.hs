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
import           Cardano.Api.Eras.Constraints
import           Cardano.Api.Governance.Actions.ProposalProcedure
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Keys.Shelley
import qualified Cardano.Api.ReexposeLedger as Ledger
import           Cardano.Api.Script
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseTextEnvelope

import qualified Cardano.Binary as CBOR
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Binary.Plain as Plain
import           Cardano.Ledger.Core (EraCrypto)
import qualified Cardano.Ledger.Core as L
import           Cardano.Ledger.Keys (HasKeyRole (..), KeyRole (DRepRole))

import           Data.ByteString.Lazy (ByteString)
import qualified Data.Map as Map
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           GHC.Generics

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


-- TODO: Conway era -
-- These should be the different keys corresponding to the Constitutional Committee and DReps.
-- We can then derive the StakeCredentials from them.
data Voter era
  = VoterCommittee (VotingCredential era) -- ^ Constitutional committee
  | VoterDRep (VotingCredential era) -- ^ Delegated representative
  | VoterSpo (Hash StakePoolKey) -- ^ Stake pool operator
  deriving (Show, Eq, Ord)

instance IsShelleyBasedEra era => ToCBOR (Voter era) where
  toCBOR = \case
    VoterCommittee v ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord 0 <> toCBOR v
    VoterDRep v ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord 1 <> toCBOR v
    VoterSpo v ->
      CBOR.encodeListLen 2 <> CBOR.encodeWord 2 <> toCBOR v

instance IsShelleyBasedEra era => FromCBOR (Voter era) where
  fromCBOR = do
    CBOR.decodeListLenOf 2
    t <- CBOR.decodeWord
    case t of
      0 -> do
        !x <- fromCBOR
        return $ VoterCommittee x
      1 -> do
        !x <- fromCBOR
        return $ VoterDRep x
      2 -> do
        !x <- fromCBOR
        return $ VoterSpo x
      _ ->
        CBOR.cborError $ CBOR.DecoderErrorUnknownTag "Voter era" (fromIntegral t)

data Vote
  = No
  | Yes
  | Abstain
  deriving (Show, Eq)

toVoterRole :: ()
  => ConwayEraOnwards era
  -> Voter era
  -> Ledger.Voter (L.EraCrypto (ShelleyLedgerEra era))
toVoterRole eon =
  conwayEraOnwardsConstraints eon $ \case
    VoterCommittee (VotingCredential cred) ->
      Ledger.CommitteeVoter $ coerceKeyRole cred -- TODO: Conway era - Alexey realllllyyy doesn't like this. We need to fix it.
    VoterDRep (VotingCredential cred) ->
      Ledger.DRepVoter cred
    VoterSpo (StakePoolKeyHash kh) ->
      Ledger.StakePoolVoter kh

fromVoterRole :: ()
  => ConwayEraOnwards era
  -> Ledger.Voter (L.EraCrypto (ShelleyLedgerEra era))
  -> Voter era
fromVoterRole eon =
  conwayEraOnwardsConstraints eon $ \case
    Ledger.CommitteeVoter cred ->
      VoterCommittee (VotingCredential (coerceKeyRole cred))    -- TODO: Conway era - We shouldn't be using coerceKeyRole.
    Ledger.DRepVoter cred ->
      VoterDRep (VotingCredential cred)
    Ledger.StakePoolVoter kh ->
      VoterSpo (StakePoolKeyHash kh)

toVote :: Vote -> Ledger.Vote
toVote = \case
  No -> Ledger.VoteNo
  Yes -> Ledger.VoteYes
  Abstain -> Ledger.Abstain

toVotingCredential :: ()
  => ConwayEraOnwards era
  -> StakeCredential
  -> Either Plain.DecoderError (VotingCredential era)
toVotingCredential sbe (StakeCredentialByKey (StakeKeyHash kh)) = do
  let cbor = Plain.serialize $ Ledger.KeyHashObj kh
  eraDecodeVotingCredential sbe cbor

toVotingCredential _sbe (StakeCredentialByScript (ScriptHash _sh)) =
  error "toVotingCredential: script stake credentials not implemented yet"
  -- TODO: Conway era
  -- let cbor = Plain.serialize $ Ledger.ScriptHashObj sh
  -- eraDecodeVotingCredential sbe cbor

-- TODO: Conway era
-- This is a hack. data StakeCredential in cardano-api is not parameterized by era, it defaults to StandardCrypto.
-- However VotingProcedure is parameterized on era. We need to also parameterize StakeCredential on era.
eraDecodeVotingCredential :: ()
  => ConwayEraOnwards era
  -> ByteString
  -> Either Plain.DecoderError (VotingCredential era)
eraDecodeVotingCredential eon bs =
  conwayEraOnwardsConstraints eon $
    case Plain.decodeFull bs of
      Left e -> Left e
      Right x -> Right $ VotingCredential x

newtype VotingCredential era = VotingCredential
  { unVotingCredential :: Ledger.Credential 'DRepRole (EraCrypto (ShelleyLedgerEra era))
  }

deriving instance Show (VotingCredential crypto)
deriving instance Eq (VotingCredential crypto)
deriving instance Ord (VotingCredential crypto)

instance IsShelleyBasedEra era => ToCBOR (VotingCredential era) where
  toCBOR = \case
    VotingCredential v ->
      shelleyBasedEraConstraints (shelleyBasedEra @era) $ CBOR.toCBOR v

instance IsShelleyBasedEra era => FromCBOR (VotingCredential era) where
  fromCBOR = do
    v <- shelleyBasedEraConstraints (shelleyBasedEra @era) CBOR.fromCBOR
    return $ VotingCredential v

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
  { unVotingProcedure :: Ledger.VotingProcedure (ShelleyLedgerEra era)
  } deriving (Show, Eq)

instance IsShelleyBasedEra era => ToCBOR (VotingProcedure era) where
  toCBOR (VotingProcedure vp) = shelleyBasedEraConstraints sbe $ L.toEraCBOR @(ShelleyLedgerEra era) vp
    where sbe = shelleyBasedEra @era

instance IsShelleyBasedEra era => FromCBOR (VotingProcedure era) where
  fromCBOR = shelleyBasedEraConstraints (shelleyBasedEra @era) $ VotingProcedure <$> L.fromEraCBOR @(ShelleyLedgerEra era)

instance IsShelleyBasedEra era => SerialiseAsCBOR (VotingProcedure era) where
  serialiseToCBOR = shelleyBasedEraConstraints (shelleyBasedEra @era) CBOR.serialize'
  deserialiseFromCBOR _proxy = shelleyBasedEraConstraints (shelleyBasedEra @era) CBOR.decodeFull'

instance IsShelleyBasedEra era => HasTextEnvelope (VotingProcedure era) where
  textEnvelopeType _ = "Governance vote"

instance HasTypeProxy era => HasTypeProxy (VotingProcedure era) where
  data AsType (VotingProcedure era) = AsVote
  proxyToAsType _ = AsVote

newtype VotingProcedures era = VotingProcedures
  { unVotingProcedures  :: L.VotingProcedures (ShelleyLedgerEra era)
  }

deriving instance Eq (VotingProcedures era)
deriving instance Generic (VotingProcedures era)
deriving instance Show (VotingProcedures era)

instance IsShelleyBasedEra era => ToCBOR (VotingProcedures era) where
  toCBOR = \case
    VotingProcedures vp ->
      shelleyBasedEraConstraints (shelleyBasedEra @era)
        $ L.toEraCBOR @(ShelleyLedgerEra era) vp

instance IsShelleyBasedEra era => FromCBOR (VotingProcedures era) where
  fromCBOR =
    shelleyBasedEraConstraints (shelleyBasedEra @era)
      $ VotingProcedures <$> L.fromEraCBOR @(ShelleyLedgerEra era)

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
  -> L.Voter (L.EraCrypto (ShelleyLedgerEra era))
  -> L.GovActionId (L.EraCrypto (ShelleyLedgerEra era))
  -> L.VotingProcedure (ShelleyLedgerEra era)
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
