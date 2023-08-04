{-# LANGUAGE DataKinds #-}
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

module Cardano.Api.Governance.Actions.VotingProcedure
  ( GovernanceActionId(..)
  , makeGoveranceActionId
  , Voter(..)
  , Vote(..)
  , toVoterRole
  , toVote
  , toVotingCredential
  , eraDecodeVotingCredential
  , VotingCredential(..)
  , createVotingProcedure
  , VotingProcedure(..)
  ) where

import           Cardano.Api.Address
import           Cardano.Api.Eras
import           Cardano.Api.Feature.ConwayEraOnwards
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.Script
import           Cardano.Api.SerialiseCBOR (FromCBOR (fromCBOR), SerialiseAsCBOR (..),
                   ToCBOR (toCBOR))
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.TxIn

import qualified Cardano.Binary as CBOR
import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Binary.Plain as Plain
import qualified Cardano.Ledger.Conway.Governance as Ledger
import           Cardano.Ledger.Core (EraCrypto)
import qualified Cardano.Ledger.Core as Shelley
import qualified Cardano.Ledger.Credential as Ledger
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Keys (HasKeyRole (..), KeyRole (Voting))
import qualified Cardano.Ledger.TxIn as Ledger

import           Data.ByteString.Lazy (ByteString)
import           Data.Maybe.Strict

-- | A representation of whether the era supports transactions with votes.
--
-- The Conway and subsequent eras support governance actions.
--
data TxVotesSupportedInEra era where
     VotesSupportedInConwayEra  :: TxVotesSupportedInEra ConwayEra
{-# DEPRECATED TxVotesSupportedInEra "Use ConwayEraOnwards instead" #-}

deriving instance Show (TxVotesSupportedInEra era)
deriving instance Eq (TxVotesSupportedInEra era)

newtype GovernanceActionId ledgerera = GovernanceActionId
  { unGovernanceActionId :: Ledger.GovernanceActionId (EraCrypto ledgerera)
  }
  deriving (Show, Eq)

makeGoveranceActionId
  :: ConwayEraOnwards era
  -> TxIn
  -> GovernanceActionId (ShelleyLedgerEra era)
makeGoveranceActionId w txin =
  let Ledger.TxIn txid (Ledger.TxIx txix) = toShelleyTxIn txin
  in conwayEraOnwardsConstraints w
      $ GovernanceActionId
      $ Ledger.GovernanceActionId
          { Ledger.gaidTxId = txid
          , Ledger.gaidGovActionIx = Ledger.GovernanceActionIx txix
          }


-- TODO: Conway era -
-- These should be the different keys corresponding to the Constitutional Committee and DReps.
-- We can then derive the StakeCredentials from them.
data Voter era
  = VoterCommittee (VotingCredential era) -- ^ Constitutional committee
  | VoterDRep (VotingCredential era) -- ^ Delegated representative
  | VoterSpo (Hash StakePoolKey) -- ^ Stake pool operator
  deriving (Show, Eq)

data Vote
  = No
  | Yes
  | Abstain
  deriving (Show, Eq)

toVoterRole
  :: EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
  => ConwayEraOnwards era
  -> Voter era
  -> Ledger.Voter (Shelley.EraCrypto (ShelleyLedgerEra era))
toVoterRole _ = \case
  VoterCommittee (VotingCredential cred) ->
    Ledger.CommitteeVoter $ coerceKeyRole cred -- TODO: Conway era - Alexey realllllyyy doesn't like this. We need to fix it.
  VoterDRep (VotingCredential cred) ->
    Ledger.DRepVoter cred
  VoterSpo (StakePoolKeyHash kh) ->
    Ledger.StakePoolVoter kh

toVote :: Vote -> Ledger.Vote
toVote = \case
  No -> Ledger.VoteNo
  Yes -> Ledger.VoteYes
  Abstain -> Ledger.Abstain

toVotingCredential
  :: ConwayEraOnwards era
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
eraDecodeVotingCredential
  :: ConwayEraOnwards era
  -> ByteString
  -> Either Plain.DecoderError (VotingCredential era)
eraDecodeVotingCredential w bs =
  conwayEraOnwardsConstraints w $
    case Plain.decodeFull bs of
      Left e -> Left e
      Right x -> Right $ VotingCredential x

newtype VotingCredential era = VotingCredential
  { unVotingCredential :: Ledger.Credential 'Voting (EraCrypto (ShelleyLedgerEra era))
  }

deriving instance Show (VotingCredential crypto)
deriving instance Eq (VotingCredential crypto)

createVotingProcedure
  :: ConwayEraOnwards era
  -> Vote
  -> Voter era
  -> GovernanceActionId (ShelleyLedgerEra era)
  -> VotingProcedure era
createVotingProcedure w vChoice vt (GovernanceActionId govActId) =
  conwayEraOnwardsConstraints w
    $ VotingProcedure $ Ledger.VotingProcedure
      { Ledger.vProcGovActionId = govActId
      , Ledger.vProcVoter = toVoterRole w vt
      , Ledger.vProcVote = toVote vChoice
      , Ledger.vProcAnchor = SNothing -- TODO: Conway
      }

newtype VotingProcedure era = VotingProcedure
  { unVotingProcedure :: Ledger.VotingProcedure (ShelleyLedgerEra era)
  }
  deriving (Show, Eq)

instance IsShelleyBasedEra era => ToCBOR (VotingProcedure era) where
  toCBOR (VotingProcedure vp) = shelleyBasedEraConstraints sbe $ Shelley.toEraCBOR @(ShelleyLedgerEra era) vp
    where sbe = shelleyBasedEra @era

instance IsShelleyBasedEra era => FromCBOR (VotingProcedure era) where
  fromCBOR = shelleyBasedEraConstraints (shelleyBasedEra @era) $ VotingProcedure <$> Shelley.fromEraCBOR @(ShelleyLedgerEra era)

instance IsShelleyBasedEra era => SerialiseAsCBOR (VotingProcedure era) where
  serialiseToCBOR = shelleyBasedEraConstraints (shelleyBasedEra @era) CBOR.serialize'
  deserialiseFromCBOR _proxy = shelleyBasedEraConstraints (shelleyBasedEra @era) CBOR.decodeFull'

instance IsShelleyBasedEra era => HasTextEnvelope (VotingProcedure era) where
  textEnvelopeType _ = "Governance vote"

instance HasTypeProxy era => HasTypeProxy (VotingProcedure era) where
  data AsType (VotingProcedure era) = AsVote
  proxyToAsType _ = AsVote

