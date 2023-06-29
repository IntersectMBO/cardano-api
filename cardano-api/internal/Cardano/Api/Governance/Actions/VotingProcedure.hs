{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Governance.Actions.VotingProcedure where

import           Cardano.Api.Address
import           Cardano.Api.Eras
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Script
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.TxIn
import           Cardano.Api.Utils

import qualified Cardano.Ledger.BaseTypes as Ledger
import qualified Cardano.Ledger.Binary.Plain as Plain
import qualified Cardano.Ledger.Conway.Governance as Gov
import           Cardano.Ledger.Core (EraCrypto)
import qualified Cardano.Ledger.Credential as Ledger
import           Cardano.Ledger.Keys
import qualified Cardano.Ledger.TxIn as Ledger

import           Data.ByteString.Lazy (ByteString)
import           Data.Maybe.Strict


-- | A representation of whether the era supports tx voting on governance actions.
--
-- The Conway and subsequent eras support tx voting on governance actions.
--
data TxVotes era where
  TxVotesNone :: TxVotes era

  TxVotes
    :: TxVotesSupportedInEra era
    -> [(VoteChoice, VoterType, GovernanceActionIdentifier (ShelleyLedgerEra era), VotingCredential era)]
    -> TxVotes era

deriving instance Show (TxVotes era)
deriving instance Eq (TxVotes era)


-- | A representation of whether the era supports transactions with votes.
--
-- The Conway and subsequent eras support governance actions.
--
data TxVotesSupportedInEra era where

     VotesSupportedInConwayEra  :: TxVotesSupportedInEra ConwayEra

deriving instance Show (TxVotesSupportedInEra era)
deriving instance Eq (TxVotesSupportedInEra era)


votesSupportedInEra :: ShelleyBasedEra  era -> Maybe (TxVotesSupportedInEra era)
votesSupportedInEra ShelleyBasedEraShelley = Nothing
votesSupportedInEra ShelleyBasedEraAllegra = Nothing
votesSupportedInEra ShelleyBasedEraMary    = Nothing
votesSupportedInEra ShelleyBasedEraAlonzo  = Nothing
votesSupportedInEra ShelleyBasedEraBabbage = Nothing
votesSupportedInEra ShelleyBasedEraConway  = Just VotesSupportedInConwayEra


newtype GovernanceActionIdentifier ledgerera
  = GovernanceActionIdentifier (Gov.GovernanceActionId (EraCrypto ledgerera))
  deriving (Show, Eq)

makeGoveranceActionIdentifier
  :: ShelleyBasedEra era -> TxIn -> GovernanceActionIdentifier (ShelleyLedgerEra era)
makeGoveranceActionIdentifier sbe txin =
  let Ledger.TxIn txid (Ledger.TxIx txix) = toShelleyTxIn txin
  in obtainEraCryptoConstraints sbe
       $ GovernanceActionIdentifier $
           Gov.GovernanceActionId
             { Gov.gaidTxId = txid
             , Gov.gaidGovActionIx = Gov.GovernanceActionIx txix
             }

-- toVotingCredential :: _ -> Ledger.Credential 'Voting (EraCrypto ledgerera)
-- toVotingCredential = undefined

data VoterType
  = CC -- ^ Constitutional committee
  | DR -- ^ Delegated representative
  | SP -- ^ Stake pool operator
  deriving (Show, Eq)

data VoteChoice
  = No
  | Yes
  | Abst -- ^ Abstain
  deriving (Show, Eq)

toVoterRole :: VoterType -> Gov.VoterRole
toVoterRole CC = Gov.ConstitutionalCommittee
toVoterRole DR = Gov.DRep
toVoterRole SP = Gov.SPO

toVote :: VoteChoice -> Gov.Vote
toVote No = Gov.VoteNo
toVote Yes = Gov.VoteYes
toVote Abst = Gov.Abstain

toVotingCredential
  :: ShelleyBasedEra era
  -> StakeCredential
  -> Either Plain.DecoderError (VotingCredential era)
toVotingCredential sbe (StakeCredentialByKey (StakeKeyHash kh)) = do
    let cbor = Plain.serialize kh
    eraDecodeVotingCredential sbe cbor

toVotingCredential sbe (StakeCredentialByScript (ScriptHash sh)) = do
    let cbor = Plain.serialize sh
    eraDecodeVotingCredential sbe cbor

--test :: ShelleyBasedEra era -> StakeCredential -> VotingCredential ConwayEra
--test sbe (StakeCredentialByKey k)= undefined
--test sbe (StakeCredentialByScript shash) =
--  let shelleyScHash = toShelleyScriptHash shash
--      shelleyCredential = Ledger.ScriptHashObj shelleyScHash
--  in VotingCredential shelleyCredential


-- TODO: Conway era
-- This is a hack. data StakeCredential in cardano-api is not parameterized by era, it defaults to StandardCrypto.
-- However VotingProcedure is parameterized on era. We need to also parameterize StakeCredential on era.
eraDecodeVotingCredential
  :: ShelleyBasedEra era
  -> ByteString
  -> Either Plain.DecoderError (VotingCredential era)
eraDecodeVotingCredential sbe bs = obtainCryptoConstraints sbe $
  case Plain.decodeFull bs of
    Left e -> Left e
    Right x -> Right $ VotingCredential x


newtype VotingCredential era
  = VotingCredential (Ledger.Credential 'Voting (EraCrypto (ShelleyLedgerEra era)))

deriving instance Show (VotingCredential crypto)
deriving instance Eq (VotingCredential crypto)

createVotingProcedure
  :: ShelleyBasedEra era
  -> VoteChoice
  -> VoterType
  -> GovernanceActionIdentifier (ShelleyLedgerEra era)
  -> VotingCredential era -- ^ Governance witness credential (ledger checks that you are allowed to vote)
  -> Vote era
createVotingProcedure sbe vChoice vt (GovernanceActionIdentifier govActId) (VotingCredential govWitnessCredential) =
  obtainEraCryptoConstraints sbe
    $ Vote $ Gov.VotingProcedure
      { Gov.vProcGovActionId = govActId
      , Gov.vProcRole = toVoterRole vt
      , Gov.vProcRoleKeyHash = govWitnessCredential
      , Gov.vProcVote = toVote vChoice
      , Gov.vProcAnchor = SNothing -- TODO: Conway
      }

newtype Vote era = Vote { unVote :: Gov.VotingProcedure (ShelleyLedgerEra era) }
  deriving (Show, Eq)

instance IsShelleyBasedEra era => ToCBOR (Vote era) where
  toCBOR (Vote _vp) = undefined

instance IsShelleyBasedEra era => FromCBOR (Vote era) where
  fromCBOR = undefined
instance IsShelleyBasedEra era => SerialiseAsCBOR (Vote era) where

  serialiseToCBOR = undefined
  deserialiseFromCBOR = undefined


instance IsShelleyBasedEra era => HasTextEnvelope (Vote era) where
  textEnvelopeType _ = "Governance vote"

instance HasTypeProxy era => HasTypeProxy (Vote era) where
    data AsType (Vote era) = AsVote
    proxyToAsType _ = AsVote
