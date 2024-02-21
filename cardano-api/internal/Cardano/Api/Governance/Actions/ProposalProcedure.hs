{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Governance.Actions.ProposalProcedure where

import           Cardano.Api.Address
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.ProtocolParameters
import qualified Cardano.Api.ReexposeLedger as Ledger
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.TxIn
import           Cardano.Api.Value

import qualified Cardano.Binary as CBOR
import qualified Cardano.Ledger.Address as L
import           Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.Conway as Conway
import qualified Cardano.Ledger.Conway.Governance as Gov
import qualified Cardano.Ledger.Conway.Governance as Ledger
import           Cardano.Ledger.Core (EraCrypto)
import qualified Cardano.Ledger.Core as Shelley
import qualified Cardano.Ledger.Credential as L
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Keys (KeyRole (ColdCommitteeRole))

import           Data.ByteString (ByteString)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import           Data.Word

data AnyGovernanceAction = forall era. AnyGovernanceAction (Gov.GovAction era)

-- TODO: Conway - Transitiion to Ledger.GovAction
data GovernanceAction era
  = MotionOfNoConfidence
      (StrictMaybe (Ledger.GovPurposeId Ledger.CommitteePurpose (ShelleyLedgerEra era)))
  | ProposeNewConstitution
      (StrictMaybe (Ledger.GovPurposeId Ledger.ConstitutionPurpose (ShelleyLedgerEra era)))
      (Ledger.Anchor StandardCrypto)
      (StrictMaybe (Shelley.ScriptHash StandardCrypto))
  | ProposeNewCommittee
      (StrictMaybe (Ledger.GovPurposeId Ledger.CommitteePurpose (ShelleyLedgerEra era)))
      [Hash CommitteeColdKey] -- ^ Old constitutional committee
      (Map (Hash CommitteeColdKey) EpochNo) -- ^ New committee members with epoch number when each of them expires
      Rational -- ^ Quorum of the committee that is necessary for a successful vote
  | InfoAct
  | TreasuryWithdrawal
      [(Network, StakeCredential, Lovelace)]
      !(StrictMaybe (Shelley.ScriptHash StandardCrypto)) -- ^ Governance policy
  | InitiateHardfork
      (StrictMaybe (Ledger.GovPurposeId Ledger.HardForkPurpose (ShelleyLedgerEra era)))
      ProtVer
  | UpdatePParams
      (StrictMaybe (Ledger.GovPurposeId Ledger.PParamUpdatePurpose (ShelleyLedgerEra era)))
      (Ledger.PParamsUpdate (ShelleyLedgerEra era))
      !(StrictMaybe (Shelley.ScriptHash StandardCrypto)) -- ^ Governance policy

toGovernanceAction :: ()
  => ShelleyBasedEra era
  -> GovernanceAction era
  -> Gov.GovAction (ShelleyLedgerEra era)
toGovernanceAction sbe =
  shelleyBasedEraConstraints sbe $ \case
    MotionOfNoConfidence prevGovId ->
      Gov.NoConfidence prevGovId
    ProposeNewConstitution prevGovAction anchor mConstitutionScriptHash ->
      Gov.NewConstitution prevGovAction Gov.Constitution
        { Gov.constitutionAnchor = anchor
        , Gov.constitutionScript = mConstitutionScriptHash
        }
    ProposeNewCommittee prevGovId oldCommitteeMembers newCommitteeMembers quor ->
      Gov.UpdateCommittee
        prevGovId -- previous governance action id
        (Set.fromList $ map toCommitteeMember oldCommitteeMembers) -- members to remove
        (Map.mapKeys toCommitteeMember newCommitteeMembers) -- members to add
        (fromMaybe (error $ mconcat ["toGovernanceAction: the given quorum "
                                  , show quor
                                  , " was outside of the unit interval!"
                                  ])
              $ boundRational @UnitInterval quor)
    InfoAct ->
      Gov.InfoAction
    TreasuryWithdrawal withdrawals govPol ->
      let m = Map.fromList [(L.RewardAcnt nw (toShelleyStakeCredential sc), toShelleyLovelace l) | (nw,sc,l) <- withdrawals]
      in Gov.TreasuryWithdrawals m govPol
    InitiateHardfork prevGovId pVer ->
      Gov.HardForkInitiation prevGovId pVer
    UpdatePParams preGovId ppup govPol ->
      Gov.ParameterChange preGovId ppup govPol

fromGovernanceAction
  :: EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
  => Gov.GovAction (ShelleyLedgerEra era)
  -> GovernanceAction era
fromGovernanceAction = \case
  Gov.NoConfidence prevGovId ->
    MotionOfNoConfidence prevGovId
  Gov.NewConstitution prevGovId constitution ->
    ProposeNewConstitution prevGovId
      (Gov.constitutionAnchor constitution)
      (Gov.constitutionScript constitution)
  Gov.ParameterChange prevGovId pparams govPolicy ->
    UpdatePParams prevGovId pparams govPolicy
  Gov.HardForkInitiation prevGovId pVer ->
    InitiateHardfork prevGovId pVer
  Gov.TreasuryWithdrawals withdrawlMap govPolicy ->
    let res = [ (L.getRwdNetwork rwdAcnt, fromShelleyStakeCredential (L.getRwdCred rwdAcnt), fromShelleyLovelace coin)
              | (rwdAcnt, coin) <- Map.toList withdrawlMap
              ]
    in TreasuryWithdrawal res govPolicy
  Gov.UpdateCommittee prevGovId oldCommitteeMembers newCommitteeMembers quor ->
    ProposeNewCommittee
      prevGovId
      (map fromCommitteeMember $ Set.toList oldCommitteeMembers)
      (Map.mapKeys fromCommitteeMember newCommitteeMembers)
      (unboundRational quor)
  Gov.InfoAction ->
    InfoAct

newtype Proposal era = Proposal { unProposal :: Gov.ProposalProcedure (ShelleyLedgerEra era) }

instance IsShelleyBasedEra era => Show (Proposal era) where
  show (Proposal pp) = do
    let ppStr = shelleyBasedEraConstraints (shelleyBasedEra @era) $ show pp
    "Proposal {unProposal = " <> ppStr <> "}"

instance IsShelleyBasedEra era => Eq (Proposal era) where
  (Proposal pp1) == (Proposal pp2) = shelleyBasedEraConstraints (shelleyBasedEra @era) $ pp1 == pp2

instance IsShelleyBasedEra era => ToCBOR (Proposal era) where
  toCBOR (Proposal vp) = shelleyBasedEraConstraints (shelleyBasedEra @era) $ Shelley.toEraCBOR @Conway.Conway vp

instance IsShelleyBasedEra era => FromCBOR (Proposal era) where
  fromCBOR = Proposal <$> shelleyBasedEraConstraints (shelleyBasedEra @era) (Shelley.fromEraCBOR @Conway.Conway)

instance IsShelleyBasedEra era => SerialiseAsCBOR (Proposal era) where
  serialiseToCBOR = shelleyBasedEraConstraints (shelleyBasedEra @era) CBOR.serialize'
  deserialiseFromCBOR _proxy = shelleyBasedEraConstraints (shelleyBasedEra @era) CBOR.decodeFull'

instance IsShelleyBasedEra era => HasTextEnvelope (Proposal era) where
  textEnvelopeType _ = "Governance proposal"

instance HasTypeProxy era => HasTypeProxy (Proposal era) where
    data AsType (Proposal era) = AsProposal
    proxyToAsType _ = AsProposal


createProposalProcedure
  :: ShelleyBasedEra era
  -> Network
  -> Lovelace -- ^ Deposit
  -> StakeCredential -- ^ Credential to return the deposit to.
  -> GovernanceAction era
  -> Ledger.Anchor StandardCrypto
  -> Proposal era
createProposalProcedure sbe nw dep cred govAct anchor =
  shelleyBasedEraConstraints sbe $
    Proposal Gov.ProposalProcedure
      { Gov.pProcDeposit = toShelleyLovelace dep
      , Gov.pProcReturnAddr = L.RewardAcnt nw $ toShelleyStakeCredential cred
      , Gov.pProcGovAction = toGovernanceAction sbe govAct
      , Gov.pProcAnchor = anchor
      }

fromProposalProcedure
  :: ShelleyBasedEra era
  -> Proposal era
  -> (Lovelace, Hash StakeKey, GovernanceAction era)
fromProposalProcedure sbe (Proposal pp) =
  shelleyBasedEraConstraints sbe
    ( fromShelleyLovelace $ Gov.pProcDeposit pp
    , case fromShelleyStakeCredential (L.getRwdCred (Gov.pProcReturnAddr pp)) of
          StakeCredentialByKey keyhash -> keyhash
          StakeCredentialByScript _scripthash ->
            error "fromProposalProcedure TODO: Conway era script reward addresses not yet supported"
    , fromGovernanceAction (Gov.pProcGovAction pp)
    )


createPreviousGovernanceActionId
  :: EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
  => TxId
  -> Word32 -- ^ Governance action transation index
  -> Ledger.GovPurposeId (r :: Ledger.GovActionPurpose) (ShelleyLedgerEra era)
createPreviousGovernanceActionId  txid index =
   Ledger.GovPurposeId $ createGovernanceActionId txid index


createGovernanceActionId :: TxId -> Word32 -> Gov.GovActionId StandardCrypto
createGovernanceActionId txid index =
   Ledger.GovActionId
     { Ledger.gaidTxId = toShelleyTxId txid
     , Ledger.gaidGovActionIx = Ledger.GovActionIx index
     }


createAnchor :: Url -> ByteString -> Anchor StandardCrypto
createAnchor url anchorData =
  Ledger.Anchor
    { anchorUrl = url
    , anchorDataHash = hashAnchorData $ Ledger.AnchorData anchorData
    }

-- ----------------------------------------------------------------------------
-- TODO conversions that likely need to live elsewhere and may even deserve
-- additional wrapper types

toCommitteeMember :: Hash CommitteeColdKey -> L.Credential ColdCommitteeRole StandardCrypto
toCommitteeMember (CommitteeColdKeyHash keyhash) = L.KeyHashObj keyhash

fromCommitteeMember :: L.Credential ColdCommitteeRole StandardCrypto -> Hash CommitteeColdKey
fromCommitteeMember = \case
  L.KeyHashObj keyhash -> CommitteeColdKeyHash keyhash
  L.ScriptHashObj _scripthash -> error "TODO script committee members not yet supported"


