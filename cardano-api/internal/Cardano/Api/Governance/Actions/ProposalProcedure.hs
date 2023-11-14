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

import qualified Cardano.Binary as CBOR
import qualified Cardano.Ledger.Address as L
import           Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.Coin as L
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
      (StrictMaybe (Ledger.PrevGovActionId Ledger.CommitteePurpose StandardCrypto))
  | ProposeNewConstitution
      (StrictMaybe (Ledger.PrevGovActionId Ledger.ConstitutionPurpose StandardCrypto))
      (Ledger.Anchor StandardCrypto)
  | ProposeNewCommittee
      (StrictMaybe (Ledger.PrevGovActionId Ledger.CommitteePurpose StandardCrypto))
      [Hash CommitteeColdKey] -- ^ Old constitutional committee
      (Map (Hash CommitteeColdKey) EpochNo) -- ^ New committee members with epoch number when each of them expires
      Rational -- ^ Quorum of the committee that is necessary for a successful vote
  | InfoAct
  | TreasuryWithdrawal [(Network, StakeCredential, L.Coin)]
  | InitiateHardfork
      (StrictMaybe (Ledger.PrevGovActionId Ledger.HardForkPurpose StandardCrypto))
      ProtVer
  | UpdatePParams
      (StrictMaybe (Ledger.PrevGovActionId Ledger.PParamUpdatePurpose StandardCrypto))
      (Ledger.PParamsUpdate (ShelleyLedgerEra era))

toGovernanceAction :: ()
  => ShelleyBasedEra era
  -> GovernanceAction era
  -> Gov.GovAction (ShelleyLedgerEra era)
toGovernanceAction sbe =
  shelleyBasedEraConstraints sbe $ \case
    MotionOfNoConfidence prevGovId ->
      Gov.NoConfidence prevGovId
    ProposeNewConstitution prevGovAction anchor ->
      Gov.NewConstitution prevGovAction Gov.Constitution
        { Gov.constitutionAnchor = anchor
        , Gov.constitutionScript = SNothing   -- TODO: Conway era
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
    TreasuryWithdrawal withdrawals ->
      let m = Map.fromList [(L.mkRwdAcnt nw (toShelleyStakeCredential sc), l) | (nw,sc,l) <- withdrawals]
      in Gov.TreasuryWithdrawals m
    InitiateHardfork prevGovId pVer ->
      Gov.HardForkInitiation prevGovId pVer
    UpdatePParams preGovId ppup ->
      Gov.ParameterChange preGovId ppup

fromGovernanceAction
  :: EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
  => Gov.GovAction (ShelleyLedgerEra era)
  -> GovernanceAction era
fromGovernanceAction = \case
  Gov.NoConfidence prevGovId ->
    MotionOfNoConfidence prevGovId
  Gov.NewConstitution prevGovId constitution ->
    ProposeNewConstitution prevGovId $ Gov.constitutionAnchor constitution
  Gov.ParameterChange prevGovId pparams ->
    UpdatePParams prevGovId pparams
  Gov.HardForkInitiation prevGovId pVer ->
    InitiateHardfork prevGovId pVer
  Gov.TreasuryWithdrawals withdrawlMap ->
    let res = [ (L.getRwdNetwork rwdAcnt, fromShelleyStakeCredential (L.getRwdCred rwdAcnt), coin)
              | (rwdAcnt, coin) <- Map.toList withdrawlMap
              ]
    in TreasuryWithdrawal res
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
  -> L.Coin -- ^ Deposit
  -> Hash StakeKey -- ^ Return address
  -> GovernanceAction era
  -> Ledger.Anchor StandardCrypto
  -> Proposal era
createProposalProcedure sbe nw dep (StakeKeyHash retAddrh) govAct anchor =
  shelleyBasedEraConstraints sbe $
    Proposal Gov.ProposalProcedure
      { Gov.pProcDeposit = dep
      , Gov.pProcReturnAddr = L.mkRwdAcnt nw (L.KeyHashObj retAddrh)
      , Gov.pProcGovAction = toGovernanceAction sbe govAct
      , Gov.pProcAnchor = anchor
      }

fromProposalProcedure
  :: ShelleyBasedEra era
  -> Proposal era
  -> (L.Coin, Hash StakeKey, GovernanceAction era)
fromProposalProcedure sbe (Proposal pp) =
  shelleyBasedEraConstraints sbe
    ( Gov.pProcDeposit pp
    , case fromShelleyStakeCredential (L.getRwdCred (Gov.pProcReturnAddr pp)) of
          StakeCredentialByKey keyhash -> keyhash
          StakeCredentialByScript _scripthash ->
            error "fromProposalProcedure TODO: Conway era script reward addresses not yet supported"
    , fromGovernanceAction (Gov.pProcGovAction pp)
    )


createPreviousGovernanceActionId
  :: TxId
  -> Word32 -- ^ Governance action transation index
  -> Ledger.PrevGovActionId  (r :: Ledger.GovActionPurpose) StandardCrypto
createPreviousGovernanceActionId  txid index =
   Ledger.PrevGovActionId $ createGovernanceActionId txid index


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


