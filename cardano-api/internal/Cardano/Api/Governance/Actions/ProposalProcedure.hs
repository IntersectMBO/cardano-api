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
import           Cardano.Api.Eras
import           Cardano.Api.Eras.Constraints
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.ProtocolParameters
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
data GovernanceAction
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
  | TreasuryWithdrawal [(Network, StakeCredential, Lovelace)]
  | InitiateHardfork
      (StrictMaybe (Ledger.PrevGovActionId Ledger.HardForkPurpose StandardCrypto))
      ProtVer
  | UpdatePParams
      (StrictMaybe (Ledger.PrevGovActionId Ledger.PParamUpdatePurpose StandardCrypto))
      ProtocolParametersUpdate
  deriving (Eq, Show)


toGovernanceAction
  :: EraCrypto ledgerera ~ StandardCrypto
  => ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> GovernanceAction
  -> Gov.GovAction ledgerera
toGovernanceAction _ (MotionOfNoConfidence prevGovId) = Gov.NoConfidence prevGovId
toGovernanceAction _ (ProposeNewConstitution prevGovAction anchor) =
  Gov.NewConstitution prevGovAction Gov.Constitution
    { Gov.constitutionAnchor = anchor
    , Gov.constitutionScript = SNothing   -- TODO: Conway era
    }
toGovernanceAction _ (ProposeNewCommittee prevGovId oldCommitteeMembers newCommitteeMembers quor) =
  Gov.NewCommittee
    prevGovId
    (Set.fromList $ map toCommitteeMember oldCommitteeMembers)
    Gov.Committee
      { Gov.committeeMembers = Map.mapKeys toCommitteeMember newCommitteeMembers
      , Gov.committeeQuorum =
            fromMaybe
              (error $ mconcat ["toGovernanceAction: the given quorum "
                               , show quor
                               , " was outside of the unit interval!"
                               ])
          $ boundRational @UnitInterval quor
       }
toGovernanceAction _ InfoAct = Gov.InfoAction
toGovernanceAction _ (TreasuryWithdrawal withdrawals) =
  let m = Map.fromList [(L.mkRwdAcnt nw (toShelleyStakeCredential sc), toShelleyLovelace l) | (nw,sc,l) <- withdrawals]
  in Gov.TreasuryWithdrawals m
toGovernanceAction _ (InitiateHardfork prevGovId pVer) =
  Gov.HardForkInitiation prevGovId pVer
toGovernanceAction sbe (UpdatePParams preGovId ppup) =
  case toLedgerPParamsUpdate sbe ppup of
    Left e -> error $ "toGovernanceAction: " <> show e
    Right ppup' -> Gov.ParameterChange preGovId ppup'

fromGovernanceAction
  :: EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
  => ShelleyBasedEra era
  -> Gov.GovAction (ShelleyLedgerEra era)
  -> GovernanceAction
fromGovernanceAction sbe = \case
  Gov.NoConfidence prevGovId ->
    MotionOfNoConfidence prevGovId
  Gov.NewConstitution prevGovId constitution ->
    ProposeNewConstitution prevGovId $ Gov.constitutionAnchor constitution
  Gov.ParameterChange prevGovId pparams ->
    UpdatePParams prevGovId $ fromLedgerPParamsUpdate sbe pparams
  Gov.HardForkInitiation prevGovId pVer ->
    InitiateHardfork prevGovId pVer
  Gov.TreasuryWithdrawals withdrawlMap ->
    let res = [ (L.getRwdNetwork rwdAcnt, fromShelleyStakeCredential (L.getRwdCred rwdAcnt), fromShelleyLovelace coin)
              | (rwdAcnt, coin) <- Map.toList withdrawlMap
              ]
    in TreasuryWithdrawal res
  Gov.NewCommittee prevGovId oldCommitteeMembers newCommittee ->
    let Gov.Committee
          { Gov.committeeMembers = newCommitteeMembers
          , Gov.committeeQuorum = quor
          } = newCommittee
    in ProposeNewCommittee
         prevGovId
         (map fromCommitteeMember $ Set.toList oldCommitteeMembers)
         (Map.mapKeys fromCommitteeMember newCommitteeMembers)
         (unboundRational quor)
  Gov.InfoAction ->
    InfoAct

newtype Proposal era = Proposal { unProposal :: Gov.ProposalProcedure (ShelleyLedgerEra era) }

instance IsShelleyBasedEra era => Show (Proposal era) where
  show (Proposal pp) = do
    let ppStr = withShelleyBasedEraConstraintsForLedger (shelleyBasedEra @era) $ show pp
    "Proposal {unProposal = " <> ppStr <> "}"

instance IsShelleyBasedEra era => Eq (Proposal era) where
  (Proposal pp1) == (Proposal pp2) = withShelleyBasedEraConstraintsForLedger (shelleyBasedEra @era) $ pp1 == pp2

instance IsShelleyBasedEra era => ToCBOR (Proposal era) where
  toCBOR (Proposal vp) = withShelleyBasedEraConstraintsForLedger (shelleyBasedEra @era) $ Shelley.toEraCBOR @Conway.Conway vp

instance IsShelleyBasedEra era => FromCBOR (Proposal era) where
  fromCBOR = Proposal <$> withShelleyBasedEraConstraintsForLedger (shelleyBasedEra @era) (Shelley.fromEraCBOR @Conway.Conway)

instance IsShelleyBasedEra era => SerialiseAsCBOR (Proposal era) where
  serialiseToCBOR = withShelleyBasedEraConstraintsForLedger (shelleyBasedEra @era) CBOR.serialize'
  deserialiseFromCBOR _proxy = withShelleyBasedEraConstraintsForLedger (shelleyBasedEra @era) CBOR.decodeFull'

instance IsShelleyBasedEra era => HasTextEnvelope (Proposal era) where
  textEnvelopeType _ = "Governance proposal"

instance HasTypeProxy era => HasTypeProxy (Proposal era) where
    data AsType (Proposal era) = AsProposal
    proxyToAsType _ = AsProposal


createProposalProcedure
  :: ShelleyBasedEra era
  -> Network
  -> Lovelace -- ^ Deposit
  -> Hash StakeKey -- ^ Return address
  -> GovernanceAction
  -> Ledger.Anchor StandardCrypto
  -> Proposal era
createProposalProcedure sbe nw dep (StakeKeyHash retAddrh) govAct anchor =
  shelleyBasedEraConstraints sbe $ shelleyBasedEraConstraints sbe $
    Proposal Gov.ProposalProcedure
      { Gov.pProcDeposit = toShelleyLovelace dep
      , Gov.pProcReturnAddr = L.mkRwdAcnt nw (L.KeyHashObj retAddrh)
      , Gov.pProcGovAction = toGovernanceAction sbe govAct
      , Gov.pProcAnchor = anchor
      }

fromProposalProcedure
  :: ShelleyBasedEra era
  -> Proposal era
  -> (Lovelace, Hash StakeKey, GovernanceAction)
fromProposalProcedure sbe (Proposal pp) =
  shelleyBasedEraConstraints sbe
    ( fromShelleyLovelace $ Gov.pProcDeposit pp
    , case fromShelleyStakeCredential (L.getRwdCred (Gov.pProcReturnAddr pp)) of
          StakeCredentialByKey keyhash -> keyhash
          StakeCredentialByScript _scripthash ->
            error "fromProposalProcedure TODO: Conway era script reward addresses not yet supported"
    , fromGovernanceAction sbe (Gov.pProcGovAction pp)
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


