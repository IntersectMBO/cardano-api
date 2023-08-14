{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Governance.Actions.ProposalProcedure where

import           Cardano.Api.Address
import           Cardano.Api.Eras
import           Cardano.Api.Feature.ConwayEraOnwards
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.ProtocolParameters
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.Value

import qualified Cardano.Binary as CBOR
import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Address as L
import           Cardano.Ledger.BaseTypes
import qualified Cardano.Ledger.Conway as Conway
import qualified Cardano.Ledger.Conway.Governance as Gov
import           Cardano.Ledger.Core (EraCrypto)
import qualified Cardano.Ledger.Core as Shelley
import qualified Cardano.Ledger.Credential as L
import           Cardano.Ledger.Crypto (HASH, StandardCrypto)
import           Cardano.Ledger.Keys (HasKeyRole (coerceKeyRole), KeyRole (ColdCommitteeRole))
import           Cardano.Ledger.SafeHash

import           Data.ByteString (ByteString)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import qualified Data.Set as Set

-- | A representation of whether the era supports tx governance actions.
--
-- The Conway and subsequent eras support governance actions.
--
data TxGovernanceActions era where
  TxGovernanceActionsNone :: TxGovernanceActions era

  TxGovernanceActions
    :: ConwayEraOnwards era
    -> [Proposal era]
    -> TxGovernanceActions era

deriving instance IsShelleyBasedEra era => Show (TxGovernanceActions era)
deriving instance IsShelleyBasedEra era => Eq (TxGovernanceActions era)

data AnyGovernanceAction = forall era. AnyGovernanceAction (Gov.GovAction era)

-- TODO: Conway - fill in remaining actions
data GovernanceAction
  = MotionOfNoConfidence
  | ProposeNewConstitution ByteString   -- NB: This is the bytes of the hash, not the bytes to be hashed
  | ProposeNewCommittee [Hash StakeKey] (Map (Hash StakeKey) EpochNo) Rational -- NB: This also includes stake pool keys
  | InfoAct
  | TreasuryWithdrawal [(StakeCredential, Lovelace)]
  | InitiateHardfork ProtVer
  | UpdatePParams ProtocolParametersUpdate
  deriving (Eq, Show)

-- | The cardano-api:'GovernanceAction' interface does not yet include the id
-- of the previous relevant governance action, but the `cardano-ledger`
-- 'Gov.GovAction' does optionally include it.
temporarilyOptOutOfPrevGovAction :: StrictMaybe (Gov.PrevGovActionId purpose c)   -- TODO
temporarilyOptOutOfPrevGovAction = SNothing

toGovernanceAction
  :: EraCrypto ledgerera ~ StandardCrypto
  => ShelleyLedgerEra era ~ ledgerera
  => ShelleyBasedEra era
  -> GovernanceAction
  -> Gov.GovAction ledgerera
toGovernanceAction _ MotionOfNoConfidence = Gov.NoConfidence temporarilyOptOutOfPrevGovAction
toGovernanceAction _ (ProposeNewConstitution bs) =
  Gov.NewConstitution temporarilyOptOutOfPrevGovAction Gov.Constitution
    { Gov.constitutionAnchor = Gov.Anchor
      { Gov.anchorUrl = error "TODO new constitution anchorUrl"
      , Gov.anchorDataHash = unsafeBytesToSafeHash bs   -- TODO "safe*" alternative?
      }
    , Gov.constitutionScript = SNothing   -- TODO
    }
toGovernanceAction _ (ProposeNewCommittee oldCommitteeMembers newCommitteeMembers quor) =
  Gov.NewCommittee
    temporarilyOptOutOfPrevGovAction
    (Set.fromList $ map toCommitteeMember oldCommitteeMembers)
    Gov.Committee
      { Gov.committeeMembers = Map.mapKeys toCommitteeMember newCommitteeMembers
      , Gov.committeeQuorum =
            fromMaybe (error "the given quorum was outside of the unit interval!")
          $ boundRational @UnitInterval quor
       }
toGovernanceAction _ InfoAct = Gov.InfoAction
toGovernanceAction _ (TreasuryWithdrawal withdrawals) =
  let m = Map.fromList [(L.mkRwdAcnt (error "TODO which Network?") (toShelleyStakeCredential sc), toShelleyLovelace l) | (sc,l) <- withdrawals]
  in Gov.TreasuryWithdrawals m
toGovernanceAction _ (InitiateHardfork pVer) = Gov.HardForkInitiation temporarilyOptOutOfPrevGovAction pVer
toGovernanceAction sbe (UpdatePParams ppup) =
  case toLedgerPParamsUpdate sbe ppup of
    Left e -> error $ "toGovernanceAction: " <> show e
    -- TODO: Conway era - remove use of error. Ideally we will use the ledger's PParams type
    -- in place of ProtocolParametersUpdate
    Right ppup' -> Gov.ParameterChange temporarilyOptOutOfPrevGovAction ppup'

fromGovernanceAction
  :: EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
  => ShelleyBasedEra era
  -> Gov.GovAction (ShelleyLedgerEra era)
  -> GovernanceAction
fromGovernanceAction sbe = \case
  Gov.NoConfidence _TODO ->
    MotionOfNoConfidence
  Gov.NewConstitution _TODO constitution ->
    ProposeNewConstitution $ originalBytes $ Gov.anchorDataHash $ Gov.constitutionAnchor constitution
  Gov.ParameterChange _TODO pparams ->
    UpdatePParams $ fromLedgerPParamsUpdate sbe pparams
  Gov.HardForkInitiation _TODO pVer ->
    InitiateHardfork pVer
  Gov.TreasuryWithdrawals withdrawlMap ->
    let res = [ (fromShelleyStakeCredential (L.getRwdCred rwdAcnt), fromShelleyLovelace coin)
              | (rwdAcnt, coin) <- Map.toList withdrawlMap
              ]
    in TreasuryWithdrawal res
  Gov.NewCommittee _TODO oldCommitteeMembers newCommittee ->
    let Gov.Committee
          { Gov.committeeMembers = newCommitteeMembers
          , Gov.committeeQuorum = quor
          } = newCommittee
    in ProposeNewCommittee
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
  -> Lovelace -- ^ Deposit
  -> Hash StakeKey -- ^ Return address
  -> GovernanceAction
  -> Proposal era
createProposalProcedure sbe dep (StakeKeyHash retAddrh) govAct =
  shelleyBasedEraConstraints sbe $ shelleyBasedEraConstraints sbe $
    Proposal Gov.ProposalProcedure
      { Gov.pProcDeposit = toShelleyLovelace dep
      , Gov.pProcReturnAddr = L.mkRwdAcnt (error "TODO which network?") (L.KeyHashObj retAddrh)
      , Gov.pProcGovAction = toGovernanceAction sbe govAct
      , Gov.pProcAnchor = error "TODO which anchor?"
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
          StakeCredentialByScript _scripthash -> error "TODO reward addresses not yet supported"
    , fromGovernanceAction sbe (Gov.pProcGovAction pp)
    )

-- ----------------------------------------------------------------------------
-- TODO conversions that likely need to live elsewhere and may even deserve
-- additional wrapper types

toCommitteeMember :: Hash StakeKey -> L.Credential ColdCommitteeRole StandardCrypto
toCommitteeMember (StakeKeyHash keyhash) = coerceKeyRole $ L.KeyHashObj keyhash

fromCommitteeMember :: L.Credential ColdCommitteeRole StandardCrypto -> Hash StakeKey
fromCommitteeMember = (. coerceKeyRole) $ \case
  L.KeyHashObj keyhash -> StakeKeyHash keyhash
  L.ScriptHashObj _scripthash -> error "TODO script committee members not yet supported"

-- | TODO: I'm very unsure whether it's correct to use 'unsafeMakeSafeHash'
-- here? I've asked Alexey if maybe this doesn't need to actually be a
-- 'SafeHash', which would let us remove it.
unsafeBytesToSafeHash
  :: Crypto.HashAlgorithm (HASH c)
  => ByteString
  -> SafeHash c a
unsafeBytesToSafeHash bs = case Crypto.hashFromBytes bs of
  Nothing -> error "the argument to `ProposeNewConstitution' must be a valid hash"
  Just h -> unsafeMakeSafeHash h
