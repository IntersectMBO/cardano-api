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
import           Cardano.Api.Experimental.Eras
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
import           Data.Maybe (fromMaybe)
import           Data.Typeable
import           Data.Word
import           GHC.Exts (IsList (..))

data AnyGovernanceAction = forall era. AnyGovernanceAction (Gov.GovAction era)

-- TODO: Conway - Transitiion to Ledger.GovAction
data GovernanceAction era
  = MotionOfNoConfidence
      (StrictMaybe (Ledger.GovPurposeId Ledger.CommitteePurpose (LedgerEra era)))
  | ProposeNewConstitution
      (StrictMaybe (Ledger.GovPurposeId Ledger.ConstitutionPurpose (LedgerEra era)))
      (Ledger.Anchor StandardCrypto)
      (StrictMaybe (Shelley.ScriptHash StandardCrypto))
  | ProposeNewCommittee
      (StrictMaybe (Ledger.GovPurposeId Ledger.CommitteePurpose (LedgerEra era)))
      [L.Credential ColdCommitteeRole StandardCrypto]
      -- ^ Old constitutional committee
      (Map (L.Credential ColdCommitteeRole StandardCrypto) EpochNo)
      -- ^ New committee members with epoch number when each of them expires
      Rational
      -- ^ Quorum of the committee that is necessary for a successful vote
  | InfoAct
  | -- | Governance policy
    TreasuryWithdrawal
      [(Network, StakeCredential, L.Coin)]
      !(StrictMaybe (Shelley.ScriptHash StandardCrypto))
  | InitiateHardfork
      (StrictMaybe (Ledger.GovPurposeId Ledger.HardForkPurpose (LedgerEra era)))
      ProtVer
  | -- | Governance policy
    UpdatePParams
      (StrictMaybe (Ledger.GovPurposeId Ledger.PParamUpdatePurpose (LedgerEra era)))
      (Ledger.PParamsUpdate (LedgerEra era))
      !(StrictMaybe (Shelley.ScriptHash StandardCrypto))

toGovernanceAction
  :: ()
  => GovernanceAction ConwayEra
  -> Gov.GovAction (LedgerEra ConwayEra)
toGovernanceAction =
  \case
    MotionOfNoConfidence prevGovId ->
      Gov.NoConfidence prevGovId
    ProposeNewConstitution prevGovAction anchor mConstitutionScriptHash ->
      Gov.NewConstitution
        prevGovAction
        Gov.Constitution
          { Gov.constitutionAnchor = anchor
          , Gov.constitutionScript = mConstitutionScriptHash
          }
    ProposeNewCommittee prevGovId oldCommitteeMembers newCommitteeMembers quor ->
      Gov.UpdateCommittee
        prevGovId -- previous governance action id
        (fromList oldCommitteeMembers) -- members to remove
        newCommitteeMembers -- members to add
        ( fromMaybe
            ( error $
                mconcat
                  [ "toGovernanceAction: the given quorum "
                  , show quor
                  , " was outside of the unit interval!"
                  ]
            )
            $ boundRational @UnitInterval quor
        )
    InfoAct ->
      Gov.InfoAction
    TreasuryWithdrawal withdrawals govPol ->
      let m = fromList [(L.RewardAccount nw (toShelleyStakeCredential sc), l) | (nw, sc, l) <- withdrawals]
       in Gov.TreasuryWithdrawals m govPol
    InitiateHardfork prevGovId pVer ->
      Gov.HardForkInitiation prevGovId pVer
    UpdatePParams preGovId ppup govPol ->
      Gov.ParameterChange preGovId ppup govPol

fromGovernanceAction
  :: EraCrypto (LedgerEra era) ~ StandardCrypto
  => Gov.GovAction (LedgerEra era)
  -> GovernanceAction era
fromGovernanceAction = \case
  Gov.NoConfidence prevGovId ->
    MotionOfNoConfidence prevGovId
  Gov.NewConstitution prevGovId constitution ->
    ProposeNewConstitution
      prevGovId
      (Gov.constitutionAnchor constitution)
      (Gov.constitutionScript constitution)
  Gov.ParameterChange prevGovId pparams govPolicy ->
    UpdatePParams prevGovId pparams govPolicy
  Gov.HardForkInitiation prevGovId pVer ->
    InitiateHardfork prevGovId pVer
  Gov.TreasuryWithdrawals withdrawlMap govPolicy ->
    let res =
          [ (L.raNetwork rwdAcnt, fromShelleyStakeCredential (L.raCredential rwdAcnt), coin)
          | (rwdAcnt, coin) <- toList withdrawlMap
          ]
     in TreasuryWithdrawal res govPolicy
  Gov.UpdateCommittee prevGovId oldCommitteeMembers newCommitteeMembers quor ->
    ProposeNewCommittee
      prevGovId
      (toList oldCommitteeMembers)
      newCommitteeMembers
      (unboundRational quor)
  Gov.InfoAction ->
    InfoAct

newtype Proposal era = Proposal {unProposal :: Gov.ProposalProcedure (LedgerEra era)}

instance (IsEra era, Typeable era) => Show (Proposal era) where
  show (Proposal pp) = do
    let ppStr = obtainCommonConstraints (useEra @era) $ show pp
    "Proposal {unProposal = " <> ppStr <> "}"

instance (IsEra era, Typeable era) => Eq (Proposal era) where
  (Proposal pp1) == (Proposal pp2) = obtainCommonConstraints (useEra @era) $ pp1 == pp2

instance (IsEra era, Typeable era) => ToCBOR (Proposal era) where
  toCBOR (Proposal vp) = obtainCommonConstraints (useEra @era) $ Shelley.toEraCBOR @Conway.Conway vp

instance (IsEra era, Typeable era) => FromCBOR (Proposal era) where
  fromCBOR =
    Proposal <$> obtainCommonConstraints (useEra @era) (Shelley.fromEraCBOR @Conway.Conway)

instance (IsEra era, Typeable era, HasTypeProxy era) => SerialiseAsCBOR (Proposal era) where
  serialiseToCBOR = obtainCommonConstraints (useEra @era) CBOR.serialize'
  deserialiseFromCBOR _proxy = obtainCommonConstraints (useEra @era) CBOR.decodeFull'

instance (IsEra era, Typeable era, HasTypeProxy era) => HasTextEnvelope (Proposal era) where
  textEnvelopeType _ = "Governance proposal"

instance HasTypeProxy era => HasTypeProxy (Proposal era) where
  data AsType (Proposal era) = AsProposal
  proxyToAsType _ = AsProposal

createProposalProcedure
  :: Era era
  -> Network
  -> L.Coin
  -- ^ Deposit
  -> StakeCredential
  -- ^ Credential to return the deposit to.
  -> GovernanceAction era
  -> Ledger.Anchor StandardCrypto
  -> Proposal era
createProposalProcedure BabbageEra _nw _dep _cred _govAct _anchor =
  error "This case should not exist because mainnet is conway"
createProposalProcedure ConwayEra nw dep cred govAct anchor =
  Proposal
    Gov.ProposalProcedure
      { Gov.pProcDeposit = dep
      , Gov.pProcReturnAddr = L.RewardAccount nw $ toShelleyStakeCredential cred
      , Gov.pProcGovAction = toGovernanceAction govAct
      , Gov.pProcAnchor = anchor
      }

fromProposalProcedure
  :: Era era
  -> Proposal era
  -> (L.Coin, Hash StakeKey, GovernanceAction era)
fromProposalProcedure BabbageEra _ = error "This case should not exist because mainnet is conway"
fromProposalProcedure ConwayEra (Proposal pp) =
  ( Gov.pProcDeposit pp
  , case fromShelleyStakeCredential (L.raCredential (Gov.pProcReturnAddr pp)) of
      StakeCredentialByKey keyhash -> keyhash
      StakeCredentialByScript _scripthash ->
        error "fromProposalProcedure TODO: Conway era script reward addresses not yet supported"
  , fromGovernanceAction (Gov.pProcGovAction pp)
  )

createPreviousGovernanceActionId
  :: EraCrypto (LedgerEra era) ~ StandardCrypto
  => TxId
  -> Word16
  -- ^ Governance action transation index
  -> Ledger.GovPurposeId (r :: Ledger.GovActionPurpose) (LedgerEra era)
createPreviousGovernanceActionId txid index =
  Ledger.GovPurposeId $ createGovernanceActionId txid index

createGovernanceActionId :: TxId -> Word16 -> Gov.GovActionId StandardCrypto
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
