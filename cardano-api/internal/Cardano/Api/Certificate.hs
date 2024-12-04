{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Certificates embedded in transactions
module Cardano.Api.Certificate
  ( Certificate (..)

    -- * Registering stake address and delegating
  , StakeAddressRequirements (..)
  , StakeDelegationRequirements (..)
  , makeStakeAddressDelegationCertificate
  , makeStakeAddressRegistrationCertificate
  , makeStakeAddressUnregistrationCertificate
  , PoolId

    -- * Registering stake pools
  , StakePoolRegistrationRequirements (..)
  , StakePoolRetirementRequirements (..)
  , makeStakePoolRegistrationCertificate
  , makeStakePoolRetirementCertificate
  , StakePoolParameters (..)
  , StakePoolRelay (..)
  , StakePoolMetadataReference (..)

    -- * Conway specific certificates
  , CommitteeColdkeyResignationRequirements (..)
  , CommitteeHotKeyAuthorizationRequirements (..)
  , DRepRegistrationRequirements (..)
  , DRepUnregistrationRequirements (..)
  , DRepUpdateRequirements (..)
  , makeCommitteeColdkeyResignationCertificate
  , makeCommitteeHotKeyAuthorizationCertificate
  , makeDrepRegistrationCertificate
  , makeDrepUnregistrationCertificate
  , makeDrepUpdateCertificate
  , makeStakeAddressAndDRepDelegationCertificate

    -- * Registering DReps
  , DRepMetadataReference (..)

    -- * Special certificates
  , GenesisKeyDelegationRequirements (..)
  , MirCertificateRequirements (..)
  , makeMIRCertificate
  , makeGenesisKeyDelegationCertificate
  , Ledger.MIRTarget (..)
  , Ledger.MIRPot (..)
  , selectStakeCredentialWitness

    -- * Anchor data
  , AnchorDataFromCertificateError (..)
  , getAnchorDataFromCertificate

    -- * Internal conversion functions
  , toShelleyCertificate
  , fromShelleyCertificate
  , toShelleyPoolParams
  , fromShelleyPoolParams

    -- * Data family instances
  , AsType (..)

    -- * Internal functions
  , filterUnRegCreds
  , filterUnRegDRepCreds
  , isDRepRegOrUpdateCert
  )
where

import           Cardano.Api.Address
import           Cardano.Api.DRepMetadata
import           Cardano.Api.Eon.Convert
import           Cardano.Api.Eon.ConwayEraOnwards
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eon.ShelleyToBabbageEra
import           Cardano.Api.Eras
import           Cardano.Api.Error (Error (..))
import           Cardano.Api.Governance.Actions.VotingProcedure
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Keys.Praos
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.Pretty (Doc)
import           Cardano.Api.ReexposeLedger (EraCrypto, StandardCrypto)
import qualified Cardano.Api.ReexposeLedger as Ledger
import           Cardano.Api.Script
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.StakePoolMetadata
import           Cardano.Api.Utils (noInlineMaybeToStrictMaybe)
import           Cardano.Api.Value

import           Cardano.Ledger.BaseTypes (strictMaybe)
import qualified Cardano.Ledger.Coin as L
import qualified Cardano.Ledger.Keys as Ledger

import           Control.Monad.Except (MonadError (..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.IP (IPv4, IPv6)
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Typeable
import           GHC.Exts (IsList (..), fromString)
import           Network.Socket (PortNumber)

-- ----------------------------------------------------------------------------
-- Certificates embedded in transactions
--

data Certificate era where
  -- Pre-Conway
  --   1. Stake registration
  --   2. Stake unregistration
  --   3. Stake delegation
  --   4. Pool retirement
  --   5. Pool registration
  --   6. Genesis delegation
  --   7. MIR certificates
  ShelleyRelatedCertificate
    :: ShelleyToBabbageEra era
    -> Ledger.ShelleyTxCert (ShelleyLedgerEra era)
    -> Certificate era
  -- Conway onwards
  -- TODO: Add comments about the new types of certificates
  ConwayCertificate
    :: ConwayEraOnwards era
    -> Ledger.ConwayTxCert (ShelleyLedgerEra era)
    -> Certificate era
  deriving anyclass SerialiseAsCBOR

deriving instance Eq (Certificate era)

deriving instance Show (Certificate era)

instance Typeable era => HasTypeProxy (Certificate era) where
  data AsType (Certificate era) = AsCertificate
  proxyToAsType _ = AsCertificate

instance
  forall era
   . IsShelleyBasedEra era
  => ToCBOR (Certificate era)
  where
  toCBOR =
    shelleyBasedEraConstraints (shelleyBasedEra @era) $
      Ledger.toEraCBOR @(ShelleyLedgerEra era) . toShelleyCertificate

instance
  IsShelleyBasedEra era
  => FromCBOR (Certificate era)
  where
  fromCBOR =
    shelleyBasedEraConstraints (shelleyBasedEra @era) $
      fromShelleyCertificate shelleyBasedEra <$> Ledger.fromEraCBOR @(ShelleyLedgerEra era)

instance
  IsShelleyBasedEra era
  => HasTextEnvelope (Certificate era)
  where
  textEnvelopeType _ =
    forEraInEon @ConwayEraOnwards
      (cardanoEra :: CardanoEra era)
      "CertificateShelley"
      (const "CertificateConway")
  textEnvelopeDefaultDescr cert = case cert of
    ShelleyRelatedCertificate _ (Ledger.ShelleyTxCertDelegCert Ledger.ShelleyRegCert{}) -> "Stake address registration"
    ShelleyRelatedCertificate _ (Ledger.ShelleyTxCertDelegCert Ledger.ShelleyUnRegCert{}) -> "Stake address deregistration"
    ShelleyRelatedCertificate _ (Ledger.ShelleyTxCertDelegCert Ledger.ShelleyDelegCert{}) -> "Stake address delegation"
    ShelleyRelatedCertificate _ (Ledger.ShelleyTxCertPool Ledger.RetirePool{}) -> "Pool retirement"
    ShelleyRelatedCertificate _ (Ledger.ShelleyTxCertPool Ledger.RegPool{}) -> "Pool registration"
    ShelleyRelatedCertificate _ Ledger.ShelleyTxCertGenesisDeleg{} -> "Genesis key delegation"
    ShelleyRelatedCertificate _ Ledger.ShelleyTxCertMir{} -> "MIR"
    -- Conway and onwards related
    -- Constitutional Committee related
    ConwayCertificate _ (Ledger.ConwayTxCertGov Ledger.ConwayRegDRep{}) -> "Constitution committee member key registration"
    ConwayCertificate _ (Ledger.ConwayTxCertGov Ledger.ConwayUnRegDRep{}) -> "Constitution committee member key unregistration"
    ConwayCertificate _ (Ledger.ConwayTxCertGov Ledger.ConwayUpdateDRep{}) -> "Constitution committee member key registration update"
    ConwayCertificate _ (Ledger.ConwayTxCertGov Ledger.ConwayAuthCommitteeHotKey{}) -> "Constitution committee member hot key registration"
    ConwayCertificate _ (Ledger.ConwayTxCertGov Ledger.ConwayResignCommitteeColdKey{}) -> "Constitution committee member hot key resignation"
    ConwayCertificate _ (Ledger.ConwayTxCertDeleg Ledger.ConwayRegCert{}) -> "Stake address registration"
    ConwayCertificate _ (Ledger.ConwayTxCertDeleg Ledger.ConwayUnRegCert{}) -> "Stake address deregistration"
    ConwayCertificate _ (Ledger.ConwayTxCertDeleg Ledger.ConwayDelegCert{}) -> "Stake address delegation"
    ConwayCertificate _ (Ledger.ConwayTxCertDeleg Ledger.ConwayRegDelegCert{}) -> "Stake address registration and delegation"
    ConwayCertificate _ (Ledger.ConwayTxCertPool Ledger.RegPool{}) -> "Pool registration"
    ConwayCertificate _ (Ledger.ConwayTxCertPool Ledger.RetirePool{}) -> "Pool retirement"

-- ----------------------------------------------------------------------------
-- Stake pool parameters
--

type PoolId = Hash StakePoolKey

data StakePoolParameters
  = StakePoolParameters
  { stakePoolId :: PoolId
  , stakePoolVRF :: Hash VrfKey
  , stakePoolCost :: L.Coin
  , stakePoolMargin :: Rational
  , stakePoolRewardAccount :: StakeAddress
  , stakePoolPledge :: L.Coin
  , stakePoolOwners :: [Hash StakeKey]
  , stakePoolRelays :: [StakePoolRelay]
  , stakePoolMetadata :: Maybe StakePoolMetadataReference
  }
  deriving (Eq, Show)

data StakePoolRelay
  = -- | One or both of IPv4 & IPv6
    StakePoolRelayIp
      (Maybe IPv4)
      (Maybe IPv6)
      (Maybe PortNumber)
  | -- | An DNS name pointing to a @A@ or @AAAA@ record.
    StakePoolRelayDnsARecord
      ByteString
      (Maybe PortNumber)
  | -- | A DNS name pointing to a @SRV@ record.
    StakePoolRelayDnsSrvRecord
      ByteString
  deriving (Eq, Show)

data StakePoolMetadataReference
  = StakePoolMetadataReference
  { stakePoolMetadataURL :: Text
  , stakePoolMetadataHash :: Hash StakePoolMetadata
  }
  deriving (Eq, Show)

-- ----------------------------------------------------------------------------
-- DRep parameters
--

data DRepMetadataReference
  = DRepMetadataReference
  { drepMetadataURL :: Text
  , drepMetadataHash :: Hash DRepMetadata
  }
  deriving (Eq, Show)

-- ----------------------------------------------------------------------------
-- Constructor functions
--

data StakeAddressRequirements era where
  StakeAddrRegistrationConway
    :: ConwayEraOnwards era
    -> L.Coin
    -> StakeCredential
    -> StakeAddressRequirements era
  StakeAddrRegistrationPreConway
    :: ShelleyToBabbageEra era
    -> StakeCredential
    -> StakeAddressRequirements era

makeStakeAddressRegistrationCertificate :: StakeAddressRequirements era -> Certificate era
makeStakeAddressRegistrationCertificate = \case
  StakeAddrRegistrationPreConway w scred ->
    shelleyToBabbageEraConstraints w $
      ShelleyRelatedCertificate w $
        Ledger.mkRegTxCert $
          toShelleyStakeCredential scred
  StakeAddrRegistrationConway cOnwards deposit scred ->
    conwayEraOnwardsConstraints cOnwards $
      ConwayCertificate cOnwards $
        Ledger.mkRegDepositTxCert (toShelleyStakeCredential scred) deposit

makeStakeAddressUnregistrationCertificate :: StakeAddressRequirements era -> Certificate era
makeStakeAddressUnregistrationCertificate req =
  case req of
    StakeAddrRegistrationConway cOnwards deposit scred ->
      conwayEraOnwardsConstraints cOnwards $
        ConwayCertificate cOnwards $
          Ledger.mkUnRegDepositTxCert (toShelleyStakeCredential scred) deposit
    StakeAddrRegistrationPreConway atMostEra scred ->
      shelleyToBabbageEraConstraints atMostEra $
        ShelleyRelatedCertificate atMostEra $
          Ledger.mkUnRegTxCert $
            toShelleyStakeCredential scred

data StakeDelegationRequirements era where
  StakeDelegationRequirementsConwayOnwards
    :: ConwayEraOnwards era
    -> StakeCredential
    -> Ledger.Delegatee (EraCrypto (ShelleyLedgerEra era))
    -> StakeDelegationRequirements era
  StakeDelegationRequirementsPreConway
    :: ShelleyToBabbageEra era
    -> StakeCredential
    -> PoolId
    -> StakeDelegationRequirements era

makeStakeAddressDelegationCertificate :: StakeDelegationRequirements era -> Certificate era
makeStakeAddressDelegationCertificate = \case
  StakeDelegationRequirementsConwayOnwards cOnwards scred delegatee ->
    conwayEraOnwardsConstraints cOnwards $
      ConwayCertificate cOnwards $
        Ledger.mkDelegTxCert (toShelleyStakeCredential scred) delegatee
  StakeDelegationRequirementsPreConway atMostBabbage scred pid ->
    shelleyToBabbageEraConstraints atMostBabbage $
      ShelleyRelatedCertificate atMostBabbage $
        Ledger.mkDelegStakeTxCert (toShelleyStakeCredential scred) (unStakePoolKeyHash pid)

data StakePoolRegistrationRequirements era where
  StakePoolRegistrationRequirementsConwayOnwards
    :: ConwayEraOnwards era
    -> Ledger.PoolParams (EraCrypto (ShelleyLedgerEra era))
    -> StakePoolRegistrationRequirements era
  StakePoolRegistrationRequirementsPreConway
    :: ShelleyToBabbageEra era
    -> Ledger.PoolParams (EraCrypto (ShelleyLedgerEra era))
    -> StakePoolRegistrationRequirements era

makeStakePoolRegistrationCertificate
  :: ()
  => StakePoolRegistrationRequirements era
  -> Certificate era
makeStakePoolRegistrationCertificate = \case
  StakePoolRegistrationRequirementsConwayOnwards cOnwards poolParams ->
    conwayEraOnwardsConstraints cOnwards $
      ConwayCertificate cOnwards $
        Ledger.mkRegPoolTxCert poolParams
  StakePoolRegistrationRequirementsPreConway atMostBab poolParams ->
    shelleyToBabbageEraConstraints atMostBab $
      ShelleyRelatedCertificate atMostBab $
        Ledger.mkRegPoolTxCert poolParams

data StakePoolRetirementRequirements era where
  StakePoolRetirementRequirementsConwayOnwards
    :: ConwayEraOnwards era
    -> PoolId
    -> Ledger.EpochNo
    -> StakePoolRetirementRequirements era
  StakePoolRetirementRequirementsPreConway
    :: ShelleyToBabbageEra era
    -> PoolId
    -> Ledger.EpochNo
    -> StakePoolRetirementRequirements era

makeStakePoolRetirementCertificate
  :: ()
  => StakePoolRetirementRequirements era
  -> Certificate era
makeStakePoolRetirementCertificate req =
  case req of
    StakePoolRetirementRequirementsPreConway atMostBab poolId retirementEpoch ->
      shelleyToBabbageEraConstraints atMostBab $
        ShelleyRelatedCertificate atMostBab $
          Ledger.mkRetirePoolTxCert (unStakePoolKeyHash poolId) retirementEpoch
    StakePoolRetirementRequirementsConwayOnwards atMostBab poolId retirementEpoch ->
      conwayEraOnwardsConstraints atMostBab $
        ConwayCertificate atMostBab $
          Ledger.mkRetirePoolTxCert (unStakePoolKeyHash poolId) retirementEpoch

data GenesisKeyDelegationRequirements era where
  GenesisKeyDelegationRequirements
    :: ShelleyToBabbageEra era
    -> Hash GenesisKey
    -> Hash GenesisDelegateKey
    -> Hash VrfKey
    -> GenesisKeyDelegationRequirements era

makeGenesisKeyDelegationCertificate :: GenesisKeyDelegationRequirements era -> Certificate era
makeGenesisKeyDelegationCertificate
  ( GenesisKeyDelegationRequirements
      atMostEra
      (GenesisKeyHash hGenKey)
      (GenesisDelegateKeyHash hGenDelegKey)
      (VrfKeyHash hVrfKey)
    ) =
    ShelleyRelatedCertificate atMostEra $
      shelleyToBabbageEraConstraints atMostEra $
        Ledger.ShelleyTxCertGenesisDeleg $
          Ledger.GenesisDelegCert hGenKey hGenDelegKey (Ledger.toVRFVerKeyHash hVrfKey)

data MirCertificateRequirements era where
  MirCertificateRequirements
    :: ShelleyToBabbageEra era
    -> Ledger.MIRPot
    -> Ledger.MIRTarget (EraCrypto (ShelleyLedgerEra era))
    -> MirCertificateRequirements era

makeMIRCertificate
  :: ()
  => MirCertificateRequirements era
  -> Certificate era
makeMIRCertificate (MirCertificateRequirements atMostEra mirPot mirTarget) =
  ShelleyRelatedCertificate atMostEra $
    Ledger.ShelleyTxCertMir $
      Ledger.MIRCert mirPot mirTarget

data DRepRegistrationRequirements era where
  DRepRegistrationRequirements
    :: ConwayEraOnwards era
    -> (Ledger.Credential Ledger.DRepRole (EraCrypto (ShelleyLedgerEra era)))
    -> L.Coin
    -> DRepRegistrationRequirements era

makeDrepRegistrationCertificate
  :: ()
  => DRepRegistrationRequirements era
  -> Maybe (Ledger.Anchor (EraCrypto (ShelleyLedgerEra era)))
  -> Certificate era
makeDrepRegistrationCertificate (DRepRegistrationRequirements conwayOnwards vcred deposit) anchor =
  ConwayCertificate conwayOnwards
    . Ledger.ConwayTxCertGov
    $ Ledger.ConwayRegDRep vcred deposit (noInlineMaybeToStrictMaybe anchor)

data CommitteeHotKeyAuthorizationRequirements era where
  CommitteeHotKeyAuthorizationRequirements
    :: ConwayEraOnwards era
    -> Ledger.Credential Ledger.ColdCommitteeRole (EraCrypto (ShelleyLedgerEra era))
    -> Ledger.Credential Ledger.HotCommitteeRole (EraCrypto (ShelleyLedgerEra era))
    -> CommitteeHotKeyAuthorizationRequirements era

makeCommitteeHotKeyAuthorizationCertificate
  :: ()
  => CommitteeHotKeyAuthorizationRequirements era
  -> Certificate era
makeCommitteeHotKeyAuthorizationCertificate (CommitteeHotKeyAuthorizationRequirements cOnwards coldKeyCredential hotKeyCredential) =
  ConwayCertificate cOnwards
    . Ledger.ConwayTxCertGov
    $ Ledger.ConwayAuthCommitteeHotKey coldKeyCredential hotKeyCredential

data CommitteeColdkeyResignationRequirements era where
  CommitteeColdkeyResignationRequirements
    :: ConwayEraOnwards era
    -> Ledger.Credential Ledger.ColdCommitteeRole (EraCrypto (ShelleyLedgerEra era))
    -> Maybe (Ledger.Anchor (EraCrypto (ShelleyLedgerEra era)))
    -> CommitteeColdkeyResignationRequirements era

makeCommitteeColdkeyResignationCertificate
  :: ()
  => CommitteeColdkeyResignationRequirements era
  -> Certificate era
makeCommitteeColdkeyResignationCertificate (CommitteeColdkeyResignationRequirements cOnwards coldKeyCred anchor) =
  ConwayCertificate cOnwards
    . Ledger.ConwayTxCertGov
    $ Ledger.ConwayResignCommitteeColdKey
      coldKeyCred
      (noInlineMaybeToStrictMaybe anchor)

data DRepUnregistrationRequirements era where
  DRepUnregistrationRequirements
    :: ConwayEraOnwards era
    -> (Ledger.Credential Ledger.DRepRole (EraCrypto (ShelleyLedgerEra era)))
    -> L.Coin
    -> DRepUnregistrationRequirements era

makeDrepUnregistrationCertificate
  :: ()
  => DRepUnregistrationRequirements era
  -> Certificate era
makeDrepUnregistrationCertificate (DRepUnregistrationRequirements conwayOnwards vcred deposit) =
  ConwayCertificate conwayOnwards
    . Ledger.ConwayTxCertGov
    $ Ledger.ConwayUnRegDRep vcred deposit

makeStakeAddressAndDRepDelegationCertificate
  :: ()
  => ConwayEraOnwards era
  -> StakeCredential
  -> Ledger.Delegatee (EraCrypto (ShelleyLedgerEra era))
  -> L.Coin
  -> Certificate era
makeStakeAddressAndDRepDelegationCertificate w cred delegatee deposit =
  conwayEraOnwardsConstraints w $
    ConwayCertificate w $
      Ledger.mkRegDepositDelegTxCert (toShelleyStakeCredential cred) delegatee deposit

data DRepUpdateRequirements era where
  DRepUpdateRequirements
    :: ConwayEraOnwards era
    -> Ledger.Credential Ledger.DRepRole (EraCrypto (ShelleyLedgerEra era))
    -> DRepUpdateRequirements era

makeDrepUpdateCertificate
  :: DRepUpdateRequirements era
  -> Maybe (Ledger.Anchor (EraCrypto (ShelleyLedgerEra era)))
  -> Certificate era
makeDrepUpdateCertificate (DRepUpdateRequirements conwayOnwards vcred) mAnchor =
  ConwayCertificate conwayOnwards
    . Ledger.ConwayTxCertGov
    $ Ledger.ConwayUpdateDRep vcred (noInlineMaybeToStrictMaybe mAnchor)

-- ----------------------------------------------------------------------------
-- Helper functions
--

getTxCertWitness
  :: ShelleyBasedEra era
  -> Ledger.TxCert (ShelleyLedgerEra era)
  -> Maybe StakeCredential
getTxCertWitness sbe ledgerCert = shelleyBasedEraConstraints sbe $
  case Ledger.getVKeyWitnessTxCert ledgerCert of
    Just keyHash -> Just $ StakeCredentialByKey $ StakeKeyHash $ Ledger.coerceKeyRole keyHash
    Nothing ->
      StakeCredentialByScript . fromShelleyScriptHash
        <$> Ledger.getScriptWitnessTxCert ledgerCert

-- | Get the stake credential witness for a certificate that requires it.
-- Only stake address deregistration and delegation requires witnessing (witness can be script or key).
selectStakeCredentialWitness
  :: Certificate era
  -> Maybe StakeCredential
selectStakeCredentialWitness = \case
  ShelleyRelatedCertificate stbEra shelleyCert ->
    shelleyToBabbageEraConstraints stbEra $
      getTxCertWitness (convert stbEra) shelleyCert
  ConwayCertificate cEra conwayCert ->
    conwayEraOnwardsConstraints cEra $
      getTxCertWitness (convert cEra) conwayCert

filterUnRegCreds
  :: Certificate era -> Maybe StakeCredential
filterUnRegCreds =
  fmap fromShelleyStakeCredential . \case
    ShelleyRelatedCertificate stbEra shelleyCert -> shelleyToBabbageEraConstraints stbEra $
      case shelleyCert of
        Ledger.RegTxCert _ -> Nothing
        Ledger.UnRegTxCert cred -> Just cred
        Ledger.DelegStakeTxCert _ _ -> Nothing
        Ledger.RegPoolTxCert _ -> Nothing
        Ledger.RetirePoolTxCert _ _ -> Nothing
        Ledger.MirTxCert _ -> Nothing
        Ledger.GenesisDelegTxCert{} -> Nothing
    ConwayCertificate cEra conwayCert -> conwayEraOnwardsConstraints cEra $
      case conwayCert of
        Ledger.RegPoolTxCert _ -> Nothing
        Ledger.RetirePoolTxCert _ _ -> Nothing
        Ledger.RegDepositTxCert _ _ -> Nothing
        Ledger.UnRegDepositTxCert cred _ -> Just cred
        Ledger.DelegTxCert _ _ -> Nothing
        Ledger.RegDepositDelegTxCert{} -> Nothing
        Ledger.AuthCommitteeHotKeyTxCert{} -> Nothing
        Ledger.ResignCommitteeColdTxCert{} -> Nothing
        Ledger.RegDRepTxCert{} -> Nothing
        Ledger.UnRegDRepTxCert{} -> Nothing
        Ledger.UpdateDRepTxCert{} -> Nothing
        -- those are old shelley patterns
        Ledger.RegTxCert _ -> Nothing
        -- stake cred deregistration w/o deposit
        Ledger.UnRegTxCert cred -> Just cred

filterUnRegDRepCreds
  :: Certificate era -> Maybe (Ledger.Credential Ledger.DRepRole Ledger.StandardCrypto)
filterUnRegDRepCreds = \case
  ShelleyRelatedCertificate _ _ -> Nothing
  ConwayCertificate cEra conwayCert -> conwayEraOnwardsConstraints cEra $
    case conwayCert of
      Ledger.RegPoolTxCert _ -> Nothing
      Ledger.RetirePoolTxCert _ _ -> Nothing
      Ledger.RegDepositTxCert _ _ -> Nothing
      Ledger.UnRegDepositTxCert _ _ -> Nothing
      Ledger.DelegTxCert _ _ -> Nothing
      Ledger.RegDepositDelegTxCert{} -> Nothing
      Ledger.AuthCommitteeHotKeyTxCert{} -> Nothing
      Ledger.ResignCommitteeColdTxCert{} -> Nothing
      Ledger.RegDRepTxCert{} -> Nothing
      Ledger.UnRegDRepTxCert cred _ -> Just cred
      Ledger.UpdateDRepTxCert{} -> Nothing
      -- those are old shelley patterns
      Ledger.RegTxCert _ -> Nothing
      -- stake cred deregistration w/o deposit
      Ledger.UnRegTxCert _ -> Nothing

-- ----------------------------------------------------------------------------
-- Internal conversion functions
--

toShelleyCertificate
  :: ()
  => Certificate era
  -> Ledger.TxCert (ShelleyLedgerEra era)
toShelleyCertificate = \case
  ShelleyRelatedCertificate w c ->
    shelleyToBabbageEraConstraints w c
  ConwayCertificate w c ->
    conwayEraOnwardsConstraints w c

fromShelleyCertificate
  :: ()
  => ShelleyBasedEra era
  -> Ledger.TxCert (ShelleyLedgerEra era)
  -> Certificate era
fromShelleyCertificate =
  caseShelleyToBabbageOrConwayEraOnwards ShelleyRelatedCertificate ConwayCertificate

toShelleyPoolParams :: StakePoolParameters -> Ledger.PoolParams StandardCrypto
toShelleyPoolParams
  StakePoolParameters
    { stakePoolId = StakePoolKeyHash poolkh
    , stakePoolVRF = VrfKeyHash vrfkh
    , stakePoolCost
    , stakePoolMargin
    , stakePoolRewardAccount
    , stakePoolPledge
    , stakePoolOwners
    , stakePoolRelays
    , stakePoolMetadata
    } =
    -- TODO: validate pool parameters such as the PoolMargin below, but also
    -- do simple client-side sanity checks, e.g. on the pool metadata url
    Ledger.PoolParams
      { Ledger.ppId = poolkh
      , Ledger.ppVrf = Ledger.toVRFVerKeyHash vrfkh
      , Ledger.ppPledge = stakePoolPledge
      , Ledger.ppCost = stakePoolCost
      , Ledger.ppMargin =
          fromMaybe
            (error "toShelleyPoolParams: invalid PoolMargin")
            (Ledger.boundRational stakePoolMargin)
      , Ledger.ppRewardAccount = toShelleyStakeAddr stakePoolRewardAccount
      , Ledger.ppOwners =
          fromList
            [kh | StakeKeyHash kh <- stakePoolOwners]
      , Ledger.ppRelays =
          fromList
            (map toShelleyStakePoolRelay stakePoolRelays)
      , Ledger.ppMetadata =
          toShelleyPoolMetadata
            <$> Ledger.maybeToStrictMaybe stakePoolMetadata
      }
   where
    toShelleyStakePoolRelay :: StakePoolRelay -> Ledger.StakePoolRelay
    toShelleyStakePoolRelay (StakePoolRelayIp mipv4 mipv6 mport) =
      Ledger.SingleHostAddr
        (fromIntegral <$> Ledger.maybeToStrictMaybe mport)
        (Ledger.maybeToStrictMaybe mipv4)
        (Ledger.maybeToStrictMaybe mipv6)
    toShelleyStakePoolRelay (StakePoolRelayDnsARecord dnsname mport) =
      Ledger.SingleHostName
        (fromIntegral <$> Ledger.maybeToStrictMaybe mport)
        (toShelleyDnsName dnsname)
    toShelleyStakePoolRelay (StakePoolRelayDnsSrvRecord dnsname) =
      Ledger.MultiHostName
        (toShelleyDnsName dnsname)

    toShelleyPoolMetadata :: StakePoolMetadataReference -> Ledger.PoolMetadata
    toShelleyPoolMetadata
      StakePoolMetadataReference
        { stakePoolMetadataURL
        , stakePoolMetadataHash = StakePoolMetadataHash mdh
        } =
        Ledger.PoolMetadata
          { Ledger.pmUrl = toShelleyUrl stakePoolMetadataURL
          , Ledger.pmHash = Ledger.hashToBytes mdh
          }

    toShelleyDnsName :: ByteString -> Ledger.DnsName
    toShelleyDnsName name =
      fromMaybe (error "toShelleyDnsName: invalid dns name. TODO: proper validation")
        . Ledger.textToDns (BS.length name)
        $ Text.decodeLatin1 name

    toShelleyUrl :: Text -> Ledger.Url
    toShelleyUrl url =
      fromMaybe (error "toShelleyUrl: invalid url. TODO: proper validation") $
        Ledger.textToUrl (Text.length url) url

fromShelleyPoolParams
  :: Ledger.PoolParams StandardCrypto
  -> StakePoolParameters
fromShelleyPoolParams
  Ledger.PoolParams
    { Ledger.ppId
    , Ledger.ppVrf
    , Ledger.ppPledge
    , Ledger.ppCost
    , Ledger.ppMargin
    , Ledger.ppRewardAccount
    , Ledger.ppOwners
    , Ledger.ppRelays
    , Ledger.ppMetadata
    } =
    StakePoolParameters
      { stakePoolId = StakePoolKeyHash ppId
      , stakePoolVRF = VrfKeyHash (Ledger.fromVRFVerKeyHash ppVrf)
      , stakePoolCost = ppCost
      , stakePoolMargin = Ledger.unboundRational ppMargin
      , stakePoolRewardAccount = fromShelleyStakeAddr ppRewardAccount
      , stakePoolPledge = ppPledge
      , stakePoolOwners = map StakeKeyHash (toList ppOwners)
      , stakePoolRelays =
          map
            fromShelleyStakePoolRelay
            (toList ppRelays)
      , stakePoolMetadata =
          fromShelleyPoolMetadata
            <$> Ledger.strictMaybeToMaybe ppMetadata
      }
   where
    fromShelleyStakePoolRelay :: Ledger.StakePoolRelay -> StakePoolRelay
    fromShelleyStakePoolRelay (Ledger.SingleHostAddr mport mipv4 mipv6) =
      StakePoolRelayIp
        (Ledger.strictMaybeToMaybe mipv4)
        (Ledger.strictMaybeToMaybe mipv6)
        (fromIntegral . Ledger.portToWord16 <$> Ledger.strictMaybeToMaybe mport)
    fromShelleyStakePoolRelay (Ledger.SingleHostName mport dnsname) =
      StakePoolRelayDnsARecord
        (fromShelleyDnsName dnsname)
        (fromIntegral . Ledger.portToWord16 <$> Ledger.strictMaybeToMaybe mport)
    fromShelleyStakePoolRelay (Ledger.MultiHostName dnsname) =
      StakePoolRelayDnsSrvRecord
        (fromShelleyDnsName dnsname)

    fromShelleyPoolMetadata :: Ledger.PoolMetadata -> StakePoolMetadataReference
    fromShelleyPoolMetadata
      Ledger.PoolMetadata
        { Ledger.pmUrl
        , Ledger.pmHash
        } =
        StakePoolMetadataReference
          { stakePoolMetadataURL = Ledger.urlToText pmUrl
          , stakePoolMetadataHash =
              StakePoolMetadataHash
                . fromMaybe (error "fromShelleyPoolMetadata: invalid hash. TODO: proper validation")
                . Ledger.hashFromBytes
                $ pmHash
          }

    -- TODO: change the ledger rep of the DNS name to use ShortByteString
    fromShelleyDnsName :: Ledger.DnsName -> ByteString
    fromShelleyDnsName =
      Text.encodeUtf8
        . Ledger.dnsToText

data AnchorDataFromCertificateError
  = InvalidPoolMetadataHashError Ledger.Url ByteString
  deriving (Eq, Show)

instance Error AnchorDataFromCertificateError where
  prettyError :: AnchorDataFromCertificateError -> Doc ann
  prettyError (InvalidPoolMetadataHashError url hash) =
    "Invalid pool metadata hash for URL " <> fromString (show url) <> ": " <> fromString (show hash)

-- | Get anchor data url and hash from a certificate. A return value of `Nothing`
-- means that the certificate does not contain anchor data.
getAnchorDataFromCertificate
  :: Certificate era
  -> Either AnchorDataFromCertificateError (Maybe (Ledger.Anchor StandardCrypto))
getAnchorDataFromCertificate c =
  case c of
    ShelleyRelatedCertificate stbe scert ->
      shelleyToBabbageEraConstraints stbe $
        case scert of
          Ledger.RegTxCert _ -> return Nothing
          Ledger.UnRegTxCert _ -> return Nothing
          Ledger.DelegStakeTxCert _ _ -> return Nothing
          Ledger.RegPoolTxCert poolParams -> strictMaybe (return Nothing) anchorDataFromPoolMetadata $ Ledger.ppMetadata poolParams
          Ledger.RetirePoolTxCert _ _ -> return Nothing
          Ledger.GenesisDelegTxCert{} -> return Nothing
          Ledger.MirTxCert _ -> return Nothing
    ConwayCertificate ceo ccert ->
      conwayEraOnwardsConstraints ceo $
        case ccert of
          Ledger.RegTxCert _ -> return Nothing
          Ledger.UnRegTxCert _ -> return Nothing
          Ledger.RegDepositTxCert _ _ -> return Nothing
          Ledger.UnRegDepositTxCert _ _ -> return Nothing
          Ledger.RegDepositDelegTxCert{} -> return Nothing
          Ledger.DelegTxCert{} -> return Nothing
          Ledger.RegPoolTxCert poolParams -> strictMaybe (return Nothing) anchorDataFromPoolMetadata $ Ledger.ppMetadata poolParams
          Ledger.RetirePoolTxCert _ _ -> return Nothing
          Ledger.RegDRepTxCert _ _ mAnchor -> return $ Ledger.strictMaybeToMaybe mAnchor
          Ledger.UnRegDRepTxCert _ _ -> return Nothing
          Ledger.UpdateDRepTxCert _ mAnchor -> return $ Ledger.strictMaybeToMaybe mAnchor
          Ledger.AuthCommitteeHotKeyTxCert _ _ -> return Nothing
          Ledger.ResignCommitteeColdTxCert _ mAnchor -> return $ Ledger.strictMaybeToMaybe mAnchor
 where
  anchorDataFromPoolMetadata
    :: MonadError AnchorDataFromCertificateError m
    => Ledger.PoolMetadata
    -> m (Maybe (Ledger.Anchor StandardCrypto))
  anchorDataFromPoolMetadata (Ledger.PoolMetadata{Ledger.pmUrl = url, Ledger.pmHash = hashBytes}) = do
    hash <-
      maybe (throwError $ InvalidPoolMetadataHashError url hashBytes) return $
        Ledger.hashFromBytes hashBytes
    return $
      Just
        ( Ledger.Anchor
            { Ledger.anchorUrl = url
            , Ledger.anchorDataHash = Ledger.unsafeMakeSafeHash hash
            }
        )

-- | Returns `True` if the certificate is a DRep registration or update certificate,
-- otherwise `False`. This is to see if the certificate needs to be compliant with
-- CIP-0119.
isDRepRegOrUpdateCert :: Certificate era -> Bool
isDRepRegOrUpdateCert = \case
  ShelleyRelatedCertificate _ _ -> False
  ConwayCertificate ceo ccert ->
    conwayEraOnwardsConstraints ceo $
      case ccert of
        Ledger.RegDRepTxCert{} -> True
        Ledger.UpdateDRepTxCert{} -> True
        _ -> False
