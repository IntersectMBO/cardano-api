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
{-# LANGUAGE TypeOperators #-}

-- | Certificates embedded in transactions
module Cardano.Api.Certificate.Internal
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
  , fromShelleyStakePoolState

    -- * Data family instances
  , AsType (..)

    -- * Internal functions
  , filterUnRegCreds
  , filterUnRegDRepCreds
  , isDRepRegOrUpdateCert
  )
where

import Cardano.Api.Address
import Cardano.Api.Certificate.Internal.DRepMetadata
import Cardano.Api.Certificate.Internal.StakePoolMetadata
import Cardano.Api.Era
import Cardano.Api.Error (Error (..))
import Cardano.Api.Experimental.Tx.Internal.Certificate qualified as Exp
import Cardano.Api.Experimental.Tx.Internal.Certificate.Compatible (getTxCertWitness)
import Cardano.Api.Governance.Internal.Action.VotingProcedure
import Cardano.Api.HasTypeProxy
import Cardano.Api.Internal.Utils (noInlineMaybeToStrictMaybe)
import Cardano.Api.Key.Internal
import Cardano.Api.Key.Internal.Praos
import Cardano.Api.Ledger.Internal.Reexport qualified as Ledger
import Cardano.Api.Pretty (Doc)
import Cardano.Api.Serialise.Cbor
import Cardano.Api.Serialise.TextEnvelope.Internal
import Cardano.Api.Value.Internal

import Cardano.Ledger.BaseTypes (strictMaybe)
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.Keys qualified as Ledger
import Cardano.Ledger.State qualified as Ledger

import Control.Monad
import Control.Monad.Except (MonadError (..))
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.IP (IPv4, IPv6)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Type.Equality (TestEquality (..))
import Data.Typeable
import GHC.Exts (IsList (..), fromString)
import Network.Socket (PortNumber)

-- ----------------------------------------------------------------------------
-- Certificates embedded in transactions
--

{-# DEPRECATED Certificate "Use `Certificate` type from Cardano.Api.Experimental.Tx.Internal.Certificate instead" #-}

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
    :: Typeable era
    => ShelleyToBabbageEra era
    -> Ledger.ShelleyTxCert (ShelleyLedgerEra era)
    -> Certificate era
  -- Conway onwards
  -- TODO: Add comments about the new types of certificates
  ConwayCertificate
    :: Typeable era
    => ConwayEraOnwards era
    -> Ledger.ConwayTxCert (ShelleyLedgerEra era)
    -> Certificate era
  deriving anyclass SerialiseAsCBOR

deriving instance Eq (Certificate era)

deriving instance Ord (Certificate era)

deriving instance Show (Certificate era)

instance TestEquality Certificate where
  testEquality (ShelleyRelatedCertificate _ c) (ShelleyRelatedCertificate _ c') =
    shelleyCertTypeEquality c c'
  testEquality (ConwayCertificate _ c) (ConwayCertificate _ c') =
    conwayCertTypeEquality c c'
  testEquality ShelleyRelatedCertificate{} ConwayCertificate{} = Nothing
  testEquality ConwayCertificate{} ShelleyRelatedCertificate{} = Nothing

conwayCertTypeEquality
  :: (Typeable eraA, Typeable eraB)
  => Ledger.ConwayTxCert (ShelleyLedgerEra eraA)
  -> Ledger.ConwayTxCert (ShelleyLedgerEra eraB)
  -> Maybe (eraA :~: eraB)
conwayCertTypeEquality _ _ = eqT

shelleyCertTypeEquality
  :: (Typeable eraA, Typeable eraB)
  => Ledger.ShelleyTxCert (ShelleyLedgerEra eraA)
  -> Ledger.ShelleyTxCert (ShelleyLedgerEra eraB)
  -> Maybe (eraA :~: eraB)
shelleyCertTypeEquality _ _ = eqT

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

certificateToTxCert :: Certificate era -> L.TxCert (ShelleyLedgerEra era)
certificateToTxCert c =
  case c of
    ShelleyRelatedCertificate eon cert ->
      case eon of
        ShelleyToBabbageEraShelley -> cert
        ShelleyToBabbageEraAllegra -> cert
        ShelleyToBabbageEraMary -> cert
        ShelleyToBabbageEraAlonzo -> cert
        ShelleyToBabbageEraBabbage -> cert
    ConwayCertificate eon cert ->
      case eon of
        ConwayEraOnwardsConway -> cert
        ConwayEraOnwardsDijkstra -> error "certificateToTxCert: Dijkstra era is not yet supported"

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
-- TODO: Remove me, its not in use anywhere
data DRepMetadataReference
  = DRepMetadataReference
  { drepMetadataURL :: Text
  , drepMetadataHash :: Hash DRepMetadata
  }
  deriving (Eq, Show)

-- ----------------------------------------------------------------------------
-- Constructor functions
--

{-# DEPRECATED
  StakeAddressRequirements
  "Use Cardano.Api.Experimental's makeStakeAddressRegistrationCertificate instead"
  #-}

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

{-# DEPRECATED
  makeStakeAddressRegistrationCertificate
  "If you need compatibility with all shelley based eras use Cardano.Api.Compatible.Certificate's \
   \ makeStakeAddressRegistrationCertificate. Otherwise use Cardano.Api.Experimental.Certificate's makeStakeAddressRegistrationCertificate"
  #-}
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

{-# DEPRECATED
  makeStakeAddressUnregistrationCertificate
  "If you need compatibility with all shelley based eras use Cardano.Api.Compatible.Certificate's \
   \ makeStakeAddressUnregistrationCertificate. Otherwise use Cardano.Api.Experimental.Certificate's makeStakeAddressUnregistrationCertificate"
  #-}
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

{-# DEPRECATED
  StakeDelegationRequirements
  "If you need compatibility with all shelley based eras use Cardano.Api.Compatible.Certificate's \
   \ makeStakeAddressDelegationCertificate. Otherwise use Cardano.Api.Experimental.Certificate's makeStakeAddressDelegationCertificate"
  #-}

data StakeDelegationRequirements era where
  StakeDelegationRequirementsConwayOnwards
    :: ConwayEraOnwards era
    -> StakeCredential
    -> Ledger.Delegatee
    -> StakeDelegationRequirements era
  StakeDelegationRequirementsPreConway
    :: ShelleyToBabbageEra era
    -> StakeCredential
    -> PoolId
    -> StakeDelegationRequirements era

{-# DEPRECATED
  makeStakeAddressDelegationCertificate
  "If you need compatibility with all shelley based eras use Cardano.Api.Compatible.Certificate's \
   \ makeStakeAddressDelegationCertificate. Otherwise use Cardano.Api.Experimental.Certificate's makeStakeAddressDelegationCertificate"
  #-}
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
    -> Ledger.PoolParams
    -> StakePoolRegistrationRequirements era
  StakePoolRegistrationRequirementsPreConway
    :: ShelleyToBabbageEra era
    -> Ledger.PoolParams
    -> StakePoolRegistrationRequirements era

{-# DEPRECATED
  makeStakePoolRegistrationCertificate
  "If you need compatibility with all shelley based eras use Cardano.Api.Compatible.Certificate's \
   \ makeStakePoolRegistrationCertificate. Otherwise use Cardano.Api.Experimental.Certificate's makeStakePoolRegistrationCertificate"
  #-}
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

{-# DEPRECATED
  StakePoolRetirementRequirements
  "If you need compatibility with all shelley based eras use Cardano.Api.Compatible.Certificate's \
   \ makeStakePoolRetirementCertificate. Otherwise use Cardano.Api.Experimental.Certificate's makeStakePoolRetirementCertificate"
  #-}

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

{-# DEPRECATED
  makeStakePoolRetirementCertificate
  "If you need compatibility with all shelley based eras use Cardano.Api.Compatible.Certificate's \
   \ makeStakePoolRetirementCertificate. Otherwise use Cardano.Api.Experimental.Certificate's makeStakePoolRetirementCertificate"
  #-}
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

{-# DEPRECATED
  GenesisKeyDelegationRequirements
  "Use Cardano.Api.Compatible.Certificate's makeGenesisKeyDelegationCertificate instead"
  #-}

data GenesisKeyDelegationRequirements era where
  GenesisKeyDelegationRequirements
    :: ShelleyToBabbageEra era
    -> Hash GenesisKey
    -> Hash GenesisDelegateKey
    -> Hash VrfKey
    -> GenesisKeyDelegationRequirements era

{-# DEPRECATED
  makeGenesisKeyDelegationCertificate
  "Use Cardano.Api.Compatible.Certificate's makeGenesisKeyDelegationCertificate instead"
  #-}
makeGenesisKeyDelegationCertificate
  :: Typeable era => GenesisKeyDelegationRequirements era -> Certificate era
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

{-# DEPRECATED
  MirCertificateRequirements
  "Use Cardano.Api.Compatible.Certificate's makeMIRCertificate instead"
  #-}

data MirCertificateRequirements era where
  MirCertificateRequirements
    :: ShelleyToBabbageEra era
    -> Ledger.MIRPot
    -> Ledger.MIRTarget
    -> MirCertificateRequirements era

{-# DEPRECATED
  makeMIRCertificate
  "Use Cardano.Api.Compatible.Certificate's makeMIRCertificate instead"
  #-}
makeMIRCertificate
  :: Typeable era
  => MirCertificateRequirements era
  -> Certificate era
makeMIRCertificate (MirCertificateRequirements atMostEra mirPot mirTarget) =
  ShelleyRelatedCertificate atMostEra $
    Ledger.ShelleyTxCertMir $
      Ledger.MIRCert mirPot mirTarget

{-# DEPRECATED
  DRepRegistrationRequirements
  "Use Cardano.Api.Experimental.Certificate's makeDrepRegistrationCertificate instead"
  #-}

data DRepRegistrationRequirements era where
  DRepRegistrationRequirements
    :: ConwayEraOnwards era
    -> (Ledger.Credential Ledger.DRepRole)
    -> L.Coin
    -> DRepRegistrationRequirements era

{-# DEPRECATED
  makeDrepRegistrationCertificate
  "Use Cardano.Api.Experimental.Certificate's makeDrepRegistrationCertificate instead"
  #-}
makeDrepRegistrationCertificate
  :: Typeable era
  => DRepRegistrationRequirements era
  -> Maybe Ledger.Anchor
  -> Certificate era
makeDrepRegistrationCertificate (DRepRegistrationRequirements conwayOnwards vcred deposit) anchor =
  ConwayCertificate conwayOnwards
    . Ledger.ConwayTxCertGov
    $ Ledger.ConwayRegDRep vcred deposit (noInlineMaybeToStrictMaybe anchor)

data CommitteeHotKeyAuthorizationRequirements era where
  CommitteeHotKeyAuthorizationRequirements
    :: ConwayEraOnwards era
    -> Ledger.Credential Ledger.ColdCommitteeRole
    -> Ledger.Credential Ledger.HotCommitteeRole
    -> CommitteeHotKeyAuthorizationRequirements era

{-# DEPRECATED
  makeCommitteeHotKeyAuthorizationCertificate
  "Use Cardano.Api.Experimental.Certificate's makeCommitteeHotKeyAuthorizationCertificate instead"
  #-}
makeCommitteeHotKeyAuthorizationCertificate
  :: Typeable era
  => CommitteeHotKeyAuthorizationRequirements era
  -> Certificate era
makeCommitteeHotKeyAuthorizationCertificate (CommitteeHotKeyAuthorizationRequirements cOnwards coldKeyCredential hotKeyCredential) =
  ConwayCertificate cOnwards
    . Ledger.ConwayTxCertGov
    $ Ledger.ConwayAuthCommitteeHotKey coldKeyCredential hotKeyCredential

{-# DEPRECATED
  CommitteeColdkeyResignationRequirements
  "Use Cardano.Api.Experimental.Certificate's makeCommitteeColdkeyResignationCertificate instead"
  #-}

data CommitteeColdkeyResignationRequirements era where
  CommitteeColdkeyResignationRequirements
    :: ConwayEraOnwards era
    -> Ledger.Credential Ledger.ColdCommitteeRole
    -> Maybe Ledger.Anchor
    -> CommitteeColdkeyResignationRequirements era

{-# DEPRECATED
  makeCommitteeColdkeyResignationCertificate
  "Use Cardano.Api.Experimental.Certificate's makeCommitteeColdkeyResignationCertificate instead"
  #-}
makeCommitteeColdkeyResignationCertificate
  :: Typeable era
  => CommitteeColdkeyResignationRequirements era
  -> Certificate era
makeCommitteeColdkeyResignationCertificate (CommitteeColdkeyResignationRequirements cOnwards coldKeyCred anchor) =
  ConwayCertificate cOnwards
    . Ledger.ConwayTxCertGov
    $ Ledger.ConwayResignCommitteeColdKey
      coldKeyCred
      (noInlineMaybeToStrictMaybe anchor)

{-# DEPRECATED
  DRepUnregistrationRequirements
  "Use Cardano.Api.Experimental.Certificate's makeDrepUnregistrationCertificate instead"
  #-}

data DRepUnregistrationRequirements era where
  DRepUnregistrationRequirements
    :: ConwayEraOnwards era
    -> (Ledger.Credential Ledger.DRepRole)
    -> L.Coin
    -> DRepUnregistrationRequirements era

{-# DEPRECATED
  makeDrepUnregistrationCertificate
  "Use Cardano.Api.Experimental.Certificate's makeDrepUnregistrationCertificate instead"
  #-}
makeDrepUnregistrationCertificate
  :: Typeable era
  => DRepUnregistrationRequirements era
  -> Certificate era
makeDrepUnregistrationCertificate (DRepUnregistrationRequirements conwayOnwards vcred deposit) =
  ConwayCertificate conwayOnwards
    . Ledger.ConwayTxCertGov
    $ Ledger.ConwayUnRegDRep vcred deposit

{-# DEPRECATED
  makeStakeAddressAndDRepDelegationCertificate
  "Use Cardano.Api.Experimental.Certificate's makeStakeAddressAndDRepDelegationCertificate instead"
  #-}
makeStakeAddressAndDRepDelegationCertificate
  :: ()
  => ConwayEraOnwards era
  -> StakeCredential
  -> Ledger.Delegatee
  -> L.Coin
  -> Certificate era
makeStakeAddressAndDRepDelegationCertificate w cred delegatee deposit =
  conwayEraOnwardsConstraints w $
    ConwayCertificate w $
      Ledger.mkRegDepositDelegTxCert (toShelleyStakeCredential cred) delegatee deposit

{-# DEPRECATED
  DRepUpdateRequirements
  "Use Cardano.Api.Experimental.Certificate's makeDrepUpdateCertificate instead"
  #-}

data DRepUpdateRequirements era where
  DRepUpdateRequirements
    :: ConwayEraOnwards era
    -> Ledger.Credential Ledger.DRepRole
    -> DRepUpdateRequirements era

{-# DEPRECATED
  makeDrepUpdateCertificate
  "Use Cardano.Api.Experimental.Certificate's makeDrepUpdateCertificate instead"
  #-}
makeDrepUpdateCertificate
  :: Typeable era
  => DRepUpdateRequirements era
  -> Maybe Ledger.Anchor
  -> Certificate era
makeDrepUpdateCertificate (DRepUpdateRequirements conwayOnwards vcred) mAnchor =
  ConwayCertificate conwayOnwards
    . Ledger.ConwayTxCertGov
    $ Ledger.ConwayUpdateDRep vcred (noInlineMaybeToStrictMaybe mAnchor)

-- ----------------------------------------------------------------------------
-- Helper functions
--

-- | Get the stake credential witness for a certificate that requires it.
-- Only stake address deregistration and delegation requires witnessing (witness can be script or key).
{-# DEPRECATED
  selectStakeCredentialWitness
  "Use Cardano.Api.Certificate's selectStakeCredentialWitness instead"
  #-}
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
  :: ShelleyBasedEra era -> Exp.Certificate (ShelleyLedgerEra era) -> Maybe StakeCredential
filterUnRegCreds sbe (Exp.Certificate cert) =
  fmap fromShelleyStakeCredential $
    shelleyBasedEraConstraints sbe $
      Ledger.lookupUnRegStakeTxCert cert

filterUnRegDRepCreds
  :: ShelleyBasedEra era
  -> Exp.Certificate (ShelleyLedgerEra era)
  -> Maybe (Ledger.Credential Ledger.DRepRole)
filterUnRegDRepCreds sbe (Exp.Certificate cert) =
  join $ forEraInEonMaybe (toCardanoEra sbe) $ \w ->
    conwayEraOnwardsConstraints w $
      fst
        <$> Ledger.getUnRegDRepTxCert cert

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

toShelleyPoolParams :: StakePoolParameters -> Ledger.PoolParams
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
  :: Ledger.PoolParams
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

fromShelleyStakePoolState
  :: Ledger.KeyHash Ledger.StakePool
  -> Ledger.StakePoolState
  -> StakePoolParameters
fromShelleyStakePoolState
  poolId
  Ledger.StakePoolState
    { Ledger.spsVrf
    , Ledger.spsPledge
    , Ledger.spsCost
    , Ledger.spsMargin
    , Ledger.spsRewardAccount
    , Ledger.spsOwners
    , Ledger.spsRelays
    , Ledger.spsMetadata
    } =
    StakePoolParameters
      { stakePoolId = StakePoolKeyHash poolId
      , stakePoolVRF = VrfKeyHash (Ledger.fromVRFVerKeyHash spsVrf)
      , stakePoolCost = spsCost
      , stakePoolMargin = Ledger.unboundRational spsMargin
      , stakePoolRewardAccount = fromShelleyStakeAddr spsRewardAccount
      , stakePoolPledge = spsPledge
      , stakePoolOwners = map StakeKeyHash (toList spsOwners)
      , stakePoolRelays =
          map
            fromShelleyStakePoolRelay
            (toList spsRelays)
      , stakePoolMetadata =
          fromShelleyPoolMetadata
            <$> Ledger.strictMaybeToMaybe spsMetadata
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
  -> Either AnchorDataFromCertificateError (Maybe Ledger.Anchor)
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
          _ -> error "dijkstra"
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
          _ -> error "dijkstra"
 where
  anchorDataFromPoolMetadata
    :: MonadError AnchorDataFromCertificateError m
    => Ledger.PoolMetadata
    -> m (Maybe Ledger.Anchor)
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
