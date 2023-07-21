{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Certificates embedded in transactions
--
module Cardano.Api.Certificate (
    Certificate(..),

    -- * Registering stake address and delegating
    StakeAddressRequirements(..),
    StakeDelegationRequirements(..),
    makeStakeAddressDelegationCertificate,
    makeStakeAddressRegistrationCertificate,
    makeStakeAddressUnregistrationCertificate,
    makeStakeAddressPoolDelegationCertificate,
    PoolId,

    -- * Registering stake pools
    StakePoolRegistrationRequirements(..),
    StakePoolRetirementRequirements(..),
    makeStakePoolRegistrationCertificate,
    makeStakePoolRetirementCertificate,
    StakePoolParameters(..),
    StakePoolRelay(..),
    StakePoolMetadataReference(..),

    -- * Conway specific certificates
    CommitteeColdkeyResignationRequirements(..),
    CommitteeHotKeyAuthorizationRequirements(..),
    DRepRegistrationRequirements(..),
    DRepUnregistrationRequirements(..),
    makeCommitteeColdkeyResignationCertificate,
    makeCommitteeHotKeyAuthorizationCertificate,
    makeDrepRegistrationCertificate,
    makeDrepUnregistrationCertificate,

    -- * Registering DReps
    DRepMetadataReference(..),

    -- * Special certificates
    GenesisKeyDelegationRequirements(..),
    MirCertificateRequirements(..),
    makeMIRCertificate,
    makeGenesisKeyDelegationCertificate,
    Ledger.MIRTarget (..),
    Ledger.MIRPot(..),

    -- * Internal conversion functions
    toShelleyCertificate,
    fromShelleyCertificate,
    toShelleyPoolParams,
    fromShelleyPoolParams,

    -- * Data family instances
    AsType(..),

    -- * GADTs for Conway/Shelley differences
    ShelleyToBabbageEra(..),
    ConwayEraOnwards(..),

    -- * Internal functions
    filterUnRegCreds,
    selectStakeCredential,
  ) where

import           Cardano.Api.Address
import           Cardano.Api.DRepMetadata
import           Cardano.Api.EraCast
import           Cardano.Api.Eras
import           Cardano.Api.Feature
import           Cardano.Api.Governance.Actions.VotingProcedure
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Keys.Praos
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.ReexposeLedger (EraCrypto, StandardCrypto)
import qualified Cardano.Api.ReexposeLedger as Ledger
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.StakePoolMetadata
import           Cardano.Api.Utils
import           Cardano.Api.Value

import           Data.ByteString (ByteString)
import qualified Data.Foldable as Foldable
import           Data.IP (IPv4, IPv6)
import           Data.Maybe
import qualified Data.Sequence.Strict as Seq
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text.Encoding as Text
import           Data.Typeable
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
  forall era.
  ( IsShelleyBasedEra era
  ) => ToCBOR (Certificate era) where
    toCBOR =
      -- TODO CIP-1694 clean this up
      case shelleyBasedEra @era of
        ShelleyBasedEraShelley  ->
          Ledger.toEraCBOR @(ShelleyLedgerEra era) . toShelleyCertificate shelleyBasedEra
        ShelleyBasedEraAllegra  ->
          Ledger.toEraCBOR @(ShelleyLedgerEra era) . toShelleyCertificate shelleyBasedEra
        ShelleyBasedEraMary     ->
          Ledger.toEraCBOR @(ShelleyLedgerEra era) . toShelleyCertificate shelleyBasedEra
        ShelleyBasedEraAlonzo   ->
          Ledger.toEraCBOR @(ShelleyLedgerEra era) . toShelleyCertificate shelleyBasedEra
        ShelleyBasedEraBabbage  ->
          Ledger.toEraCBOR @(ShelleyLedgerEra era) . toShelleyCertificate shelleyBasedEra
        ShelleyBasedEraConway   ->
          Ledger.toEraCBOR @(ShelleyLedgerEra era) . toShelleyCertificate shelleyBasedEra



instance
  ( IsShelleyBasedEra era
  ) => FromCBOR (Certificate era) where
    fromCBOR =
      case shelleyBasedEra @era of
        ShelleyBasedEraShelley  ->
          fromShelleyCertificate shelleyBasedEra <$> Ledger.fromEraCBOR @(ShelleyLedgerEra era)
        ShelleyBasedEraAllegra  ->
          fromShelleyCertificate shelleyBasedEra <$> Ledger.fromEraCBOR @(ShelleyLedgerEra era)
        ShelleyBasedEraMary     ->
          fromShelleyCertificate shelleyBasedEra <$> Ledger.fromEraCBOR @(ShelleyLedgerEra era)
        ShelleyBasedEraAlonzo   ->
          fromShelleyCertificate shelleyBasedEra <$> Ledger.fromEraCBOR @(ShelleyLedgerEra era)
        ShelleyBasedEraBabbage  ->
          fromShelleyCertificate shelleyBasedEra <$> Ledger.fromEraCBOR @(ShelleyLedgerEra era)
        ShelleyBasedEraConway   ->
          fromShelleyCertificate shelleyBasedEra <$> Ledger.fromEraCBOR @(ShelleyLedgerEra era)


instance
  ( IsShelleyBasedEra era
  ) => HasTextEnvelope (Certificate era) where
    textEnvelopeType _ = "CertificateShelley"
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
      ConwayCertificate _ (Ledger.ConwayTxCertCommittee Ledger.ConwayRegDRep{}) -> "Constitution committee member key registration"
      ConwayCertificate _ (Ledger.ConwayTxCertCommittee Ledger.ConwayUnRegDRep{}) -> "Constitution committee member key unregistration"
      ConwayCertificate _ (Ledger.ConwayTxCertCommittee Ledger.ConwayAuthCommitteeHotKey{}) -> "Constitution committee member hot key registration"
      ConwayCertificate _ (Ledger.ConwayTxCertCommittee Ledger.ConwayResignCommitteeColdKey{}) -> "Constitution committee member hot key resignation"

      ConwayCertificate _ (Ledger.ConwayTxCertDeleg Ledger.ConwayRegCert{}) -> "Stake address registration"
      ConwayCertificate _ (Ledger.ConwayTxCertDeleg Ledger.ConwayUnRegCert{}) -> "Stake address deregistration"
      ConwayCertificate _ (Ledger.ConwayTxCertDeleg Ledger.ConwayDelegCert{}) ->  "Stake address delegation"
      ConwayCertificate _ (Ledger.ConwayTxCertDeleg Ledger.ConwayRegDelegCert{}) -> "Stake address registration and delegation"
      ConwayCertificate _ (Ledger.ConwayTxCertPool Ledger.RegPool{}) -> "Pool registration"
      ConwayCertificate _ (Ledger.ConwayTxCertPool Ledger.RetirePool{}) -> "Pool retirement"



instance EraCast Certificate where
  eraCast toEra cert =
    case cert  of
      ShelleyRelatedCertificate _ (Ledger.ShelleyTxCertDelegCert Ledger.ShelleyRegCert{}) ->
        eraCast toEra cert
      ShelleyRelatedCertificate _ (Ledger.ShelleyTxCertDelegCert Ledger.ShelleyUnRegCert{}) ->
        eraCast toEra cert
      ShelleyRelatedCertificate _ (Ledger.ShelleyTxCertDelegCert Ledger.ShelleyDelegCert{}) ->
        eraCast toEra cert
      ShelleyRelatedCertificate _ (Ledger.ShelleyTxCertPool Ledger.RetirePool{}) ->
        eraCast toEra cert
      ShelleyRelatedCertificate _ (Ledger.ShelleyTxCertPool Ledger.RegPool{}) ->
        eraCast toEra cert

      -- We cannot cast MIR and GenDeleg certs from Babbage to Conway era because they do not exist
      ShelleyRelatedCertificate (_ :: ShelleyToBabbageEra fromEra) Ledger.ShelleyTxCertGenesisDeleg{} ->
        case toEra of
          ConwayEra -> Left $ EraCastError
                                { originalValue = cert
                                , fromEra = cardanoEra @fromEra
                                , toEra = toEra
                                }
          BabbageEra -> eraCast toEra cert
          AlonzoEra -> eraCast toEra cert
          AllegraEra -> eraCast toEra cert
          MaryEra ->  eraCast toEra cert
          ShelleyEra ->  eraCast toEra cert
          ByronEra ->  error "TODO: EraCast Certififcate - Byron era"
            -- TODO: We need to modify the EraCast class to only allow casting to a future era.
            -- I can't imagine a use case where we would want to cast to a previous era

      ShelleyRelatedCertificate (_ :: ShelleyToBabbageEra fromEra) Ledger.ShelleyTxCertMir{} ->
        case toEra of
          ConwayEra -> Left $ EraCastError
                                { originalValue = cert
                                , fromEra = cardanoEra @fromEra
                                , toEra = toEra
                                }
          BabbageEra -> eraCast toEra cert
          AlonzoEra -> eraCast toEra cert
          AllegraEra -> eraCast toEra cert
          MaryEra ->  eraCast toEra cert
          ShelleyEra ->  eraCast toEra cert
          ByronEra ->  error "TODO: EraCast Certififcate - Byron era"
            -- TODO: We need to modify the EraCast class to only allow casting to a future era.
            -- I can't imagine a use case where we would want to cast to a previous era

      ConwayCertificate{} -> eraCast toEra cert

-- ----------------------------------------------------------------------------
-- Stake pool parameters
--

type PoolId = Hash StakePoolKey

data StakePoolParameters =
     StakePoolParameters {
       stakePoolId            :: PoolId,
       stakePoolVRF           :: Hash VrfKey,
       stakePoolCost          :: Lovelace,
       stakePoolMargin        :: Rational,
       stakePoolRewardAccount :: StakeAddress,
       stakePoolPledge        :: Lovelace,
       stakePoolOwners        :: [Hash StakeKey],
       stakePoolRelays        :: [StakePoolRelay],
       stakePoolMetadata      :: Maybe StakePoolMetadataReference
     }
  deriving (Eq, Show)

data StakePoolRelay =

       -- | One or both of IPv4 & IPv6
       StakePoolRelayIp
          (Maybe IPv4) (Maybe IPv6) (Maybe PortNumber)

       -- | An DNS name pointing to a @A@ or @AAAA@ record.
     | StakePoolRelayDnsARecord
          ByteString (Maybe PortNumber)

       -- | A DNS name pointing to a @SRV@ record.
     | StakePoolRelayDnsSrvRecord
          ByteString

  deriving (Eq, Show)

data StakePoolMetadataReference =
     StakePoolMetadataReference {
       stakePoolMetadataURL  :: Text,
       stakePoolMetadataHash :: Hash StakePoolMetadata
     }
  deriving (Eq, Show)

-- ----------------------------------------------------------------------------
-- DRep parameters
--

data DRepMetadataReference =
  DRepMetadataReference
  { drepMetadataURL  :: Text
  , drepMetadataHash :: Hash DRepMetadata
  }
  deriving (Eq, Show)


-- ----------------------------------------------------------------------------
-- Constructor functions
--

data ConwayEraOnwards era where
  ConwayEraOnwardsConway :: ConwayEraOnwards ConwayEra

deriving instance Show (ConwayEraOnwards era)
deriving instance Eq (ConwayEraOnwards era)

instance FeatureInEra ConwayEraOnwards where
  featureInEra no yes = \case
    ByronEra    -> no
    ShelleyEra  -> no
    AllegraEra  -> no
    MaryEra     -> no
    AlonzoEra   -> no
    BabbageEra  -> no
    ConwayEra   -> yes ConwayEraOnwardsConway

data ShelleyToBabbageEra era where
  ShelleyToBabbageEraShelley :: ShelleyToBabbageEra ShelleyEra
  ShelleyToBabbageEraAllegra :: ShelleyToBabbageEra AllegraEra
  ShelleyToBabbageEraMary :: ShelleyToBabbageEra MaryEra
  ShelleyToBabbageEraAlonzo :: ShelleyToBabbageEra AlonzoEra
  ShelleyToBabbageEraBabbage :: ShelleyToBabbageEra BabbageEra

deriving instance Show (ShelleyToBabbageEra era)
deriving instance Eq (ShelleyToBabbageEra era)

instance FeatureInEra ShelleyToBabbageEra where
  featureInEra no yes = \case
    ByronEra    -> no
    ShelleyEra  -> yes ShelleyToBabbageEraShelley
    AllegraEra  -> yes ShelleyToBabbageEraAllegra
    MaryEra     -> yes ShelleyToBabbageEraMary
    AlonzoEra   -> yes ShelleyToBabbageEraAlonzo
    BabbageEra  -> yes ShelleyToBabbageEraBabbage
    ConwayEra   -> no

data StakeAddressRequirements era where
  StakeAddrRegistrationConway
    :: ConwayEraOnwards era
    -> Lovelace
    -> StakeCredential
    -> StakeAddressRequirements era

  StakeAddrRegistrationPreConway
    :: ShelleyToBabbageEra era
    -> StakeCredential
    -> StakeAddressRequirements era


makeStakeAddressRegistrationCertificate :: StakeAddressRequirements era -> Certificate era
makeStakeAddressRegistrationCertificate req =
  case req of
    StakeAddrRegistrationPreConway atMostEra scred ->
      shelleyCertificateConstraints atMostEra
        $ makeStakeAddressRegistrationCertificatePreConway atMostEra scred
    StakeAddrRegistrationConway cOnwards ll scred ->
      conwayCertificateConstraints cOnwards
        $ makeStakeAddressRegistrationCertificatePostConway cOnwards scred ll
 where
  makeStakeAddressRegistrationCertificatePreConway :: ()
    => EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
    => Ledger.ShelleyEraTxCert (ShelleyLedgerEra era)
    => Ledger.TxCert (ShelleyLedgerEra era) ~ Ledger.ShelleyTxCert (ShelleyLedgerEra era)
    => ShelleyToBabbageEra era
    -> StakeCredential
    -> Certificate era
  makeStakeAddressRegistrationCertificatePreConway atMostBabbage scred =
    ShelleyRelatedCertificate atMostBabbage $ Ledger.mkRegTxCert $ toShelleyStakeCredential scred

  makeStakeAddressRegistrationCertificatePostConway :: ()
    => Ledger.TxCert (ShelleyLedgerEra era) ~ Ledger.ConwayTxCert (ShelleyLedgerEra era)
    => Ledger.ConwayEraTxCert (ShelleyLedgerEra era)
    => EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
    => ConwayEraOnwards era
    -> StakeCredential
    -> Lovelace
    -> Certificate era
  makeStakeAddressRegistrationCertificatePostConway cWayEraOn scred deposit =
    ConwayCertificate cWayEraOn
        $ Ledger.mkRegDepositTxCert
            (toShelleyStakeCredential scred)
            (toShelleyLovelace deposit)

makeStakeAddressUnregistrationCertificate :: StakeAddressRequirements era -> Certificate era
makeStakeAddressUnregistrationCertificate req =
  case req of
    StakeAddrRegistrationConway cOnwards ll scred ->
      conwayCertificateConstraints cOnwards
        $ makeStakeAddressDeregistrationCertificatePostConway cOnwards scred ll

    StakeAddrRegistrationPreConway atMostEra scred ->
      shelleyCertificateConstraints atMostEra
        $ makeStakeAddressDeregistrationCertificatePreConway atMostEra scred
 where
  makeStakeAddressDeregistrationCertificatePreConway
    :: Ledger.ShelleyEraTxCert (ShelleyLedgerEra era)
    => Ledger.TxCert (ShelleyLedgerEra era) ~ Ledger.ShelleyTxCert (ShelleyLedgerEra era)
    => EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
    => ShelleyToBabbageEra era
    -> StakeCredential
    -> Certificate era
  makeStakeAddressDeregistrationCertificatePreConway aMostBab scred =
    ShelleyRelatedCertificate aMostBab
      $ Ledger.mkUnRegTxCert $ toShelleyStakeCredential scred

  makeStakeAddressDeregistrationCertificatePostConway
    :: EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
    => Ledger.TxCert (ShelleyLedgerEra era) ~ Ledger.ConwayTxCert (ShelleyLedgerEra era)
    => Ledger.ConwayEraTxCert (ShelleyLedgerEra era)
    => ConwayEraOnwards era
    -> StakeCredential
    -> Lovelace
    -> Certificate era
  makeStakeAddressDeregistrationCertificatePostConway cOn scred deposit  =
    ConwayCertificate cOn
      $ Ledger.mkUnRegDepositTxCert
          (toShelleyStakeCredential scred)
          (toShelleyLovelace deposit)

{-# DEPRECATED makeStakeAddressPoolDelegationCertificate "This function is deprecated, please use 'makeStakeAddressDelegationCertificate' instead." #-}
makeStakeAddressPoolDelegationCertificate :: ()
  => ShelleyBasedEra era
  -> StakeCredential
  -> PoolId
  -> Certificate era
makeStakeAddressPoolDelegationCertificate sbe scred poolId =
  case sbe of
    ShelleyBasedEraShelley ->
      makeStakeAddressDelegationCertificate
        (StakeDelegationRequirementsPreConway ShelleyToBabbageEraShelley scred poolId)
    ShelleyBasedEraAllegra ->
      makeStakeAddressDelegationCertificate
        (StakeDelegationRequirementsPreConway ShelleyToBabbageEraAllegra scred poolId)
    ShelleyBasedEraMary ->
      makeStakeAddressDelegationCertificate
        (StakeDelegationRequirementsPreConway ShelleyToBabbageEraMary scred poolId)
    ShelleyBasedEraAlonzo ->
      makeStakeAddressDelegationCertificate
        (StakeDelegationRequirementsPreConway ShelleyToBabbageEraAlonzo scred poolId)
    ShelleyBasedEraBabbage ->
      makeStakeAddressDelegationCertificate
        (StakeDelegationRequirementsPreConway ShelleyToBabbageEraBabbage scred poolId)
    ShelleyBasedEraConway ->
      makeStakeAddressDelegationCertificate
        (StakeDelegationRequirementsConwayOnwards ConwayEraOnwardsConway scred (Ledger.DelegStake $ unStakePoolKeyHash poolId))

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
makeStakeAddressDelegationCertificate req =
  case req of
    StakeDelegationRequirementsConwayOnwards cOnwards scred delegatee ->
      conwayCertificateConstraints cOnwards
        $ ConwayCertificate cOnwards
        $ Ledger.mkDelegTxCert (toShelleyStakeCredential scred) delegatee

    StakeDelegationRequirementsPreConway atMostBabbage scred pid ->
      shelleyCertificateConstraints atMostBabbage
        $ ShelleyRelatedCertificate atMostBabbage
        $ Ledger.mkDelegStakeTxCert (toShelleyStakeCredential scred) (unStakePoolKeyHash pid)

data StakePoolRegistrationRequirements era where
  StakePoolRegistrationRequirementsConwayOnwards
    :: ConwayEraOnwards era
    -> Ledger.PoolParams (EraCrypto (ShelleyLedgerEra era))
    -> StakePoolRegistrationRequirements era

  StakePoolRegistrationRequirementsPreConway
    :: ShelleyToBabbageEra era
    -> Ledger.PoolParams (EraCrypto (ShelleyLedgerEra era))
    -> StakePoolRegistrationRequirements era

makeStakePoolRegistrationCertificate :: ()
  => StakePoolRegistrationRequirements era
  -> Certificate era
makeStakePoolRegistrationCertificate req =
  case req of
    StakePoolRegistrationRequirementsConwayOnwards cOnwards poolParams ->
      conwayCertificateConstraints cOnwards
        $ ConwayCertificate cOnwards
        $ Ledger.mkRegPoolTxCert poolParams
    StakePoolRegistrationRequirementsPreConway atMostBab poolParams ->
      shelleyCertificateConstraints atMostBab
        $ ShelleyRelatedCertificate atMostBab
        $ Ledger.mkRegPoolTxCert poolParams

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

makeStakePoolRetirementCertificate :: ()
  => StakePoolRetirementRequirements era
  -> Certificate era
makeStakePoolRetirementCertificate req =
  case req of
    StakePoolRetirementRequirementsPreConway atMostBab poolId retirementEpoch ->
      shelleyCertificateConstraints atMostBab
        $ ShelleyRelatedCertificate atMostBab
        $ Ledger.mkRetirePoolTxCert (unStakePoolKeyHash poolId) retirementEpoch
    StakePoolRetirementRequirementsConwayOnwards atMostBab poolId retirementEpoch ->
      conwayCertificateConstraints atMostBab
        $ ConwayCertificate atMostBab
        $ Ledger.mkRetirePoolTxCert (unStakePoolKeyHash poolId) retirementEpoch

data GenesisKeyDelegationRequirements ere where
  GenesisKeyDelegationRequirements
    :: ShelleyToBabbageEra era
    -> Hash GenesisKey
    -> Hash GenesisDelegateKey
    -> Hash VrfKey
    -> GenesisKeyDelegationRequirements era

makeGenesisKeyDelegationCertificate :: GenesisKeyDelegationRequirements era -> Certificate era
makeGenesisKeyDelegationCertificate (GenesisKeyDelegationRequirements atMostEra
                                       (GenesisKeyHash hGenKey) (GenesisDelegateKeyHash hGenDelegKey) (VrfKeyHash hVrfKey)) =
  ShelleyRelatedCertificate atMostEra
    $ shelleyCertificateConstraints atMostEra
    $ Ledger.ShelleyTxCertGenesisDeleg $ Ledger.GenesisDelegCert hGenKey hGenDelegKey hVrfKey

data MirCertificateRequirements era where
  MirCertificateRequirements
    :: ShelleyToBabbageEra era
    -> Ledger.MIRPot
    -> Ledger.MIRTarget (EraCrypto (ShelleyLedgerEra era))
    -> MirCertificateRequirements era

makeMIRCertificate :: ()
  => MirCertificateRequirements era
  -> Certificate era
makeMIRCertificate (MirCertificateRequirements atMostEra mirPot mirTarget) =
  ShelleyRelatedCertificate atMostEra
    $ Ledger.ShelleyTxCertMir $ Ledger.MIRCert mirPot mirTarget

data DRepRegistrationRequirements era where
  DRepRegistrationRequirements
    :: ConwayEraOnwards era
    -> VotingCredential era
    -> Lovelace
    -> DRepRegistrationRequirements era


makeDrepRegistrationCertificate :: ()
  => DRepRegistrationRequirements era
  -> Certificate era
makeDrepRegistrationCertificate (DRepRegistrationRequirements conwayOnwards (VotingCredential vcred) deposit) =
  ConwayCertificate conwayOnwards
    . Ledger.ConwayTxCertCommittee
    . Ledger.ConwayRegDRep vcred
    $ toShelleyLovelace deposit

data CommitteeHotKeyAuthorizationRequirements era where
  CommitteeHotKeyAuthorizationRequirements
    :: ConwayEraOnwards era
    -> Ledger.KeyHash Ledger.CommitteeColdKey (EraCrypto (ShelleyLedgerEra era))
    -> Ledger.KeyHash Ledger.CommitteeHotKey (EraCrypto (ShelleyLedgerEra era))
    -> CommitteeHotKeyAuthorizationRequirements era

makeCommitteeHotKeyAuthorizationCertificate :: ()
  => CommitteeHotKeyAuthorizationRequirements era
  -> Certificate era
makeCommitteeHotKeyAuthorizationCertificate (CommitteeHotKeyAuthorizationRequirements cOnwards coldKeyHash hotKeyHash) =
  ConwayCertificate cOnwards
    . Ledger.ConwayTxCertCommittee
    $ Ledger.ConwayAuthCommitteeHotKey coldKeyHash hotKeyHash

data CommitteeColdkeyResignationRequirements era where
  CommitteeColdkeyResignationRequirements
    :: ConwayEraOnwards era
    -> Ledger.KeyHash Ledger.CommitteeColdKey (EraCrypto (ShelleyLedgerEra era))
    -> CommitteeColdkeyResignationRequirements era

makeCommitteeColdkeyResignationCertificate :: ()
  => CommitteeColdkeyResignationRequirements era
  -> Certificate era
makeCommitteeColdkeyResignationCertificate (CommitteeColdkeyResignationRequirements cOnwards coldKeyHash) =
  ConwayCertificate cOnwards
    . Ledger.ConwayTxCertCommittee
    $ Ledger.ConwayResignCommitteeColdKey coldKeyHash

data DRepUnregistrationRequirements era where
  DRepUnregistrationRequirements
    :: ConwayEraOnwards era
    -> VotingCredential era
    -> Lovelace
    -> DRepUnregistrationRequirements era

makeDrepUnregistrationCertificate :: ()
  => DRepUnregistrationRequirements era
  -> Certificate era
makeDrepUnregistrationCertificate (DRepUnregistrationRequirements conwayOnwards (VotingCredential vcred) deposit) =
  ConwayCertificate conwayOnwards
    . Ledger.ConwayTxCertCommittee
    . Ledger.ConwayUnRegDRep vcred
    $ toShelleyLovelace deposit

-- ----------------------------------------------------------------------------
-- Helper functions
--

selectStakeCredential
  :: ShelleyBasedEra era -> Certificate era -> Maybe StakeCredential
selectStakeCredential sbe cert =
  case cert of
    ShelleyRelatedCertificate _ (Ledger.ShelleyTxCertDelegCert (Ledger.ShelleyDelegCert stakecred _))
      -> Just $ obtainEraCryptoConstraints sbe $ fromShelleyStakeCredential stakecred
    ShelleyRelatedCertificate _ (Ledger.ShelleyTxCertPool (Ledger.RegPool poolParams))
      -> let poolCred = Ledger.KeyHashObj $ Ledger.ppId poolParams
         in Just $ obtainEraCryptoConstraints sbe $ fromShelleyStakeCredential $ Ledger.coerceKeyRole poolCred

    ConwayCertificate _ (Ledger.ConwayTxCertDeleg (Ledger.ConwayRegCert stakeCred _))
      -> Just $ obtainEraCryptoConstraints sbe $ fromShelleyStakeCredential stakeCred
    ConwayCertificate _ (Ledger.ConwayTxCertPool (Ledger.RegPool poolParams))
      -> let poolCred = Ledger.KeyHashObj $ Ledger.ppId poolParams
         in Just $ obtainEraCryptoConstraints sbe $ fromShelleyStakeCredential $ Ledger.coerceKeyRole poolCred

    _                                                 -> Nothing

filterUnRegCreds
  :: ShelleyBasedEra era -> Certificate era -> Maybe StakeCredential
filterUnRegCreds sbe cert =
  case cert of
    ShelleyRelatedCertificate _ (Ledger.ShelleyTxCertDelegCert (Ledger.ShelleyUnRegCert cred)) ->
      Just $ obtainEraCryptoConstraints sbe $ fromShelleyStakeCredential cred
    ConwayCertificate _ (Ledger.ConwayTxCertDeleg (Ledger.ConwayUnRegCert cred _)) ->
      Just $ obtainEraCryptoConstraints sbe $ fromShelleyStakeCredential cred
    _  -> Nothing

-- ----------------------------------------------------------------------------
-- Internal conversion functions
--

toShelleyCertificate :: ()
  => ShelleyBasedEra era
  -> Certificate era
  -> Ledger.TxCert (ShelleyLedgerEra era)
toShelleyCertificate sbe cert =
  case cert of
    ShelleyRelatedCertificate aMostBab _ ->
      toShelleyCertificateShelleyToBabbage aMostBab cert
    ConwayCertificate cOn _ ->
      case sbe of
        ShelleyBasedEraShelley -> case cOn of {}
        ShelleyBasedEraAllegra -> case cOn of {}
        ShelleyBasedEraMary -> case cOn of {}
        ShelleyBasedEraAlonzo -> case cOn of {}
        ShelleyBasedEraBabbage -> case cOn of {}
        ShelleyBasedEraConway -> toShelleyCertificateAtLeastConway cOn cert
 where
  toShelleyCertificateShelleyToBabbage :: ()
    => ShelleyToBabbageEra era
    -> Certificate era
    -> Ledger.TxCert (ShelleyLedgerEra era)
  toShelleyCertificateShelleyToBabbage aMostBabbage (ShelleyRelatedCertificate _ shelleyTxCert) =
    case aMostBabbage of
      ShelleyToBabbageEraBabbage -> shelleyTxCert
      ShelleyToBabbageEraAlonzo -> shelleyTxCert
      ShelleyToBabbageEraMary -> shelleyTxCert
      ShelleyToBabbageEraAllegra -> shelleyTxCert
      ShelleyToBabbageEraShelley -> shelleyTxCert
  toShelleyCertificateShelleyToBabbage aMost (ConwayCertificate ConwayEraOnwardsConway _) =
    case aMost of {}


  toShelleyCertificateAtLeastConway :: ()
    => ConwayEraOnwards era
    -> Certificate era
    -> Ledger.ConwayTxCert (ShelleyLedgerEra era)
  toShelleyCertificateAtLeastConway _ (ConwayCertificate _ c) = c
  toShelleyCertificateAtLeastConway ConwayEraOnwardsConway (ShelleyRelatedCertificate aMostBab _) =
    case aMostBab of {}

fromShelleyCertificate :: ()
  => ShelleyBasedEra era
  -> Ledger.TxCert (ShelleyLedgerEra era)
  -> Certificate era
fromShelleyCertificate = \case
  ShelleyBasedEraShelley  -> ShelleyRelatedCertificate ShelleyToBabbageEraShelley
  ShelleyBasedEraAllegra  -> ShelleyRelatedCertificate ShelleyToBabbageEraAllegra
  ShelleyBasedEraMary     -> ShelleyRelatedCertificate ShelleyToBabbageEraMary
  ShelleyBasedEraAlonzo   -> ShelleyRelatedCertificate ShelleyToBabbageEraAlonzo
  ShelleyBasedEraBabbage  -> ShelleyRelatedCertificate ShelleyToBabbageEraBabbage
  ShelleyBasedEraConway   -> ConwayCertificate ConwayEraOnwardsConway


toShelleyPoolParams :: StakePoolParameters -> Ledger.PoolParams StandardCrypto
toShelleyPoolParams StakePoolParameters {
                      stakePoolId            = StakePoolKeyHash poolkh
                    , stakePoolVRF           = VrfKeyHash vrfkh
                    , stakePoolCost
                    , stakePoolMargin
                    , stakePoolRewardAccount
                    , stakePoolPledge
                    , stakePoolOwners
                    , stakePoolRelays
                    , stakePoolMetadata
                    } =
    --TODO: validate pool parameters such as the PoolMargin below, but also
    -- do simple client-side sanity checks, e.g. on the pool metadata url
    Ledger.PoolParams {
      Ledger.ppId      = poolkh
    , Ledger.ppVrf     = vrfkh
    , Ledger.ppPledge  = toShelleyLovelace stakePoolPledge
    , Ledger.ppCost    = toShelleyLovelace stakePoolCost
    , Ledger.ppMargin  = fromMaybe
                               (error "toShelleyPoolParams: invalid PoolMargin")
                               (Ledger.boundRational stakePoolMargin)
    , Ledger.ppRewardAcnt   = toShelleyStakeAddr stakePoolRewardAccount
    , Ledger.ppOwners  = Set.fromList
                               [ kh | StakeKeyHash kh <- stakePoolOwners ]
    , Ledger.ppRelays  = Seq.fromList
                               (map toShelleyStakePoolRelay stakePoolRelays)
    , Ledger.ppMetadata = toShelleyPoolMetadata <$>
                              Ledger.maybeToStrictMaybe stakePoolMetadata
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
    toShelleyPoolMetadata StakePoolMetadataReference {
                            stakePoolMetadataURL
                          , stakePoolMetadataHash = StakePoolMetadataHash mdh
                          } =
      Ledger.PoolMetadata {
        Ledger.pmUrl  = toShelleyUrl stakePoolMetadataURL
      , Ledger.pmHash = Ledger.hashToBytes mdh
      }

    toShelleyDnsName :: ByteString -> Ledger.DnsName
    toShelleyDnsName = fromMaybe (error "toShelleyDnsName: invalid dns name. TODO: proper validation")
                     . Ledger.textToDns
                     . Text.decodeLatin1

    toShelleyUrl :: Text -> Ledger.Url
    toShelleyUrl = fromMaybe (error "toShelleyUrl: invalid url. TODO: proper validation")
                 . Ledger.textToUrl


fromShelleyPoolParams :: Ledger.PoolParams StandardCrypto
                      -> StakePoolParameters
fromShelleyPoolParams
    Ledger.PoolParams {
      Ledger.ppId
    , Ledger.ppVrf
    , Ledger.ppPledge
    , Ledger.ppCost
    , Ledger.ppMargin
    , Ledger.ppRewardAcnt
    , Ledger.ppOwners
    , Ledger.ppRelays
    , Ledger.ppMetadata
    } =
    StakePoolParameters {
      stakePoolId            = StakePoolKeyHash ppId
    , stakePoolVRF           = VrfKeyHash ppVrf
    , stakePoolCost          = fromShelleyLovelace ppCost
    , stakePoolMargin        = Ledger.unboundRational ppMargin
    , stakePoolRewardAccount = fromShelleyStakeAddr ppRewardAcnt
    , stakePoolPledge        = fromShelleyLovelace ppPledge
    , stakePoolOwners        = map StakeKeyHash (Set.toList ppOwners)
    , stakePoolRelays        = map fromShelleyStakePoolRelay
                                   (Foldable.toList ppRelays)
    , stakePoolMetadata      = fromShelleyPoolMetadata <$>
                                 Ledger.strictMaybeToMaybe ppMetadata
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
    fromShelleyPoolMetadata Ledger.PoolMetadata {
                              Ledger.pmUrl
                            , Ledger.pmHash
                            } =
      StakePoolMetadataReference {
        stakePoolMetadataURL  = Ledger.urlToText pmUrl
      , stakePoolMetadataHash = StakePoolMetadataHash
                              . fromMaybe (error "fromShelleyPoolMetadata: invalid hash. TODO: proper validation")
                              . Ledger.hashFromBytes
                              $ pmHash
      }

    --TODO: change the ledger rep of the DNS name to use ShortByteString
    fromShelleyDnsName :: Ledger.DnsName -> ByteString
    fromShelleyDnsName = Text.encodeUtf8
                       . Ledger.dnsToText

shelleyCertificateConstraints
  :: ShelleyToBabbageEra era
  -> (( Ledger.ShelleyEraTxCert (ShelleyLedgerEra era)
      , EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
      , Ledger.TxCert (ShelleyLedgerEra era) ~ Ledger.ShelleyTxCert (ShelleyLedgerEra era)
      ) => a)
  -> a
shelleyCertificateConstraints ShelleyToBabbageEraBabbage f = f
shelleyCertificateConstraints ShelleyToBabbageEraAlonzo f = f
shelleyCertificateConstraints ShelleyToBabbageEraMary    f = f
shelleyCertificateConstraints ShelleyToBabbageEraAllegra  f = f
shelleyCertificateConstraints ShelleyToBabbageEraShelley f = f

conwayCertificateConstraints
  :: ConwayEraOnwards era
  -> (( Ledger.ConwayEraTxCert (ShelleyLedgerEra era)
      , EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
      , Ledger.TxCert (ShelleyLedgerEra era) ~ Ledger.ConwayTxCert (ShelleyLedgerEra era)
      ) => a)
  -> a
conwayCertificateConstraints ConwayEraOnwardsConway f = f

