{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Certificates embedded in transactions
--
module Cardano.Api.Certificate (
    Certificate(..),

    -- * Registering stake address and delegating
    makeStakeAddressRegistrationCertificate,
    makeStakeAddressDeregistrationCertificate,
    makeStakeAddressPoolDelegationCertificate,
    PoolId,

    -- * Registering stake pools
    makeStakePoolRegistrationCertificate,
    makeStakePoolRetirementCertificate,
    StakePoolParameters(..),
    StakePoolRelay(..),
    StakePoolMetadataReference(..),

    makeCommitteeDelegationCertificate,
    makeCommitteeHotKeyUnregistrationCertificate,

    -- * Registering DReps
    DRepMetadataReference(..),

    -- * Special certificates
    makeMIRCertificate,
    makeGenesisKeyDelegationCertificate,
    MIRTarget (..),

    -- * Internal conversion functions
    toShelleyCertificate,
    fromShelleyCertificate,
    toShelleyPoolParams,
    fromShelleyPoolParams,

    -- * Data family instances
    AsType(..)
  ) where

import           Cardano.Api.Address
import           Cardano.Api.DRepMetadata
import           Cardano.Api.EraCast
import           Cardano.Api.Eras
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Keys.Praos
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.StakePoolMetadata
import           Cardano.Api.Utils
import           Cardano.Api.Value

import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Api.Era as L
import           Cardano.Ledger.BaseTypes (maybeToStrictMaybe, strictMaybeToMaybe)
import qualified Cardano.Ledger.BaseTypes as Shelley
import qualified Cardano.Ledger.Coin as Shelley (toDeltaCoin)
import qualified Cardano.Ledger.Conway.TxCert as Conway
import qualified Cardano.Ledger.Core as Shelley
import           Cardano.Ledger.Crypto (StandardCrypto)
import           Cardano.Ledger.Shelley.TxBody (MIRPot (..))
import qualified Cardano.Ledger.Shelley.TxBody as Shelley
import qualified Cardano.Ledger.Shelley.TxCert as Shelley
import           Cardano.Slotting.Slot (EpochNo (..))

import           Data.ByteString (ByteString)
import qualified Data.Foldable as Foldable
import           Data.IP (IPv4, IPv6)
import qualified Data.Map.Strict as Map
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
       :: AtMostBabbageEra era
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
          Shelley.toEraCBOR @(ShelleyLedgerEra era) . toShelleyCertificate shelleyBasedEra
        ShelleyBasedEraAllegra  ->
          Shelley.toEraCBOR @(ShelleyLedgerEra era) . toShelleyCertificate shelleyBasedEra
        ShelleyBasedEraMary     ->
          Shelley.toEraCBOR @(ShelleyLedgerEra era) . toShelleyCertificate shelleyBasedEra
        ShelleyBasedEraAlonzo   ->
          Shelley.toEraCBOR @(ShelleyLedgerEra era) . toShelleyCertificate shelleyBasedEra
        ShelleyBasedEraBabbage  ->
          Shelley.toEraCBOR @(ShelleyLedgerEra era) . toShelleyCertificate shelleyBasedEra
        ShelleyBasedEraConway   ->
          Shelley.toEraCBOR @(ShelleyLedgerEra era) . toShelleyCertificate shelleyBasedEra



instance
  ( IsShelleyBasedEra era
  ) => FromCBOR (Certificate era) where
    fromCBOR =
      case shelleyBasedEra @era of
        ShelleyBasedEraShelley  ->
          fromShelleyCertificate shelleyBasedEra <$> Shelley.fromEraCBOR @(ShelleyLedgerEra era)
        ShelleyBasedEraAllegra  ->
          fromShelleyCertificate shelleyBasedEra <$> Shelley.fromEraCBOR @(ShelleyLedgerEra era)
        ShelleyBasedEraMary     ->
          fromShelleyCertificate shelleyBasedEra <$> Shelley.fromEraCBOR @(ShelleyLedgerEra era)
        ShelleyBasedEraAlonzo   ->
          fromShelleyCertificate shelleyBasedEra <$> Shelley.fromEraCBOR @(ShelleyLedgerEra era)
        ShelleyBasedEraBabbage  ->
          fromShelleyCertificate shelleyBasedEra <$> Shelley.fromEraCBOR @(ShelleyLedgerEra era)
        ShelleyBasedEraConway   ->
          fromShelleyCertificateAtLeastConway <$> Shelley.fromEraCBOR @(ShelleyLedgerEra era)


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
  eraCast _ = \case
    StakeAddressRegistrationCertificate c ->
      pure $ StakeAddressRegistrationCertificate c
    StakeAddressDeregistrationCertificate stakeCredential ->
      pure $ StakeAddressDeregistrationCertificate stakeCredential
    StakeAddressPoolDelegationCertificate stakeCredential poolId ->
      pure $ StakeAddressPoolDelegationCertificate stakeCredential poolId
    StakePoolRegistrationCertificate stakePoolParameters ->
      pure $ StakePoolRegistrationCertificate stakePoolParameters
    StakePoolRetirementCertificate poolId epochNo ->
      pure $ StakePoolRetirementCertificate poolId epochNo
    GenesisKeyDelegationCertificate genesisKH genesisDelegateKH vrfKH ->
      pure $ GenesisKeyDelegationCertificate genesisKH genesisDelegateKH vrfKH
    CommitteeDelegationCertificate coldKeyHash hotKeyHash ->
      pure $ CommitteeDelegationCertificate coldKeyHash hotKeyHash
    CommitteeHotKeyDeregistrationCertificate coldKeyHash ->
      pure $ CommitteeHotKeyDeregistrationCertificate coldKeyHash
    MIRCertificate mirPot mirTarget ->
      pure $ MIRCertificate mirPot mirTarget

-- | The 'MIRTarget' determines the target of a 'MIRCertificate'.
-- A 'MIRCertificate' moves lovelace from either the reserves or the treasury
-- to either a collection of stake credentials or to the other pot.
data MIRTarget =

     -- | Use 'StakeAddressesMIR' to make the target of a 'MIRCertificate'
     -- a mapping of stake credentials to lovelace.
     StakeAddressesMIR [(StakeCredential, Lovelace)]

     -- | Use 'SendToReservesMIR' to make the target of a 'MIRCertificate'
     -- the reserves pot.
   | SendToReservesMIR Lovelace

     -- | Use 'SendToTreasuryMIR' to make the target of a 'MIRCertificate'
     -- the treasury pot.
   | SendToTreasuryMIR Lovelace
  deriving stock (Eq, Show)

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

data AtMostBabbageEra era where
  AtMostBabbageEraBabbage :: AtMostBabbageEra BabbageEra
  AtMostBabbageEraAlonzo :: AtMostBabbageEra AlonzoEra
  AtMostBabbageEraMary :: AtMostBabbageEra MaryEra
  AtMostBabbageEraAllegra :: AtMostBabbageEra AllegraEra
  AtMostBabbageEraShelley :: AtMostBabbageEra ShelleyEra

deriving instance Show (AtMostBabbageEra era)
deriving instance Eq (AtMostBabbageEra era)

data StakeAddressRequirements era where
  StakeAddrRegistrationConway
    :: ConwayEraOnwards era
    -> Lovelace
    -> StakeCredential
    -> StakeAddressRequirements era

  StakeAddrRegistrationPreConway
    :: AtMostBabbageEra era
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
    => AtMostBabbageEra era
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
    => AtMostBabbageEra era
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
        (StakeDelegationRequirementsPreConway AtMostBabbageEraShelley scred poolId)
    ShelleyBasedEraAllegra ->
      makeStakeAddressDelegationCertificate
        (StakeDelegationRequirementsPreConway AtMostBabbageEraAllegra scred poolId)
    ShelleyBasedEraMary ->
      makeStakeAddressDelegationCertificate
        (StakeDelegationRequirementsPreConway AtMostBabbageEraMary scred poolId)
    ShelleyBasedEraAlonzo ->
      makeStakeAddressDelegationCertificate
        (StakeDelegationRequirementsPreConway AtMostBabbageEraAlonzo scred poolId)
    ShelleyBasedEraBabbage ->
      makeStakeAddressDelegationCertificate
        (StakeDelegationRequirementsPreConway AtMostBabbageEraBabbage scred poolId)
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
    :: AtMostBabbageEra era
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
    :: AtMostBabbageEra era
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
    :: AtMostBabbageEra era
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
    :: AtMostBabbageEra era
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
    :: AtMostBabbageEra era
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
      toShelleyCertificateAtMostBabbage aMostBab cert
    ConwayCertificate cOn _ ->
      case sbe of
        ShelleyBasedEraShelley -> case cOn of {}
        ShelleyBasedEraAllegra -> case cOn of {}
        ShelleyBasedEraMary -> case cOn of {}
        ShelleyBasedEraAlonzo -> case cOn of {}
        ShelleyBasedEraBabbage -> case cOn of {}
        ShelleyBasedEraConway -> toShelleyCertificateAtLeastConway cOn cert
 where
  toShelleyCertificateAtMostBabbage :: ()
    => AtMostBabbageEra era
    -> Certificate era
    -> Ledger.TxCert (ShelleyLedgerEra era)
  toShelleyCertificateAtMostBabbage aMostBabbage (ShelleyRelatedCertificate _ shelleyTxCert) =
    case aMostBabbage of
      AtMostBabbageEraBabbage -> shelleyTxCert
      AtMostBabbageEraAlonzo -> shelleyTxCert
      AtMostBabbageEraMary -> shelleyTxCert
      AtMostBabbageEraAllegra -> shelleyTxCert
      AtMostBabbageEraShelley -> shelleyTxCert
  toShelleyCertificateAtMostBabbage aMost (ConwayCertificate ConwayEraOnwardsConway _) =
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
  ShelleyBasedEraShelley  -> ShelleyRelatedCertificate AtMostBabbageEraShelley
  ShelleyBasedEraAllegra  -> ShelleyRelatedCertificate AtMostBabbageEraAllegra
  ShelleyBasedEraMary     -> ShelleyRelatedCertificate AtMostBabbageEraMary
  ShelleyBasedEraAlonzo   -> ShelleyRelatedCertificate AtMostBabbageEraAlonzo
  ShelleyBasedEraBabbage  -> ShelleyRelatedCertificate AtMostBabbageEraBabbage
  ShelleyBasedEraConway   -> ConwayCertificate ConwayEraOnwardsConway

toShelleyPoolParams :: StakePoolParameters -> Shelley.PoolParams StandardCrypto
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
    Shelley.PoolParams {
      Shelley.ppId      = poolkh
    , Shelley.ppVrf     = vrfkh
    , Shelley.ppPledge  = toShelleyLovelace stakePoolPledge
    , Shelley.ppCost    = toShelleyLovelace stakePoolCost
    , Shelley.ppMargin  = fromMaybe
                               (error "toShelleyPoolParams: invalid PoolMargin")
                               (Shelley.boundRational stakePoolMargin)
    , Shelley.ppRewardAcnt   = toShelleyStakeAddr stakePoolRewardAccount
    , Shelley.ppOwners  = Set.fromList
                               [ kh | StakeKeyHash kh <- stakePoolOwners ]
    , Shelley.ppRelays  = Seq.fromList
                               (map toShelleyStakePoolRelay stakePoolRelays)
    , Shelley.ppMetadata = toShelleyPoolMetadata <$>
                              maybeToStrictMaybe stakePoolMetadata
    }
  where
    toShelleyStakePoolRelay :: StakePoolRelay -> Shelley.StakePoolRelay
    toShelleyStakePoolRelay (StakePoolRelayIp mipv4 mipv6 mport) =
      Shelley.SingleHostAddr
        (fromIntegral <$> maybeToStrictMaybe mport)
        (maybeToStrictMaybe mipv4)
        (maybeToStrictMaybe mipv6)

    toShelleyStakePoolRelay (StakePoolRelayDnsARecord dnsname mport) =
      Shelley.SingleHostName
        (fromIntegral <$> maybeToStrictMaybe mport)
        (toShelleyDnsName dnsname)

    toShelleyStakePoolRelay (StakePoolRelayDnsSrvRecord dnsname) =
      Shelley.MultiHostName
        (toShelleyDnsName dnsname)

    toShelleyPoolMetadata :: StakePoolMetadataReference -> Shelley.PoolMetadata
    toShelleyPoolMetadata StakePoolMetadataReference {
                            stakePoolMetadataURL
                          , stakePoolMetadataHash = StakePoolMetadataHash mdh
                          } =
      Shelley.PoolMetadata {
        Shelley.pmUrl  = toShelleyUrl stakePoolMetadataURL
      , Shelley.pmHash = Crypto.hashToBytes mdh
      }

    toShelleyDnsName :: ByteString -> Shelley.DnsName
    toShelleyDnsName = fromMaybe (error "toShelleyDnsName: invalid dns name. TODO: proper validation")
                     . Shelley.textToDns
                     . Text.decodeLatin1

    toShelleyUrl :: Text -> Shelley.Url
    toShelleyUrl = fromMaybe (error "toShelleyUrl: invalid url. TODO: proper validation")
                 . Shelley.textToUrl


fromShelleyPoolParams :: Shelley.PoolParams StandardCrypto
                      -> StakePoolParameters
fromShelleyPoolParams
    Shelley.PoolParams {
      Shelley.ppId
    , Shelley.ppVrf
    , Shelley.ppPledge
    , Shelley.ppCost
    , Shelley.ppMargin
    , Shelley.ppRewardAcnt
    , Shelley.ppOwners
    , Shelley.ppRelays
    , Shelley.ppMetadata
    } =
    StakePoolParameters {
      stakePoolId            = StakePoolKeyHash ppId
    , stakePoolVRF           = VrfKeyHash ppVrf
    , stakePoolCost          = fromShelleyLovelace ppCost
    , stakePoolMargin        = Shelley.unboundRational ppMargin
    , stakePoolRewardAccount = fromShelleyStakeAddr ppRewardAcnt
    , stakePoolPledge        = fromShelleyLovelace ppPledge
    , stakePoolOwners        = map StakeKeyHash (Set.toList ppOwners)
    , stakePoolRelays        = map fromShelleyStakePoolRelay
                                   (Foldable.toList ppRelays)
    , stakePoolMetadata      = fromShelleyPoolMetadata <$>
                                 strictMaybeToMaybe ppMetadata
    }
  where
    fromShelleyStakePoolRelay :: Shelley.StakePoolRelay -> StakePoolRelay
    fromShelleyStakePoolRelay (Shelley.SingleHostAddr mport mipv4 mipv6) =
      StakePoolRelayIp
        (strictMaybeToMaybe mipv4)
        (strictMaybeToMaybe mipv6)
        (fromIntegral . Shelley.portToWord16 <$> strictMaybeToMaybe mport)

    fromShelleyStakePoolRelay (Shelley.SingleHostName mport dnsname) =
      StakePoolRelayDnsARecord
        (fromShelleyDnsName dnsname)
        (fromIntegral . Shelley.portToWord16 <$> strictMaybeToMaybe mport)

    fromShelleyStakePoolRelay (Shelley.MultiHostName dnsname) =
      StakePoolRelayDnsSrvRecord
        (fromShelleyDnsName dnsname)

    fromShelleyPoolMetadata :: Shelley.PoolMetadata -> StakePoolMetadataReference
    fromShelleyPoolMetadata Shelley.PoolMetadata {
                              Shelley.pmUrl
                            , Shelley.pmHash
                            } =
      StakePoolMetadataReference {
        stakePoolMetadataURL  = Shelley.urlToText pmUrl
      , stakePoolMetadataHash = StakePoolMetadataHash
                              . fromMaybe (error "fromShelleyPoolMetadata: invalid hash. TODO: proper validation")
                              . Crypto.hashFromBytes
                              $ pmHash
      }

    --TODO: change the ledger rep of the DNS name to use ShortByteString
    fromShelleyDnsName :: Shelley.DnsName -> ByteString
    fromShelleyDnsName = Text.encodeUtf8
                       . Shelley.dnsToText
