{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}


-- | Certificates embedded in transactions
--
module Cardano.Api.Certificate (
    Certificate(..),
    AllErasCertificates(..),

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
import           Cardano.Api.Eras
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Keys.Praos
import           Cardano.Api.Keys.Shelley
import           Cardano.Api.SerialiseCBOR
import           Cardano.Api.SerialiseTextEnvelope
import           Cardano.Api.StakePoolMetadata
import           Cardano.Api.Value

import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Babbage as L
import           Cardano.Ledger.BaseTypes (maybeToStrictMaybe, strictMaybeToMaybe)
import qualified Cardano.Ledger.BaseTypes as Shelley
import qualified Cardano.Ledger.Binary.Decoding as LedgerDec
import qualified Cardano.Ledger.Binary.Plain as Ledger
import qualified Cardano.Ledger.Binary.Version as Ledger
import qualified Cardano.Ledger.Coin as Shelley (toDeltaCoin)
import qualified Cardano.Ledger.Conway as Conway
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
import           Network.Socket (PortNumber)


-- ----------------------------------------------------------------------------
-- Certificates embedded in transactions
--

data DeprecatedAfterBabbageCertificates
  = GenesisKeyDelegationCertificate
      (Hash GenesisKey)
      (Hash GenesisDelegateKey)
      (Hash VrfKey)
  | MIRCertificate MIRPot MIRTarget
  deriving (Eq, Show)

data AllErasCertificates
   = -- Stake address certificates
     StakeAddressRegistrationCertificate   StakeCredential
   | StakeAddressDeregistrationCertificate StakeCredential
   | StakeAddressPoolDelegationCertificate StakeCredential PoolId

     -- Stake pool certificates
   | StakePoolRegistrationCertificate StakePoolParameters
   | StakePoolRetirementCertificate   PoolId EpochNo
   deriving (Eq, Show)

toBabbageCertificate
  :: ShelleyBasedEra era
  -> DeprecatedAfterBabbageCertificates
  -> Maybe (Shelley.TxCert (ShelleyLedgerEra era))
toBabbageCertificate sbe (MIRCertificate mirPot (SendToTreasuryMIR amount)) =
  case sbe of
    ShelleyBasedEraShelley ->
      case mirPot of
        ReservesMIR ->
          Just $ Shelley.MirTxCert $
            Shelley.MIRCert
              ReservesMIR
              (Shelley.SendToOppositePotMIR $ toShelleyLovelace amount)
        TreasuryMIR ->
          error "toBabbageCertificate: Incorrect MIRPot specified. Expected ReservesMIR but got TreasuryMIR"
    ShelleyBasedEraAllegra ->
      case mirPot of
        ReservesMIR ->
          Just $ Shelley.MirTxCert $
            Shelley.MIRCert
              ReservesMIR
              (Shelley.SendToOppositePotMIR $ toShelleyLovelace amount)
        TreasuryMIR ->
          error "toBabbageCertificate: Incorrect MIRPot specified. Expected ReservesMIR but got TreasuryMIR"
    ShelleyBasedEraMary ->
      case mirPot of
        ReservesMIR ->
          Just $ Shelley.MirTxCert $
            Shelley.MIRCert
              ReservesMIR
              (Shelley.SendToOppositePotMIR $ toShelleyLovelace amount)
        TreasuryMIR ->
          error "toBabbageCertificate: Incorrect MIRPot specified. Expected ReservesMIR but got TreasuryMIR"
    ShelleyBasedEraAlonzo ->
      case mirPot of
        ReservesMIR ->
          Just $ Shelley.MirTxCert $
            Shelley.MIRCert
              ReservesMIR
              (Shelley.SendToOppositePotMIR $ toShelleyLovelace amount)
        TreasuryMIR ->
          error "toBabbageCertificate: Incorrect MIRPot specified. Expected ReservesMIR but got TreasuryMIR"
    ShelleyBasedEraBabbage ->
      case mirPot of
        ReservesMIR ->
          Just $ Shelley.MirTxCert $
            Shelley.MIRCert
              ReservesMIR
              (Shelley.SendToOppositePotMIR $ toShelleyLovelace amount)
        TreasuryMIR ->
          error "toBabbageCertificate: Incorrect MIRPot specified. Expected ReservesMIR but got TreasuryMIR"
    _ -> Nothing

toBabbageCertificate sbe (MIRCertificate mirPot (SendToReservesMIR amount)) =
  case sbe of
    ShelleyBasedEraShelley ->
      case mirPot of
        TreasuryMIR ->
          Just $ Shelley.MirTxCert $
            Shelley.MIRCert
              TreasuryMIR
              (Shelley.SendToOppositePotMIR $ toShelleyLovelace amount)
        ReservesMIR ->
          error "toBabbageCertificate: Incorrect MIRPot specified. Expected TreasuryMIR but got ReservesMIR"
    ShelleyBasedEraAllegra ->
      case mirPot of
        TreasuryMIR ->
          Just $ Shelley.MirTxCert $
            Shelley.MIRCert
              TreasuryMIR
              (Shelley.SendToOppositePotMIR $ toShelleyLovelace amount)
        ReservesMIR ->
          error "toBabbageCertificate: Incorrect MIRPot specified. Expected TreasuryMIR but got ReservesMIR"
    ShelleyBasedEraMary ->
            case mirPot of
        TreasuryMIR ->
          Just $ Shelley.MirTxCert $
            Shelley.MIRCert
              TreasuryMIR
              (Shelley.SendToOppositePotMIR $ toShelleyLovelace amount)
        ReservesMIR ->
          error "toBabbageCertificate: Incorrect MIRPot specified. Expected TreasuryMIR but got ReservesMIR"
    ShelleyBasedEraAlonzo ->
            case mirPot of
        TreasuryMIR ->
          Just $ Shelley.MirTxCert $
            Shelley.MIRCert
              TreasuryMIR
              (Shelley.SendToOppositePotMIR $ toShelleyLovelace amount)
        ReservesMIR ->
          error "toBabbageCertificate: Incorrect MIRPot specified. Expected TreasuryMIR but got ReservesMIR"
    ShelleyBasedEraBabbage ->
            case mirPot of
        TreasuryMIR ->
          Just $ Shelley.MirTxCert $
            Shelley.MIRCert
              TreasuryMIR
              (Shelley.SendToOppositePotMIR $ toShelleyLovelace amount)
        ReservesMIR ->
          error "toBabbageCertificate: Incorrect MIRPot specified. Expected TreasuryMIR but got ReservesMIR"
    _ -> Nothing

toBabbageCertificate sbe (MIRCertificate mirpot (StakeAddressesMIR amounts)) =
  case sbe of
    ShelleyBasedEraShelley ->
          Just $ Shelley.MirTxCert $
      Shelley.MIRCert
        mirpot
        (Shelley.StakeAddressesMIR $ Map.fromListWith (<>)
           [ (toShelleyStakeCredential sc, Shelley.toDeltaCoin . toShelleyLovelace $ v)
           | (sc, v) <- amounts ])
    ShelleyBasedEraAllegra ->
          Just $ Shelley.MirTxCert $
      Shelley.MIRCert
        mirpot
        (Shelley.StakeAddressesMIR $ Map.fromListWith (<>)
           [ (toShelleyStakeCredential sc, Shelley.toDeltaCoin . toShelleyLovelace $ v)
           | (sc, v) <- amounts ])
    ShelleyBasedEraMary ->
          Just $ Shelley.MirTxCert $
      Shelley.MIRCert
        mirpot
        (Shelley.StakeAddressesMIR $ Map.fromListWith (<>)
           [ (toShelleyStakeCredential sc, Shelley.toDeltaCoin . toShelleyLovelace $ v)
           | (sc, v) <- amounts ])
    ShelleyBasedEraAlonzo ->
          Just $ Shelley.MirTxCert $
      Shelley.MIRCert
        mirpot
        (Shelley.StakeAddressesMIR $ Map.fromListWith (<>)
           [ (toShelleyStakeCredential sc, Shelley.toDeltaCoin . toShelleyLovelace $ v)
           | (sc, v) <- amounts ])
    ShelleyBasedEraBabbage ->
          Just $ Shelley.MirTxCert $
      Shelley.MIRCert
        mirpot
        (Shelley.StakeAddressesMIR $ Map.fromListWith (<>)
           [ (toShelleyStakeCredential sc, Shelley.toDeltaCoin . toShelleyLovelace $ v)
           | (sc, v) <- amounts ])
    _ -> Nothing
toBabbageCertificate sbe (GenesisKeyDelegationCertificate (GenesisKeyHash genesiskh) (GenesisDelegateKeyHash delegatekh) (VrfKeyHash vrfkh)) =
  case sbe of
    ShelleyBasedEraShelley ->   Just $ Shelley.GenesisDelegTxCert genesiskh delegatekh vrfkh
    ShelleyBasedEraAllegra -> Just $ Shelley.GenesisDelegTxCert genesiskh delegatekh vrfkh
    ShelleyBasedEraMary -> Just $ Shelley.GenesisDelegTxCert genesiskh delegatekh vrfkh
    ShelleyBasedEraAlonzo -> Just $ Shelley.GenesisDelegTxCert genesiskh delegatekh vrfkh
    ShelleyBasedEraBabbage -> Just $ Shelley.GenesisDelegTxCert genesiskh delegatekh vrfkh
    _ -> Nothing


data Certificate
   =
   -- Certificates that work in all eras (Shelley era onwards)
     AllErasCerts  AllErasCertificates

   -- Special certificates
   | DeprecatedAfterBabbageCerts DeprecatedAfterBabbageCertificates

   -- Conway certs
   | CommitteeDelegationCertificate
      (Hash CommitteeColdKey)
      (Hash CommitteeHotKey)

   | CommitteeHotKeyDeregistrationCertificate
      (Hash CommitteeColdKey)



  deriving stock (Eq, Show)
  deriving anyclass SerialiseAsCBOR

instance HasTypeProxy Certificate where
    data AsType Certificate = AsCertificate
    proxyToAsType _ = AsCertificate

instance ToCBOR Certificate where
    toCBOR cert =
      case cert of
        DeprecatedAfterBabbageCerts c ->
          case toShelleyCertificate ShelleyBasedEraBabbage $ DeprecatedAfterBabbageCerts c of
            Nothing -> error $ "ToCBOR Certificate: Failed to convert cardano-api babbage certificate to ledger certificate: " <> show cert
            Just ledgerCert -> Shelley.toEraCBOR @L.Babbage ledgerCert
        shelleyCert -> do
          case toShelleyCertificate ShelleyBasedEraConway shelleyCert of
            Nothing -> error $ "ToCBOR Certificate: Failed to convert cardano-api shelley certificate to ledger certificate: " <> show cert
            Just ledgerCert -> Shelley.toEraCBOR @Conway.Conway ledgerCert

instance FromCBOR Certificate where
    fromCBOR = hackyCertificateDecoder

-- Conway: TODO
-- This is a hack. The solution is to parameterize `data Certificate` over the era.
hackyCertificateDecoder :: Ledger.Decoder s Certificate
hackyCertificateDecoder = do
 bs <- Ledger.decodeBytes
 babbageVersion <- Ledger.mkVersion (8 :: Int)
 case LedgerDec.decodeFull' babbageVersion bs of
  Right babbageCert ->
    case fromShelleyCertificate ShelleyBasedEraBabbage babbageCert of
      Just c -> pure c
      Nothing ->
       fail $ mconcat [ "Successfully decoded a babbage certificate but failed to convert it to a cardano-api certificate"
                      , "\n"
                      , "Babbage certificate: " <> show babbageCert
                      ]
  Left babErr -> do
    conwayVersion <- Ledger.mkVersion (9 :: Int)
    case LedgerDec.decodeFull' conwayVersion bs of
     Right conwayCert ->
       case fromShelleyCertificate ShelleyBasedEraConway conwayCert of
         Just c -> pure c
         Nothing ->
          fail $ mconcat [ "Successfully decoded a conway certificate but failed to convert it to a cardano-api certificate"
                         , "\n"
                         , "Conway certificate: " <> show conwayCert
                         ]
     Left conErr ->
            fail $ mconcat ["Failed to decode neither a babbage certificate nor a conway certificate"
                           , "\n"
                           , "Babbage decode error: " <> show babErr
                           , "\n"
                           , "Conway decode error: " <> show conErr
                           ]


    -- (fromShelleyCertificate @Shelley.Shelley) <$> Shelley.fromEraCBOR @Conway.Conway

instance HasTextEnvelope Certificate where
    textEnvelopeType _ = "CertificateShelley"
    textEnvelopeDefaultDescr cert = case cert of
      AllErasCerts StakeAddressRegistrationCertificate{} -> "Stake address registration"
      AllErasCerts StakeAddressDeregistrationCertificate{} -> "Stake address deregistration"
      AllErasCerts StakeAddressPoolDelegationCertificate{} -> "Stake address stake pool delegation"
      AllErasCerts StakePoolRegistrationCertificate{} -> "Pool registration"
      AllErasCerts StakePoolRetirementCertificate{} -> "Pool retirement"
      DeprecatedAfterBabbageCerts GenesisKeyDelegationCertificate{} -> "Genesis key delegation"
      CommitteeDelegationCertificate{} -> "Constitution committee member key delegation"
      CommitteeHotKeyDeregistrationCertificate{} -> "Constitution committee member hot key deregistration"
      DeprecatedAfterBabbageCerts MIRCertificate{} -> "MIR"

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

makeStakeAddressRegistrationCertificate :: StakeCredential -> Certificate
makeStakeAddressRegistrationCertificate = AllErasCerts . StakeAddressRegistrationCertificate

makeStakeAddressDeregistrationCertificate :: StakeCredential -> Certificate
makeStakeAddressDeregistrationCertificate = AllErasCerts . StakeAddressDeregistrationCertificate

makeStakeAddressPoolDelegationCertificate :: StakeCredential -> PoolId -> Certificate
makeStakeAddressPoolDelegationCertificate s pid = AllErasCerts $ StakeAddressPoolDelegationCertificate s pid

makeStakePoolRegistrationCertificate :: StakePoolParameters -> Certificate
makeStakePoolRegistrationCertificate = AllErasCerts . StakePoolRegistrationCertificate

makeStakePoolRetirementCertificate :: PoolId -> EpochNo -> Certificate
makeStakePoolRetirementCertificate pid eNo = AllErasCerts $ StakePoolRetirementCertificate pid eNo

makeGenesisKeyDelegationCertificate :: Hash GenesisKey
                                    -> Hash GenesisDelegateKey
                                    -> Hash VrfKey
                                    -> Certificate
makeGenesisKeyDelegationCertificate gKey gDelegKey vryK =
  DeprecatedAfterBabbageCerts $ GenesisKeyDelegationCertificate gKey gDelegKey vryK

makeCommitteeDelegationCertificate :: ()
  => Hash CommitteeColdKey
  -> Hash CommitteeHotKey
  -> Certificate
makeCommitteeDelegationCertificate = CommitteeDelegationCertificate

makeCommitteeHotKeyUnregistrationCertificate :: ()
  => Hash CommitteeColdKey
  -> Certificate
makeCommitteeHotKeyUnregistrationCertificate = CommitteeHotKeyDeregistrationCertificate

makeMIRCertificate :: MIRPot -> MIRTarget -> Certificate
makeMIRCertificate p t = DeprecatedAfterBabbageCerts $ MIRCertificate p t


-- ----------------------------------------------------------------------------
-- Internal conversion functions
--


toShelleyCertificate
  :: Shelley.EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
  => Shelley.ShelleyEraTxCert (ShelleyLedgerEra era)
  => ShelleyBasedEra era -> Certificate -> Maybe (Shelley.TxCert (ShelleyLedgerEra era))
toShelleyCertificate _ (AllErasCerts (StakeAddressRegistrationCertificate stakecred)) =
    Just $ Shelley.mkRegTxCert (toShelleyStakeCredential stakecred)

toShelleyCertificate _ (AllErasCerts (StakeAddressDeregistrationCertificate stakecred)) =
    Just $ Shelley.mkUnRegTxCert (toShelleyStakeCredential stakecred)

toShelleyCertificate _ (AllErasCerts (StakeAddressPoolDelegationCertificate
                        stakecred (StakePoolKeyHash poolid))) =
    Just $ Shelley.mkDelegStakeTxCert (toShelleyStakeCredential stakecred) poolid

toShelleyCertificate _ (AllErasCerts (StakePoolRegistrationCertificate poolparams)) =
    Just $ Shelley.mkRegPoolTxCert (toShelleyPoolParams poolparams)

toShelleyCertificate _ (AllErasCerts (StakePoolRetirementCertificate
                       (StakePoolKeyHash poolid) epochno)) =
    Just $ Shelley.mkRetirePoolTxCert poolid epochno

toShelleyCertificate sbe (DeprecatedAfterBabbageCerts babbage) = toBabbageCertificate sbe babbage

toShelleyCertificate _
  ( CommitteeDelegationCertificate
    (CommitteeColdKeyHash _ckh)
    (CommitteeHotKeyHash  _hkh)
  ) = error "TODO CIP-1694 Need ledger types for CommitteeDelegationCertificate"

toShelleyCertificate _
  ( CommitteeHotKeyDeregistrationCertificate
    (CommitteeColdKeyHash _ckh)
  ) = error "TODO CIP-1694 Need ledger types for CommitteeHotKeyDeregistrationCertificate"


fromShelleyCertificate
  :: Shelley.EraCrypto (ShelleyLedgerEra era) ~ StandardCrypto
  => Shelley.ShelleyEraTxCert (ShelleyLedgerEra era)
  => ShelleyBasedEra era -> Shelley.TxCert (ShelleyLedgerEra era) -> Maybe Certificate
fromShelleyCertificate _ (Shelley.RegTxCert stakecred) =
    Just. AllErasCerts  $ StakeAddressRegistrationCertificate
      (fromShelleyStakeCredential stakecred)

fromShelleyCertificate _ (Shelley.UnRegTxCert stakecred) =
    Just. AllErasCerts  $ StakeAddressDeregistrationCertificate
      (fromShelleyStakeCredential stakecred)

fromShelleyCertificate _ (Shelley.DelegStakeTxCert stakecred poolid) =
    Just. AllErasCerts  $ StakeAddressPoolDelegationCertificate
      (fromShelleyStakeCredential stakecred)
      (StakePoolKeyHash poolid)

fromShelleyCertificate _ (Shelley.RegPoolTxCert poolparams) =
    Just. AllErasCerts  $ StakePoolRegistrationCertificate
      (fromShelleyPoolParams poolparams)

fromShelleyCertificate _ (Shelley.RetirePoolTxCert poolid epochno) =
    Just. AllErasCerts  $ StakePoolRetirementCertificate
      (StakePoolKeyHash poolid)
      epochno

fromShelleyCertificate sbe cert =
  case sbe of
    ShelleyBasedEraConway -> Nothing
    ShelleyBasedEraShelley ->
      case cert of
        (Shelley.MirTxCert (Shelley.MIRCert mirpot (Shelley.StakeAddressesMIR amounts))) ->
          Just $ DeprecatedAfterBabbageCerts $ MIRCertificate
            mirpot
            (StakeAddressesMIR
              [ (fromShelleyStakeCredential sc, fromShelleyDeltaLovelace v)
              | (sc, v) <- Map.toList amounts ]
            )
        (Shelley.GenesisDelegTxCert genesiskh delegatekh vrfkh)->
          Just $ DeprecatedAfterBabbageCerts $ GenesisKeyDelegationCertificate
            (GenesisKeyHash genesiskh)
            (GenesisDelegateKeyHash delegatekh)
            (VrfKeyHash vrfkh)
        (Shelley.MirTxCert (Shelley.MIRCert ReservesMIR (Shelley.SendToOppositePotMIR amount))) ->
          Just $ DeprecatedAfterBabbageCerts $ MIRCertificate ReservesMIR (SendToTreasuryMIR $ fromShelleyLovelace amount)
        (Shelley.MirTxCert (Shelley.MIRCert TreasuryMIR (Shelley.SendToOppositePotMIR amount))) ->
          Just $ DeprecatedAfterBabbageCerts $ MIRCertificate TreasuryMIR
                                (SendToReservesMIR $ fromShelleyLovelace amount)

    ShelleyBasedEraAllegra ->
      case cert of
        (Shelley.MirTxCert (Shelley.MIRCert mirpot (Shelley.StakeAddressesMIR amounts))) ->
          Just $ DeprecatedAfterBabbageCerts $ MIRCertificate
            mirpot
            (StakeAddressesMIR
              [ (fromShelleyStakeCredential sc, fromShelleyDeltaLovelace v)
              | (sc, v) <- Map.toList amounts ]
            )
        (Shelley.GenesisDelegTxCert genesiskh delegatekh vrfkh)->
          Just $ DeprecatedAfterBabbageCerts $ GenesisKeyDelegationCertificate
            (GenesisKeyHash genesiskh)
            (GenesisDelegateKeyHash delegatekh)
            (VrfKeyHash vrfkh)
        (Shelley.MirTxCert (Shelley.MIRCert ReservesMIR (Shelley.SendToOppositePotMIR amount))) ->
          Just $ DeprecatedAfterBabbageCerts $ MIRCertificate ReservesMIR (SendToTreasuryMIR $ fromShelleyLovelace amount)
        (Shelley.MirTxCert (Shelley.MIRCert TreasuryMIR (Shelley.SendToOppositePotMIR amount))) ->
          Just $ DeprecatedAfterBabbageCerts $ MIRCertificate TreasuryMIR
                                (SendToReservesMIR $ fromShelleyLovelace amount)

    ShelleyBasedEraMary ->
      case cert of
        (Shelley.MirTxCert (Shelley.MIRCert mirpot (Shelley.StakeAddressesMIR amounts))) ->
          Just $ DeprecatedAfterBabbageCerts $ MIRCertificate
            mirpot
            (StakeAddressesMIR
              [ (fromShelleyStakeCredential sc, fromShelleyDeltaLovelace v)
              | (sc, v) <- Map.toList amounts ]
            )
        (Shelley.GenesisDelegTxCert genesiskh delegatekh vrfkh)->
          Just $ DeprecatedAfterBabbageCerts $ GenesisKeyDelegationCertificate
            (GenesisKeyHash genesiskh)
            (GenesisDelegateKeyHash delegatekh)
            (VrfKeyHash vrfkh)
        (Shelley.MirTxCert (Shelley.MIRCert ReservesMIR (Shelley.SendToOppositePotMIR amount))) ->
          Just $ DeprecatedAfterBabbageCerts $ MIRCertificate ReservesMIR (SendToTreasuryMIR $ fromShelleyLovelace amount)
        (Shelley.MirTxCert (Shelley.MIRCert TreasuryMIR (Shelley.SendToOppositePotMIR amount))) ->
          Just $ DeprecatedAfterBabbageCerts $ MIRCertificate TreasuryMIR
                                (SendToReservesMIR $ fromShelleyLovelace amount)

    ShelleyBasedEraAlonzo ->
      case cert of
        (Shelley.MirTxCert (Shelley.MIRCert mirpot (Shelley.StakeAddressesMIR amounts))) ->
          Just $ DeprecatedAfterBabbageCerts $ MIRCertificate
            mirpot
            (StakeAddressesMIR
              [ (fromShelleyStakeCredential sc, fromShelleyDeltaLovelace v)
              | (sc, v) <- Map.toList amounts ]
            )
        (Shelley.GenesisDelegTxCert genesiskh delegatekh vrfkh)->
          Just $ DeprecatedAfterBabbageCerts $ GenesisKeyDelegationCertificate
            (GenesisKeyHash genesiskh)
            (GenesisDelegateKeyHash delegatekh)
            (VrfKeyHash vrfkh)
        (Shelley.MirTxCert (Shelley.MIRCert ReservesMIR (Shelley.SendToOppositePotMIR amount))) ->
          Just $ DeprecatedAfterBabbageCerts $ MIRCertificate ReservesMIR (SendToTreasuryMIR $ fromShelleyLovelace amount)
        (Shelley.MirTxCert (Shelley.MIRCert TreasuryMIR (Shelley.SendToOppositePotMIR amount))) ->
          Just $ DeprecatedAfterBabbageCerts $ MIRCertificate TreasuryMIR
                                (SendToReservesMIR $ fromShelleyLovelace amount)

    ShelleyBasedEraBabbage ->
      case cert of
        (Shelley.MirTxCert (Shelley.MIRCert mirpot (Shelley.StakeAddressesMIR amounts))) ->
          Just $ DeprecatedAfterBabbageCerts $ MIRCertificate
            mirpot
            (StakeAddressesMIR
              [ (fromShelleyStakeCredential sc, fromShelleyDeltaLovelace v)
              | (sc, v) <- Map.toList amounts ]
            )
        (Shelley.GenesisDelegTxCert genesiskh delegatekh vrfkh)->
          Just $ DeprecatedAfterBabbageCerts $ GenesisKeyDelegationCertificate
            (GenesisKeyHash genesiskh)
            (GenesisDelegateKeyHash delegatekh)
            (VrfKeyHash vrfkh)
        (Shelley.MirTxCert (Shelley.MIRCert ReservesMIR (Shelley.SendToOppositePotMIR amount))) ->
          Just $ DeprecatedAfterBabbageCerts $ MIRCertificate ReservesMIR (SendToTreasuryMIR $ fromShelleyLovelace amount)
        (Shelley.MirTxCert (Shelley.MIRCert TreasuryMIR (Shelley.SendToOppositePotMIR amount))) ->
          Just $ DeprecatedAfterBabbageCerts $ MIRCertificate TreasuryMIR
                                (SendToReservesMIR $ fromShelleyLovelace amount)


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
