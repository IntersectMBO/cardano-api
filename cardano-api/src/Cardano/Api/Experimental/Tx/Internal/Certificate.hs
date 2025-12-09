{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Api.Experimental.Tx.Internal.Certificate
  ( Certificate (..)

    -- * Registering stake address and delegating
  , makeStakeAddressDelegationCertificate
  , makeStakeAddressRegistrationCertificate
  , makeStakeAddressUnregistrationCertificate

    -- * Registering stake pools
  , makeStakePoolRegistrationCertificate
  , makeStakePoolRetirementCertificate

    -- * Governance related certificates
  , makeCommitteeColdkeyResignationCertificate
  , makeCommitteeHotKeyAuthorizationCertificate
  , makeDrepRegistrationCertificate
  , makeDrepUnregistrationCertificate
  , makeDrepUpdateCertificate
  , makeStakeAddressAndDRepDelegationCertificate

    -- * Anchor data
  , AnchorDataFromCertificateError (..)
  , getAnchorDataFromCertificate

    -- * Data family instances
  , AsType (..)
  )
where

import Cardano.Api.Address
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Error
import Cardano.Api.Experimental.Era
import Cardano.Api.Experimental.Tx.Internal.Certificate.Compatible (Delegatee)
import Cardano.Api.Experimental.Tx.Internal.Certificate.Type
import Cardano.Api.HasTypeProxy
import Cardano.Api.Hash qualified as Api
import Cardano.Api.Internal.Utils
import Cardano.Api.Key.Internal qualified as Api
import Cardano.Api.Ledger.Internal.Reexport qualified as Ledger
import Cardano.Api.Pretty
import Cardano.Api.Serialise.TextEnvelope.Internal

import Cardano.Ledger.BaseTypes (strictMaybe)

import Control.Monad.Except (MonadError (..))
import Data.ByteString (ByteString)
import Data.String (IsString (fromString))

makeStakeAddressDelegationCertificate
  :: forall era
   . IsEra era
  => StakeCredential
  -> Delegatee era
  -> Certificate (LedgerEra era)
makeStakeAddressDelegationCertificate sCred delegatee =
  case useEra @era of
    e@ConwayEra ->
      obtainCommonConstraints e $
        Certificate $
          Ledger.mkDelegTxCert (toShelleyStakeCredential sCred) delegatee
    e@DijkstraEra ->
      obtainCommonConstraints e $
        Certificate $
          Ledger.mkDelegTxCert (toShelleyStakeCredential sCred) delegatee

makeStakeAddressRegistrationCertificate
  :: forall era. IsEra era => StakeCredential -> Ledger.Coin -> Certificate (LedgerEra era)
makeStakeAddressRegistrationCertificate scred deposit =
  obtainCommonConstraints (useEra @era) $
    Certificate $
      Ledger.mkRegDepositTxCert (toShelleyStakeCredential scred) deposit

makeStakeAddressUnregistrationCertificate
  :: forall era. IsEra era => StakeCredential -> Ledger.Coin -> Certificate (LedgerEra era)
makeStakeAddressUnregistrationCertificate scred deposit =
  obtainCommonConstraints (useEra @era) $
    Certificate $
      Ledger.mkUnRegDepositTxCert (toShelleyStakeCredential scred) deposit

makeStakePoolRegistrationCertificate
  :: forall era
   . IsEra era
  => Ledger.StakePoolParams
  -> Certificate (LedgerEra era)
makeStakePoolRegistrationCertificate poolParams =
  obtainCommonConstraints (useEra @era) $
    Certificate $
      Ledger.mkRegPoolTxCert poolParams

makeStakePoolRetirementCertificate
  :: forall era
   . IsShelleyBasedEra era
  => Api.Hash Api.StakePoolKey
  -> Ledger.EpochNo
  -> Certificate (ShelleyLedgerEra era)
makeStakePoolRetirementCertificate poolId retirementEpoch =
  shelleyBasedEraConstraints (shelleyBasedEra @era) $
    Certificate $
      Ledger.mkRetirePoolTxCert (Api.unStakePoolKeyHash poolId) retirementEpoch

makeCommitteeColdkeyResignationCertificate
  :: forall era
   . IsEra era
  => Ledger.Credential Ledger.ColdCommitteeRole
  -> Maybe Ledger.Anchor
  -> Certificate (LedgerEra era)
makeCommitteeColdkeyResignationCertificate coldKeyCred anchor =
  obtainCommonConstraints (useEra @era) $
    Certificate $
      Ledger.mkResignCommitteeColdTxCert
        coldKeyCred
        (noInlineMaybeToStrictMaybe anchor)

makeCommitteeHotKeyAuthorizationCertificate
  :: forall era
   . IsEra era
  => Ledger.Credential Ledger.ColdCommitteeRole
  -> Ledger.Credential Ledger.HotCommitteeRole
  -> Certificate (LedgerEra era)
makeCommitteeHotKeyAuthorizationCertificate coldKeyCredential hotKeyCredential =
  obtainCommonConstraints (useEra @era) $
    Certificate $
      Ledger.mkAuthCommitteeHotKeyTxCert coldKeyCredential hotKeyCredential

makeDrepRegistrationCertificate
  :: forall era
   . IsEra era
  => Ledger.Credential Ledger.DRepRole
  -> Ledger.Coin
  -> Maybe Ledger.Anchor
  -> Certificate (LedgerEra era)
makeDrepRegistrationCertificate vcred deposit anchor =
  obtainCommonConstraints (useEra @era) $
    Certificate $
      Ledger.mkRegDRepTxCert vcred deposit (noInlineMaybeToStrictMaybe anchor)

makeDrepUnregistrationCertificate
  :: forall era
   . IsEra era
  => Ledger.Credential Ledger.DRepRole
  -> Ledger.Coin
  -> Certificate (LedgerEra era)
makeDrepUnregistrationCertificate vcred deposit =
  obtainCommonConstraints (useEra @era) $
    Certificate $
      Ledger.mkUnRegDRepTxCert
        vcred
        deposit

makeDrepUpdateCertificate
  :: forall era
   . IsEra era
  => Ledger.Credential Ledger.DRepRole
  -> Maybe Ledger.Anchor
  -> Certificate (LedgerEra era)
makeDrepUpdateCertificate vcred mAnchor =
  obtainCommonConstraints (useEra @era) $
    Certificate $
      Ledger.mkUpdateDRepTxCert vcred (noInlineMaybeToStrictMaybe mAnchor)

makeStakeAddressAndDRepDelegationCertificate
  :: forall era
   . IsEra era
  => StakeCredential
  -> Ledger.Delegatee
  -> Ledger.Coin
  -> Certificate (LedgerEra era)
makeStakeAddressAndDRepDelegationCertificate cred delegatee deposit =
  obtainCommonConstraints (useEra @era) $
    Certificate $
      Ledger.mkRegDepositDelegTxCert
        (toShelleyStakeCredential cred)
        delegatee
        deposit

-- -------------------------------------

getAnchorDataFromCertificate
  :: Era era
  -> Certificate (LedgerEra era)
  -> Either AnchorDataFromCertificateError (Maybe Ledger.Anchor)
getAnchorDataFromCertificate ConwayEra (Certificate c) =
  case c of
    Ledger.RegTxCert _ -> return Nothing
    Ledger.UnRegTxCert _ -> return Nothing
    Ledger.RegDepositTxCert _ _ -> return Nothing
    Ledger.UnRegDepositTxCert _ _ -> return Nothing
    Ledger.RegDepositDelegTxCert{} -> return Nothing
    Ledger.DelegTxCert{} -> return Nothing
    Ledger.RegPoolTxCert poolParams -> strictMaybe (return Nothing) anchorDataFromPoolMetadata $ Ledger.sppMetadata poolParams
    Ledger.RetirePoolTxCert _ _ -> return Nothing
    Ledger.RegDRepTxCert _ _ mAnchor -> return $ Ledger.strictMaybeToMaybe mAnchor
    Ledger.UnRegDRepTxCert _ _ -> return Nothing
    Ledger.UpdateDRepTxCert _ mAnchor -> return $ Ledger.strictMaybeToMaybe mAnchor
    Ledger.AuthCommitteeHotKeyTxCert _ _ -> return Nothing
    Ledger.ResignCommitteeColdTxCert _ mAnchor -> return $ Ledger.strictMaybeToMaybe mAnchor
    _ -> error "getAnchorDataFromCertificate: Unrecognized cert"
getAnchorDataFromCertificate DijkstraEra (Certificate c) =
  case c of
    Ledger.RegDepositTxCert _ _ -> return Nothing
    Ledger.UnRegDepositTxCert _ _ -> return Nothing
    Ledger.RegDepositDelegTxCert{} -> return Nothing
    Ledger.DelegTxCert{} -> return Nothing
    Ledger.RegPoolTxCert poolParams -> strictMaybe (return Nothing) anchorDataFromPoolMetadata $ Ledger.sppMetadata poolParams
    Ledger.RetirePoolTxCert _ _ -> return Nothing
    Ledger.RegDRepTxCert _ _ mAnchor -> return $ Ledger.strictMaybeToMaybe mAnchor
    Ledger.UnRegDRepTxCert _ _ -> return Nothing
    Ledger.UpdateDRepTxCert _ mAnchor -> return $ Ledger.strictMaybeToMaybe mAnchor
    Ledger.AuthCommitteeHotKeyTxCert _ _ -> return Nothing
    Ledger.ResignCommitteeColdTxCert _ mAnchor -> return $ Ledger.strictMaybeToMaybe mAnchor
    _ -> error "getAnchorDataFromCertificate: Unrecognized cert"

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

data AnchorDataFromCertificateError
  = InvalidPoolMetadataHashError Ledger.Url ByteString
  deriving (Eq, Show)

instance Error AnchorDataFromCertificateError where
  prettyError :: AnchorDataFromCertificateError -> Doc ann
  prettyError (InvalidPoolMetadataHashError url hash) =
    "Invalid pool metadata hash for URL " <> pretty url <> ": " <> fromString (show hash)
