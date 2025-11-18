{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wasm.Api.Certificate.StakeCertificate
  ( makeStakeAddressStakeDelegationCertificateImpl
  , makeStakeAddressStakeDelegationCertificateExperimentalEraImpl
  , makeStakeAddressRegistrationCertificateImpl
  , makeStakeAddressRegistrationCertificateExperimentalEraImpl
  , makeStakeAddressUnregistrationCertificateImpl
  , makeStakeAddressUnregistrationCertificateExperimentalEraImpl
  )
where

import Cardano.Api
  ( Coin (..)
  , Hash
  , PoolId
  , StakeKey
  , serialiseToCBOR
  , unStakePoolKeyHash
  )
import Cardano.Api.Address (StakeCredential (..))
import Cardano.Api.Experimental (Era (..), obtainCommonConstraints)
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.Certificate (Certificate (..))
import Cardano.Api.Serialise.Raw qualified as Api

import Cardano.Ledger.Api (Delegatee (DelegStake))
import Cardano.Wasm.ExceptionHandling (justOrError, rightOrError)
import Cardano.Wasm.Internal.Api.Era (currentEra, experimentalEra)

import Control.Monad.Catch (MonadThrow)
import Data.ByteString.Base16 qualified as Base16
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

-- * Type aliases for clarity

-- | A stake key hash represented as a base16-encoded string.
type StakeKeyHashBase16 = String

-- | A pool ID represented as a base16-encoded string.
type PoolIdBase16 = String

-- | Deposit amount in lovelace.
type DepositLovelace = Integer

-- | Certificate serialized to CBOR as a base16-encoded string.
type CertificateCBORBase16 = String

-- * Stake Certificate function implementation

-- | Make a certificate that delegates a stake address to a stake pool in the current era.
makeStakeAddressStakeDelegationCertificateImpl
  :: MonadThrow m => StakeKeyHashBase16 -> PoolIdBase16 -> m CertificateCBORBase16
makeStakeAddressStakeDelegationCertificateImpl skHashStr poolIdStr = do
  stakeCertHash <- readHash skHashStr
  poolId <- readPoolId poolIdStr
  makeStakeAddressStakeDelegationCertificate currentEra stakeCertHash poolId

-- | Make a certificate that delegates a stake address to a stake pool in the current experimental era.
makeStakeAddressStakeDelegationCertificateExperimentalEraImpl
  :: MonadThrow m => StakeKeyHashBase16 -> PoolIdBase16 -> m CertificateCBORBase16
makeStakeAddressStakeDelegationCertificateExperimentalEraImpl skHashStr poolIdStr = do
  stakeCertHash <- readHash skHashStr
  poolId <- readPoolId poolIdStr
  era <- justOrError "No experimental era available" experimentalEra
  makeStakeAddressStakeDelegationCertificate era stakeCertHash poolId

makeStakeAddressStakeDelegationCertificate
  :: forall era m. MonadThrow m => Exp.Era era -> Hash StakeKey -> PoolId -> m CertificateCBORBase16
makeStakeAddressStakeDelegationCertificate era stakeCertHash poolId =
  obtainCommonConstraints era $ do
    let cert :: Certificate (Exp.LedgerEra era) =
          Exp.makeStakeAddressDelegationCertificate
            (StakeCredentialByKey stakeCertHash)
            ( case era of
                ConwayEra -> DelegStake $ unStakePoolKeyHash poolId
                DijkstraEra -> DelegStake $ unStakePoolKeyHash poolId
            )
    return $ serialiseCertificateToCBOR era cert

-- | Make a stake address registration certificate in the current era.
makeStakeAddressRegistrationCertificateImpl
  :: MonadThrow m => StakeKeyHashBase16 -> DepositLovelace -> m CertificateCBORBase16
makeStakeAddressRegistrationCertificateImpl skHashStr deposit = do
  skHash <- readHash skHashStr
  makeStakeAddressRegistrationCertificateWrapper currentEra skHash deposit

--  | Make a stake address registration certificate in the current experimental era.
makeStakeAddressRegistrationCertificateExperimentalEraImpl
  :: MonadThrow m => StakeKeyHashBase16 -> DepositLovelace -> m CertificateCBORBase16
makeStakeAddressRegistrationCertificateExperimentalEraImpl skHashStr deposit = do
  skHash <- readHash skHashStr
  era <- justOrError "No experimental era available" experimentalEra
  makeStakeAddressRegistrationCertificateWrapper era skHash deposit

makeStakeAddressRegistrationCertificateWrapper
  :: forall era m. MonadThrow m => Era era -> Hash StakeKey -> DepositLovelace -> m CertificateCBORBase16
makeStakeAddressRegistrationCertificateWrapper era skHash deposit =
  obtainCommonConstraints era $ do
    let cert :: Certificate (Exp.LedgerEra era) =
          Exp.makeStakeAddressRegistrationCertificate
            (StakeCredentialByKey skHash)
            (Coin deposit)
    return $ serialiseCertificateToCBOR era cert

-- | Make a stake address unregistration certificate in the current era.
makeStakeAddressUnregistrationCertificateImpl
  :: MonadThrow m => StakeKeyHashBase16 -> DepositLovelace -> m CertificateCBORBase16
makeStakeAddressUnregistrationCertificateImpl skHashStr deposit = do
  skHash <- readHash skHashStr
  makeStakeAddressUnregistrationCertificateWrapper currentEra skHash deposit

-- | Make a stake address unregistration certificate in the current experimental era.
makeStakeAddressUnregistrationCertificateExperimentalEraImpl
  :: MonadThrow m => StakeKeyHashBase16 -> DepositLovelace -> m CertificateCBORBase16
makeStakeAddressUnregistrationCertificateExperimentalEraImpl skHashStr deposit = do
  skHash <- readHash skHashStr
  era <- justOrError "No experimental era available" experimentalEra
  makeStakeAddressUnregistrationCertificateWrapper era skHash deposit

makeStakeAddressUnregistrationCertificateWrapper
  :: forall era m. MonadThrow m => Era era -> Hash StakeKey -> DepositLovelace -> m CertificateCBORBase16
makeStakeAddressUnregistrationCertificateWrapper era skHash deposit =
  obtainCommonConstraints era $ do
    let cert :: Certificate (Exp.LedgerEra era) =
          Exp.makeStakeAddressUnregistrationCertificate
            (StakeCredentialByKey skHash)
            (Coin deposit)
    return $ serialiseCertificateToCBOR era cert

serialiseCertificateToCBOR
  :: Exp.Era era -> Certificate (Exp.LedgerEra era) -> CertificateCBORBase16
serialiseCertificateToCBOR era cert =
  obtainCommonConstraints era $ do
    Text.unpack $
      Text.decodeUtf8 $
        Base16.encode $
          serialiseToCBOR
            cert

readHash :: MonadThrow m => StakeKeyHashBase16 -> m (Hash StakeKey)
readHash = rightOrError . Api.deserialiseFromRawBytesHex . Text.encodeUtf8 . Text.pack

readPoolId :: MonadThrow m => PoolIdBase16 -> m PoolId
readPoolId = rightOrError . Api.deserialiseFromRawBytesHex . Text.encodeUtf8 . Text.pack
