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
import Cardano.Wasm.ExceptionHandling (rightOrError)

import Control.Monad.Catch (MonadThrow)
import Data.ByteString.Base16 qualified as Base16
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

-- | Make a certificate that delegates a stake address to a stake pool in Conway era.
makeStakeAddressStakeDelegationCertificateImpl :: MonadThrow m => String -> String -> m String
makeStakeAddressStakeDelegationCertificateImpl skHashStr poolIdStr = do
  stakeCertHash <- readHash skHashStr
  poolId <- readPoolId poolIdStr
  makeStakeAddressStakeDelegationCertificate Exp.ConwayEra stakeCertHash poolId

-- | Make a certificate that delegates a stake address to a stake pool in the current experimental era.
makeStakeAddressStakeDelegationCertificateExperimentalEraImpl
  :: MonadThrow m => String -> String -> m String
makeStakeAddressStakeDelegationCertificateExperimentalEraImpl skHashStr poolIdStr = do
  stakeCertHash <- readHash skHashStr
  poolId <- readPoolId poolIdStr
  makeStakeAddressStakeDelegationCertificate Exp.DijkstraEra stakeCertHash poolId

makeStakeAddressStakeDelegationCertificate
  :: forall era m. MonadThrow m => Exp.Era era -> Hash StakeKey -> PoolId -> m String
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

-- | Make a stake address registration certificate in Conway era.
makeStakeAddressRegistrationCertificateImpl :: MonadThrow m => String -> Integer -> m String
makeStakeAddressRegistrationCertificateImpl skHashStr deposit = do
  skHash <- readHash skHashStr
  makeStakeAddressRegistrationCertificateWrapper Exp.ConwayEra skHash deposit

--  | Make a stake address registration certificate in the current experimental era.
makeStakeAddressRegistrationCertificateExperimentalEraImpl
  :: MonadThrow m => String -> Integer -> m String
makeStakeAddressRegistrationCertificateExperimentalEraImpl skHashStr deposit = do
  skHash <- readHash skHashStr
  makeStakeAddressRegistrationCertificateWrapper Exp.DijkstraEra skHash deposit

makeStakeAddressRegistrationCertificateWrapper
  :: forall era m. MonadThrow m => Era era -> Hash StakeKey -> Integer -> m String
makeStakeAddressRegistrationCertificateWrapper era skHash deposit =
  obtainCommonConstraints era $ do
    let cert :: Certificate (Exp.LedgerEra era) =
          Exp.makeStakeAddressRegistrationCertificate
            (StakeCredentialByKey skHash)
            (Coin deposit)
    return $ serialiseCertificateToCBOR era cert

-- | Make a stake address unregistration certificate in Conway era.
makeStakeAddressUnregistrationCertificateImpl :: MonadThrow m => String -> Integer -> m String
makeStakeAddressUnregistrationCertificateImpl skHashStr deposit = do
  skHash <- readHash skHashStr
  makeStakeAddressUnregistrationCertificateWrapper Exp.ConwayEra skHash deposit

-- | Make a stake address unregistration certificate in the current experimental era.
makeStakeAddressUnregistrationCertificateExperimentalEraImpl
  :: MonadThrow m => String -> Integer -> m String
makeStakeAddressUnregistrationCertificateExperimentalEraImpl skHashStr deposit = do
  skHash <- readHash skHashStr
  makeStakeAddressUnregistrationCertificateWrapper Exp.DijkstraEra skHash deposit

makeStakeAddressUnregistrationCertificateWrapper
  :: forall era m. MonadThrow m => Era era -> Hash StakeKey -> Integer -> m String
makeStakeAddressUnregistrationCertificateWrapper era skHash deposit =
  obtainCommonConstraints era $ do
    let cert :: Certificate (Exp.LedgerEra era) =
          Exp.makeStakeAddressUnregistrationCertificate
            (StakeCredentialByKey skHash)
            (Coin deposit)
    return $ serialiseCertificateToCBOR era cert

serialiseCertificateToCBOR :: Exp.Era era -> Certificate (Exp.LedgerEra era) -> String
serialiseCertificateToCBOR era cert =
  obtainCommonConstraints era $ do
    Text.unpack $
      Text.decodeUtf8 $
        Base16.encode $
          serialiseToCBOR
            cert

readHash :: MonadThrow m => String -> m (Hash StakeKey)
readHash = rightOrError . Api.deserialiseFromRawBytesHex . Text.encodeUtf8 . Text.pack

readPoolId :: MonadThrow m => String -> m PoolId
readPoolId = rightOrError . Api.deserialiseFromRawBytesHex . Text.encodeUtf8 . Text.pack
