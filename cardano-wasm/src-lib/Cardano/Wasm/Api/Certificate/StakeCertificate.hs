{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Wasm.Api.Certificate.StakeCertificate
  ( StakeCertificateObject (..)
  , createStakeKeyCertificateImpl
  , asStakeRegistrationImpl
  , asStakeUnregistrationImpl
  , asDelegateOnlyImpl
  , withDepositImpl
  , withoutDepositImpl
  , withDelegationImpl
  , withoutDelegationImpl
  , toCborImpl
  )
where

import Cardano.Api
  ( Coin
  , Hash
  , PoolId
  , StakeKey
  , ToJSON (..)
  , convert
  , conwayEraOnwardsConstraints
  , serialiseToCBOR
  , unStakeKeyHash
  , unStakePoolKeyHash
  )
import Cardano.Api.Experimental (Era (..), obtainCommonConstraints)
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Experimental.Certificate (Certificate (..))
import Cardano.Api.Ledger
  ( ConwayDelegCert (..)
  , ConwayTxCert (..)
  , Credential (..)
  , maybeToStrictMaybe
  )
import Cardano.Api.Serialise.Raw qualified as Api

import Cardano.Ledger.Api (Delegatee (..))
import Cardano.Wasm.ExceptionHandling (rightOrError, throwError, toMonadFail)

import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString.Base16 qualified as Base16
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

data StakeCertificateAction
  = RegisterStake
  | UnregisterStake
  | DelegateOnly
  deriving (Show, Eq)

data StakeCertificateObject
  = forall era. StakeCertificateObject
  { era :: !(Exp.Era era)
  , stakeCredential :: !(Hash StakeKey) -- ToDo: Generalize to support scripts as well
  , deposit :: !(Maybe Coin)
  , action :: !StakeCertificateAction
  , delegateStake :: Maybe PoolId
  -- ToDo: Add DRep delegation
  }

deriving instance Show StakeCertificateObject

instance ToJSON StakeCertificateObject where
  toJSON (StakeCertificateObject{era, stakeCredential, deposit, action, delegateStake}) =
    obtainCommonConstraints era $
      Aeson.object
        [ "era" .= Exp.Some era
        , "stakeCredential" .= Text.decodeUtf8 (Api.serialiseToRawBytesHex stakeCredential)
        , "deposit" .= deposit
        , "action" .= case action of
            RegisterStake -> Aeson.String "RegisterStake"
            UnregisterStake -> Aeson.String "UnregisterStake"
            DelegateOnly -> Aeson.String "DelegateOnly"
        , "delegateStake" .= fmap (Text.decodeUtf8 . Api.serialiseToRawBytesHex) delegateStake
        ]

instance FromJSON StakeCertificateObject where
  parseJSON = Aeson.withObject "StakeCertificateObject" $ \o -> do
    Exp.Some era <- o .: "era"
    skHashText :: Text <- o .: "stakeCredential"
    stakeCredential :: Hash StakeKey <-
      toMonadFail $
        rightOrError $
          Api.deserialiseFromRawBytesHex (Text.encodeUtf8 skHashText)
    deposit :: Maybe Coin <- o .: "deposit"
    actionStr :: Text <- o .: "action"
    action <-
      case actionStr of
        "RegisterStake" -> return RegisterStake
        "UnregisterStake" -> return UnregisterStake
        "DelegateOnly" -> return DelegateOnly
        _ -> toMonadFail $ throwError ("Invalid action for StakeCertificateObject: " ++ show actionStr)
    delegateStakeText :: Maybe Text <- o .: "delegateStake"
    delegateStake :: Maybe PoolId <-
      traverse
        ( toMonadFail
            . rightOrError
            . Api.deserialiseFromRawBytesHex
            . Text.encodeUtf8
        )
        delegateStakeText
    obtainCommonConstraints era $
      return $
        StakeCertificateObject
          { era
          , stakeCredential
          , deposit
          , action
          , delegateStake
          }

-- | Creates an empty stake certificate object for the given stake key hash.
-- For the certificate to be valid must be either a registration, an unregistration or
-- a delegation certificate. But it can be both registration and delegation.
createStakeKeyCertificateImpl :: Hash StakeKey -> StakeCertificateObject
createStakeKeyCertificateImpl skHash =
  StakeCertificateObject
    { era = ConwayEra
    , stakeCredential = skHash
    , deposit = Nothing
    , action = DelegateOnly
    , delegateStake = Nothing
    }

-- | Marks the certificate as a stake registration certificate.
asStakeRegistrationImpl :: StakeCertificateObject -> StakeCertificateObject
asStakeRegistrationImpl certObj =
  certObj{action = RegisterStake}

-- | Marks the certificate as a stake un-registration certificate.
asStakeUnregistrationImpl :: StakeCertificateObject -> StakeCertificateObject
asStakeUnregistrationImpl certObj =
  certObj{action = UnregisterStake}

-- | Marks the certificate as a delegation-only certificate (not registration nor un-registration).
asDelegateOnlyImpl :: StakeCertificateObject -> StakeCertificateObject
asDelegateOnlyImpl certObj =
  certObj{action = DelegateOnly}

-- | Sets the deposit for the stake certificate. This only has effect for stake registration
-- and unregistration certificates. The amount must match the expected deposit amount specified by
-- 'ppKeyDepositL' in the protocol parameters for registration certificates and the amount
-- depositted for unregistration certificates.
withDepositImpl :: Coin -> StakeCertificateObject -> StakeCertificateObject
withDepositImpl dep certObj =
  certObj{deposit = Just dep}

-- | Resets the deposit for the stake certificate.
withoutDepositImpl :: StakeCertificateObject -> StakeCertificateObject
withoutDepositImpl certObj =
  certObj{deposit = Nothing}

-- | Sets the pool to which the stake key will be delegated.
withDelegationImpl :: PoolId -> StakeCertificateObject -> StakeCertificateObject
withDelegationImpl poolId certObj =
  certObj{delegateStake = Just poolId}

-- | Resets the delegation for the stake certificate.
withoutDelegationImpl :: StakeCertificateObject -> StakeCertificateObject
withoutDelegationImpl certObj =
  certObj{delegateStake = Nothing}

-- | Convert a StakeCertificateObject to the base16 encoding of its CBOR representation.
toCborImpl :: MonadThrow m => StakeCertificateObject -> m String
toCborImpl
  ( StakeCertificateObject
      { era
      , stakeCredential
      , deposit
      , action
      , delegateStake
      }
    ) = do
    stakeCert <- toCardanoApiCertificate era stakeCredential deposit action delegateStake
    return $
      obtainCommonConstraints era $
        Text.unpack $
          Text.decodeUtf8 $
            Base16.encode $
              serialiseToCBOR
                stakeCert

toCardanoApiCertificate
  :: MonadThrow m
  => Exp.Era era
  -> Hash StakeKey
  -> Maybe Coin
  -> StakeCertificateAction
  -> Maybe PoolId
  -> m (Certificate (Exp.LedgerEra era))
toCardanoApiCertificate era stakeCredential deposit action delegateStake =
  Exp.obtainCommonConstraints era $
    conwayEraOnwardsConstraints (convert era) $
      Certificate . ConwayTxCertDeleg
        <$> ( case (action, delegateStake) of
                (DelegateOnly, Nothing) ->
                  throwError
                    "Certificate must at least either: register, unregister, or delegate"
                (RegisterStake, Nothing) ->
                  return $ ConwayRegCert (KeyHashObj $ unStakeKeyHash stakeCredential) (maybeToStrictMaybe deposit)
                (UnregisterStake, Nothing) ->
                  return $ ConwayUnRegCert (KeyHashObj $ unStakeKeyHash stakeCredential) (maybeToStrictMaybe deposit)
                (DelegateOnly, Just poolId) ->
                  return $
                    ConwayDelegCert
                      (KeyHashObj $ unStakeKeyHash stakeCredential)
                      (DelegStake $ unStakePoolKeyHash poolId)
                (RegisterStake, Just poolId) ->
                  ConwayRegDelegCert
                    (KeyHashObj $ unStakeKeyHash stakeCredential)
                    (DelegStake $ unStakePoolKeyHash poolId)
                    <$> case deposit of
                      Just dep -> return dep
                      Nothing -> throwError "Deposit must be specified for stake registration and delegation certificate"
                (UnregisterStake, Just _) ->
                  throwError "Cannot unregister and delegate in the same certificate"
            )
