{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Wasm.Api.Certificate.StakeCertificate
  ( StakeCertificateObject (..)
  , createStakeKeyCertificate
  , asStakeRegistration
  , asStakeUnregistration
  , asDelegateOnly
  , withDeposit
  , withoutDeposit
  , withDelegation
  , withoutDelegation
  )
where

import Cardano.Api (Coin, Hash, PoolId, StakeKey, ToJSON (..))
import Cardano.Api.Experimental (Era (..), obtainCommonConstraints)
import Cardano.Api.Experimental qualified as Exp
import Cardano.Api.Serialise.Raw qualified as Api

import Cardano.Wasm.ExceptionHandling (rightOrError, throwError, toMonadFail)

import Data.Aeson (FromJSON, (.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Text (Text)
import Data.Text.Encoding qualified as Text

data StakeCertificateAction
  = RegisterStake
  | UnregisterStake
  | DelegateOnly
  deriving (Show, Eq)

data Delegation
  = NoDelegation
  | DelegateToPool PoolId
  -- ToDo: Add DRep delegation
  deriving (Show, Eq)

data StakeCertificateObject
  = forall era. StakeCertificateObject
  { era :: !(Era era)
  , stakeCredential :: !(Hash StakeKey) -- ToDo: Generalize to support scripts as well
  , deposit :: !(Maybe Coin)
  , action :: !StakeCertificateAction
  , delegation :: Delegation
  }

deriving instance Show StakeCertificateObject

instance ToJSON StakeCertificateObject where
  toJSON (StakeCertificateObject{era, stakeCredential, deposit, action, delegation}) =
    obtainCommonConstraints era $
      Aeson.object
        [ "era" .= Exp.Some era
        , "stakeCredential" .= Text.decodeUtf8 (Api.serialiseToRawBytesHex stakeCredential)
        , "deposit" .= deposit
        , "action" .= case action of
            RegisterStake -> Aeson.String "RegisterStake"
            UnregisterStake -> Aeson.String "UnregisterStake"
            DelegateOnly -> Aeson.String "DelegateOnly"
        , "delegation" .= case delegation of
            NoDelegation -> Aeson.String "NoDelegation"
            DelegateToPool pid -> Aeson.object ["DelegateToPool" .= show pid]
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
    delegationVal <- o .: "delegation"
    delegation <-
      case delegationVal of
        Aeson.String "NoDelegation" -> return NoDelegation
        Aeson.Object obj -> do
          pidStr :: Text <- obj .: "DelegateToPool"
          DelegateToPool
            <$> toMonadFail (rightOrError $ Api.deserialiseFromRawBytesHex $ Text.encodeUtf8 pidStr)
        _ ->
          toMonadFail $ throwError ("Invalid delegation for StakeCertificateObject: " ++ show delegationVal)
    obtainCommonConstraints era $
      return $
        StakeCertificateObject
          { era
          , stakeCredential
          , deposit
          , action
          , delegation
          }

-- | Creates an empty stake certificate object for the given stake key hash.
-- For the certificate to be valid must be either a registration, an unregistration or
-- a delegation certificate. But it can be both registration and delegation.
createStakeKeyCertificate :: Hash StakeKey -> StakeCertificateObject
createStakeKeyCertificate skHash =
  StakeCertificateObject
    { era = ConwayEra
    , stakeCredential = skHash
    , deposit = Nothing
    , action = DelegateOnly
    , delegation = NoDelegation
    }

-- | Marks the certificate as a stake registration certificate.
asStakeRegistration :: StakeCertificateObject -> StakeCertificateObject
asStakeRegistration certObj =
  certObj{action = RegisterStake}

-- | Marks the certificate as a stake un-registration certificate.
asStakeUnregistration :: StakeCertificateObject -> StakeCertificateObject
asStakeUnregistration certObj =
  certObj{action = UnregisterStake}

-- | Marks the certificate as a delegation-only certificate (not registration nor un-registration).
asDelegateOnly :: StakeCertificateObject -> StakeCertificateObject
asDelegateOnly certObj =
  certObj{action = DelegateOnly}

-- | Sets the deposit for the stake certificate. This only has effect for stake registration
-- and unregistration certificates. The amount must match the expected deposit amount specified by
-- 'ppKeyDepositL' in the protocol parameters for registration certificates and the amount
-- depositted for unregistration certificates.
withDeposit :: Coin -> StakeCertificateObject -> StakeCertificateObject
withDeposit dep certObj =
  certObj{deposit = Just dep}

-- | Resets the deposit for the stake certificate.
withoutDeposit :: StakeCertificateObject -> StakeCertificateObject
withoutDeposit certObj =
  certObj{deposit = Nothing}

-- | Sets the pool to which the stake key will be delegated.
withDelegation :: PoolId -> StakeCertificateObject -> StakeCertificateObject
withDelegation poolId certObj =
  certObj{delegation = DelegateToPool poolId}

-- | Resets the delegation for the stake certificate.
withoutDelegation :: StakeCertificateObject -> StakeCertificateObject
withoutDelegation certObj =
  certObj{delegation = NoDelegation}
