{-# LANGUAGE DataKinds #-}
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
  ( Credential (..)
  , KeyRole (Staking)
  , ShelleyEraTxCert (mkUnRegTxCert)
  , mkDelegTxCert
  , mkRegDepositDelegTxCert
  , mkRegDepositTxCert
  , mkRegTxCert
  , mkUnRegDepositTxCert
  , mkUnRegTxCert
  )
import Cardano.Api.Serialise.Raw qualified as Api

import Cardano.Ledger.Api (ConwayEraTxCert, Delegatee (..), EraTxCert (..))
import Cardano.Wasm.ExceptionHandling (rightOrError, throwError, toMonadFail)

import Control.Monad.Catch (MonadThrow)
import Data.Aeson (FromJSON, (.:), (.:?), (.=))
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
  , mDeposit :: !(Maybe Coin)
  , action :: !StakeCertificateAction
  , mDelegatee :: Maybe Delegatee
  -- ToDo: Add DRep delegation
  }

deriving instance Show StakeCertificateObject

instance ToJSON StakeCertificateObject where
  toJSON (StakeCertificateObject{era, stakeCredential, mDeposit, action, mDelegatee}) =
    obtainCommonConstraints era $
      Aeson.object
        [ "era" .= Exp.Some era
        , "stakeCredential" .= Text.decodeUtf8 (Api.serialiseToRawBytesHex stakeCredential)
        , "deposit" .= mDeposit
        , "action" .= case action of
            RegisterStake -> Aeson.String "RegisterStake"
            UnregisterStake -> Aeson.String "UnregisterStake"
            DelegateOnly -> Aeson.String "DelegateOnly"
        , "delegateStake" .= mDelegatee
        ]

instance FromJSON StakeCertificateObject where
  parseJSON = Aeson.withObject "StakeCertificateObject" $ \o -> do
    Exp.Some era <- o .: "era"
    skHashText :: Text <- o .: "stakeCredential"
    stakeCredential :: Hash StakeKey <-
      toMonadFail $
        rightOrError $
          Api.deserialiseFromRawBytesHex (Text.encodeUtf8 skHashText)
    mDeposit :: Maybe Coin <- o .:? "deposit"
    actionStr :: Text <- o .: "action"
    action <-
      case actionStr of
        "RegisterStake" -> return RegisterStake
        "UnregisterStake" -> return UnregisterStake
        "DelegateOnly" -> return DelegateOnly
        _ -> toMonadFail $ throwError ("Invalid action for StakeCertificateObject: " ++ show actionStr)
    mDelegatee :: Maybe Delegatee <- o .:? "delegateStake"
    obtainCommonConstraints era $
      return $
        StakeCertificateObject
          { era
          , stakeCredential
          , mDeposit
          , action
          , mDelegatee
          }

-- | Creates an empty stake certificate object for the given stake key base16 hash.
-- For the certificate to be valid must be either a registration, an unregistration or
-- a delegation certificate. But it can be both registration and delegation.
createStakeKeyCertificateImpl :: MonadThrow m => String -> m StakeCertificateObject
createStakeKeyCertificateImpl skHashStr = do
  skHash <- readHash skHashStr
  return $
    StakeCertificateObject
      { era = ConwayEra
      , stakeCredential = skHash
      , mDeposit = Nothing
      , action = DelegateOnly
      , mDelegatee = Nothing
      }
 where
  readHash :: MonadThrow m => String -> m (Hash StakeKey)
  readHash = rightOrError . Api.deserialiseFromRawBytesHex . Text.encodeUtf8 . Text.pack

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
  certObj{mDeposit = Just dep}

-- | Resets the deposit for the stake certificate.
withoutDepositImpl :: StakeCertificateObject -> StakeCertificateObject
withoutDepositImpl certObj =
  certObj{mDeposit = Nothing}

-- | Sets the pool to which the stake key will be delegated.
withDelegationImpl :: PoolId -> StakeCertificateObject -> StakeCertificateObject
withDelegationImpl poolId certObj =
  certObj{mDelegatee = Just (DelegStake $ unStakePoolKeyHash poolId)}

-- | Resets the delegation for the stake certificate.
withoutDelegationImpl :: StakeCertificateObject -> StakeCertificateObject
withoutDelegationImpl certObj =
  certObj{mDelegatee = Nothing}

-- | Convert a StakeCertificateObject to the base16 encoding of its CBOR representation.
toCborImpl :: MonadThrow m => StakeCertificateObject -> m String
toCborImpl
  ( StakeCertificateObject
      { era
      , stakeCredential
      , mDeposit
      , action
      , mDelegatee
      }
    ) = do
    stakeCert <- toCardanoApiCertificate era stakeCredential mDeposit action mDelegatee
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
  -> Maybe Delegatee
  -> m (Certificate (Exp.LedgerEra era))
toCardanoApiCertificate era stakeCredential mDeposit action mDelegatee =
  Exp.obtainCommonConstraints era $
    conwayEraOnwardsConstraints (convert era) $
      Certificate
        <$> ( case (action, mDelegatee) of
                (DelegateOnly, Nothing) ->
                  throwError
                    "Certificate must at least either: register, unregister, or delegate"
                (RegisterStake, Nothing) -> makeRegUnregCertWithMaybeDeposit era mkRegDepositTxCert mkRegTxCert stakeCredential mDeposit
                (UnregisterStake, Nothing) -> makeRegUnregCertWithMaybeDeposit era mkUnRegDepositTxCert mkUnRegTxCert stakeCredential mDeposit
                (DelegateOnly, Just delegatee) ->
                  return $
                    mkDelegTxCert
                      (KeyHashObj $ unStakeKeyHash stakeCredential)
                      delegatee
                (RegisterStake, Just delegatee) ->
                  mkRegDepositDelegTxCert
                    (KeyHashObj $ unStakeKeyHash stakeCredential)
                    delegatee
                    <$> case mDeposit of
                      Just dep -> return dep
                      Nothing -> throwError "Deposit must be specified for stake registration and delegation certificate"
                (UnregisterStake, Just _) ->
                  throwError "Cannot unregister and delegate in the same certificate"
            )
 where
  makeRegUnregCertWithMaybeDeposit
    :: (Exp.EraCommonConstraints era, MonadThrow m)
    => Era era
    -> ( forall era2
          . ConwayEraTxCert era2
         => Credential Staking -> Coin -> TxCert era2
       )
    -> ( forall era2
          . ShelleyEraTxCert era2
         => Credential Staking -> TxCert era2
       )
    -> Hash StakeKey
    -> Maybe Coin
    -> m (TxCert (Exp.LedgerEra era))
  makeRegUnregCertWithMaybeDeposit _era' makeRegUnregDeposit _makeRegUnregNoDeposit stakeCredential' (Just deposit) =
    return $
      makeRegUnregDeposit (KeyHashObj $ unStakeKeyHash stakeCredential') deposit
  makeRegUnregCertWithMaybeDeposit era' _makeRegUnregDeposit makeRegUnregNoDeposit stakeCredential' Nothing =
    case era' of
      Exp.ConwayEra ->
        return $
          makeRegUnregNoDeposit (KeyHashObj $ unStakeKeyHash stakeCredential')
      Exp.DijkstraEra -> throwError "Deposit must be specified for stake registration/unregistration in Dijkstra era"
