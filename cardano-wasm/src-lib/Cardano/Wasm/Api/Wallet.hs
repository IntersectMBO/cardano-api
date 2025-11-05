{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Cardano.Wasm.Api.Wallet
  ( WalletObject (..)
  , generatePaymentWalletImpl
  , generateStakeWalletImpl
  , restorePaymentWalletFromSigningKeyBech32Impl
  , restoreStakeWalletFromSigningKeyBech32Impl
  , generateTestnetPaymentWalletImpl
  , generateTestnetStakeWalletImpl
  , restoreTestnetPaymentWalletFromSigningKeyBech32Impl
  , restoreTestnetStakeWalletFromSigningKeyBech32Impl
  , getAddressBech32Impl
  , getBech32ForPaymentVerificationKeyImpl
  , getBech32ForPaymentSigningKeyImpl
  , getBech32ForStakeVerificationKeyImpl
  , getBech32ForStakeSigningKeyImpl
  , getBase16ForPaymentVerificationKeyHashImpl
  , getBase16ForStakeVerificationKeyHashImpl
  )
where

import Cardano.Api
  ( AsType (AsPaymentKey, AsStakeKey)
  , FromJSON
  , Key (..)
  , NetworkId (..)
  , NetworkMagic (..)
  , PaymentCredential (..)
  , PaymentKey
  , StakeAddressReference (..)
  , StakeKey
  , ToJSON
  , deserialiseFromBech32
  , deserialiseFromRawBytesHex
  , deterministicSigningKey
  , fromNetworkMagic
  , makeShelleyAddress
  , serialiseAddress
  , serialiseToBech32
  , serialiseToRawBytesHex
  , toNetworkMagic
  )
import Cardano.Api.Address (StakeCredential (..))

import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Wasm.ExceptionHandling (rightOrError, toMonadFail)
import Cardano.Wasm.Internal.Api.Random (getRandomBytes)

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Generics (Generic)

data WalletObject
  = PaymentWallet
      { paymentWalletNetworkId :: NetworkId
      , paymentWalletPaymentSigningKey :: SigningKey PaymentKey
      }
  | StakeWallet
      { stakeWalletNetworkId :: NetworkId
      , stakeWalletPaymentSigningKey :: SigningKey PaymentKey
      , stakeWalletStakeSigningKey :: SigningKey StakeKey
      }

deriving instance Show WalletObject

deriving instance Generic WalletObject

instance ToJSON WalletObject where
  toJSON :: WalletObject -> Aeson.Value
  toJSON (PaymentWallet nid key) =
    let NetworkMagic nm = toNetworkMagic nid
     in Aeson.object
          [ "type" .= ("PaymentWallet" :: Text)
          , "networkMagic" .= (fromIntegral nm :: Int)
          , "signingKey" .= Text.decodeUtf8 (serialiseToRawBytesHex key)
          ]
  toJSON (StakeWallet nid pKey sKey) =
    let NetworkMagic nm = toNetworkMagic nid
     in Aeson.object
          [ "type" .= ("StakeWallet" :: Text)
          , "networkMagic" .= (fromIntegral nm :: Int)
          , "paymentSigningKey" .= Text.decodeUtf8 (serialiseToRawBytesHex pKey)
          , "stakeSigningKey" .= Text.decodeUtf8 (serialiseToRawBytesHex sKey)
          ]

instance FromJSON WalletObject where
  parseJSON :: Aeson.Value -> Aeson.Parser WalletObject
  parseJSON = Aeson.withObject "WalletObject" $ \o -> do
    (typ :: Text) <- o Aeson..: "type"
    case typ of
      "PaymentWallet" -> do
        keyHex <- o Aeson..: "signingKey"
        (nm :: Int) <- o Aeson..: "networkMagic"
        PaymentWallet (fromNetworkMagic (NetworkMagic (fromIntegral nm)))
          <$> toMonadFail (rightOrError $ deserialiseFromRawBytesHex (Text.encodeUtf8 keyHex))
      "StakeWallet" -> do
        pKeyHex <- o Aeson..: "paymentSigningKey"
        sKeyHex <- o Aeson..: "stakeSigningKey"
        (nm :: Int) <- o Aeson..: "networkMagic"
        StakeWallet (fromNetworkMagic (NetworkMagic (fromIntegral nm)))
          <$> toMonadFail (rightOrError $ deserialiseFromRawBytesHex (Text.encodeUtf8 pKeyHex))
          <*> toMonadFail (rightOrError $ deserialiseFromRawBytesHex (Text.encodeUtf8 sKeyHex))
      other -> fail $ "Unsupported wallet type: " ++ show other

-- * Wallet mainnet generation

-- | Generate a simple payment wallet for mainnet.
generatePaymentWalletImpl :: IO WalletObject
generatePaymentWalletImpl = do
  let seedSize = deterministicSigningKeySeedSize AsPaymentKey
  randomBytes <- getRandomBytes seedSize
  let seed = mkSeedFromBytes randomBytes
      key = deterministicSigningKey AsPaymentKey seed
  return (PaymentWallet Mainnet key)

-- | Generate a stake wallet for mainnet.
generateStakeWalletImpl :: IO WalletObject
generateStakeWalletImpl = do
  let seedSize = deterministicSigningKeySeedSize AsPaymentKey
  randomBytes1 <- getRandomBytes seedSize
  let seed1 = mkSeedFromBytes randomBytes1
      pKey = deterministicSigningKey AsPaymentKey seed1
  randomBytes2 <- getRandomBytes seedSize
  let seed2 = mkSeedFromBytes randomBytes2
      sKey = deterministicSigningKey AsStakeKey seed2
  return (StakeWallet Mainnet pKey sKey)

-- * Wallet restoration for mainnet.

-- | Restore a mainnet payment wallet from a Bech32 encoded signing key.
restorePaymentWalletFromSigningKeyBech32Impl :: String -> IO WalletObject
restorePaymentWalletFromSigningKeyBech32Impl signingKeyBech32 = do
  key <- rightOrError $ deserialiseFromBech32 (Text.pack signingKeyBech32)
  pure $ PaymentWallet Mainnet key

-- | Restore a mainnet stake wallet from Bech32 encoded signing keys.
restoreStakeWalletFromSigningKeyBech32Impl :: String -> String -> IO WalletObject
restoreStakeWalletFromSigningKeyBech32Impl paymentKeyBech32 stakeKeyBech32 = do
  pKey <- rightOrError $ deserialiseFromBech32 (Text.pack paymentKeyBech32)
  sKey <- rightOrError $ deserialiseFromBech32 (Text.pack stakeKeyBech32)
  pure $ StakeWallet Mainnet pKey sKey

-- * Wallet testnet generation

-- | Generate a simple payment wallet for testnet, given the testnet's network magic.
generateTestnetPaymentWalletImpl :: Int -> IO WalletObject
generateTestnetPaymentWalletImpl networkMagic = do
  let seedSize = deterministicSigningKeySeedSize AsPaymentKey
  randomBytes <- getRandomBytes seedSize
  let seed = mkSeedFromBytes randomBytes
      key = deterministicSigningKey AsPaymentKey seed
  return $ PaymentWallet (Testnet $ NetworkMagic $ fromIntegral networkMagic) key

-- | Generate a stake wallet for testnet, given the testnet's network magic.
generateTestnetStakeWalletImpl :: Int -> IO WalletObject
generateTestnetStakeWalletImpl networkMagic = do
  let seedSize = deterministicSigningKeySeedSize AsPaymentKey
  randomBytes1 <- getRandomBytes seedSize
  let seed1 = mkSeedFromBytes randomBytes1
      pKey = deterministicSigningKey AsPaymentKey seed1
  randomBytes2 <- getRandomBytes seedSize
  let seed2 = mkSeedFromBytes randomBytes2
      sKey = deterministicSigningKey AsStakeKey seed2
  return $ StakeWallet (Testnet $ NetworkMagic $ fromIntegral networkMagic) pKey sKey

-- * Wallet restoration for testnet.

-- | Restore a testnet payment wallet from a Bech32 encoded signing key.
restoreTestnetPaymentWalletFromSigningKeyBech32Impl :: Int -> String -> IO WalletObject
restoreTestnetPaymentWalletFromSigningKeyBech32Impl networkMagic signingKeyHex = do
  key <- rightOrError $ deserialiseFromBech32 (Text.pack signingKeyHex)
  pure $ PaymentWallet (Testnet (NetworkMagic (fromIntegral networkMagic))) key

-- | Restore a testnet stake wallet from Bech32 encoded signing keys.
restoreTestnetStakeWalletFromSigningKeyBech32Impl :: Int -> String -> String -> IO WalletObject
restoreTestnetStakeWalletFromSigningKeyBech32Impl networkMagic paymentKeyBech32 stakeKeyBech32 = do
  pKey <- rightOrError $ deserialiseFromBech32 (Text.pack paymentKeyBech32)
  sKey <- rightOrError $ deserialiseFromBech32 (Text.pack stakeKeyBech32)
  pure $ StakeWallet (Testnet (NetworkMagic (fromIntegral networkMagic))) pKey sKey

-- * Wallet information retrieval

-- ** Bech32 of addresses and keys

-- | Get the Bech32 representation of the address. (Can be shared for receiving funds.)
getAddressBech32Impl :: WalletObject -> String
getAddressBech32Impl (PaymentWallet nid key) =
  Text.unpack $
    serialiseAddress $
      makeShelleyAddress
        nid
        (PaymentCredentialByKey (verificationKeyHash (getVerificationKey key)))
        NoStakeAddress
getAddressBech32Impl (StakeWallet nid pKey sKey) =
  Text.unpack $
    serialiseAddress $
      makeShelleyAddress
        nid
        (PaymentCredentialByKey (verificationKeyHash (getVerificationKey pKey)))
        (StakeAddressByValue (StakeCredentialByKey (verificationKeyHash (getVerificationKey sKey))))

-- | Get the Bech32 representation of the verification key of the wallet. (Can be shared for verification.)
getBech32ForPaymentVerificationKeyImpl :: WalletObject -> String
getBech32ForPaymentVerificationKeyImpl (PaymentWallet _ key) =
  Text.unpack $ serialiseToBech32 (getVerificationKey key)
getBech32ForPaymentVerificationKeyImpl (StakeWallet _ pKey _) =
  Text.unpack $ serialiseToBech32 (getVerificationKey pKey)

-- | Get the Bech32 representation of the stake verification key of the wallet. (Can be shared for verification.)
getBech32ForStakeVerificationKeyImpl :: WalletObject -> String
getBech32ForStakeVerificationKeyImpl (PaymentWallet _ _) =
  error "Payment wallets do not have stake keys"
getBech32ForStakeVerificationKeyImpl (StakeWallet _ _ sKey) =
  Text.unpack $ serialiseToBech32 (getVerificationKey sKey)

-- | Get the Bech32 representation of the signing key of the wallet, if any. (Must be kept secret.)
getBech32ForPaymentSigningKeyImpl :: WalletObject -> String
getBech32ForPaymentSigningKeyImpl (PaymentWallet _ pkey) =
  Text.unpack $ serialiseToBech32 pkey
getBech32ForPaymentSigningKeyImpl (StakeWallet _ pKey _) =
  Text.unpack $ serialiseToBech32 pKey

-- | Get the Bech32 representation of the stake signing key of the wallet, if any. (Must be kept secret.)
getBech32ForStakeSigningKeyImpl :: WalletObject -> String
getBech32ForStakeSigningKeyImpl (PaymentWallet _ _) =
  error "Payment wallets do not have stake keys"
getBech32ForStakeSigningKeyImpl (StakeWallet _ _ sKey) =
  Text.unpack $ serialiseToBech32 sKey

-- ** Base16 of key hashes

-- | Get the base16 representation of the hash of the verification key of the wallet.
getBase16ForPaymentVerificationKeyHashImpl :: WalletObject -> String
getBase16ForPaymentVerificationKeyHashImpl (PaymentWallet _ pkey) =
  Text.unpack $
    Text.decodeUtf8 (serialiseToRawBytesHex (verificationKeyHash (getVerificationKey pkey)))
getBase16ForPaymentVerificationKeyHashImpl (StakeWallet _ pKey _) =
  Text.unpack $
    Text.decodeUtf8 (serialiseToRawBytesHex (verificationKeyHash (getVerificationKey pKey)))

-- | Get the base16 representation of the hash of the stake verification key of the wallet.
getBase16ForStakeVerificationKeyHashImpl :: WalletObject -> String
getBase16ForStakeVerificationKeyHashImpl (PaymentWallet _ _) =
  error "Payment wallets do not have stake keys"
getBase16ForStakeVerificationKeyHashImpl (StakeWallet _ _ sKey) =
  Text.unpack $
    Text.decodeUtf8 (serialiseToRawBytesHex (verificationKeyHash (getVerificationKey sKey)))
