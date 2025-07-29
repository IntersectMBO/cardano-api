{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Wasm.Internal.Api.Wallet
  ( WalletObject (..)
  , generateMainnetPaymentWalletImpl
  , restoreMainnetPaymentWalletFromSigningKeyBech32Impl
  , generateTestnetPaymentWalletImpl
  , restoreTestnetPaymentWalletFromSigningKeyBech32Impl
  , getAddressBech32
  , getBech32ForVerificationKeyImpl
  , getBech32ForSigningKeyImpl
  , getBase16ForVerificationKeyHashImpl
  )
where

import Cardano.Api
  ( AsType (AsPaymentKey)
  , FromJSON
  , Key (..)
  , NetworkId (..)
  , NetworkMagic (..)
  , PaymentCredential (..)
  , PaymentKey
  , StakeAddressReference (..)
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

import Cardano.Crypto.Seed (mkSeedFromBytes)
import Cardano.Wasm.Internal.ExceptionHandling (rightOrError, toMonadFail)
import Cardano.Wasm.Internal.JavaScript.Random (getRandomBytes)

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Generics (Generic)

data WalletObject = PaymentWallet
  { paymentWalletNetworkId :: NetworkId
  , paymentWalletPaymentSigningKey :: SigningKey PaymentKey
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
      other -> fail $ "Unsupported wallet type: " ++ show other

-- | Generate a simple payment wallet for mainnet.
generateMainnetPaymentWalletImpl :: IO WalletObject
generateMainnetPaymentWalletImpl = do
  let seedSize = deterministicSigningKeySeedSize AsPaymentKey
  randomBytes <- getRandomBytes seedSize
  let seed = mkSeedFromBytes randomBytes
      key = deterministicSigningKey AsPaymentKey seed
  return (PaymentWallet Mainnet key)

-- | Restore a mainnet payment wallet from a Bech32 encoded signing key.
restoreMainnetPaymentWalletFromSigningKeyBech32Impl :: String -> IO WalletObject
restoreMainnetPaymentWalletFromSigningKeyBech32Impl signingKeyBech32 = do
  let key = rightOrError $ deserialiseFromBech32 (Text.pack signingKeyBech32)
  PaymentWallet Mainnet <$> toMonadFail key

-- | Generate a simple payment wallet for testnet, given the testnet's network magic.
generateTestnetPaymentWalletImpl :: Int -> IO WalletObject
generateTestnetPaymentWalletImpl networkMagic = do
  let seedSize = deterministicSigningKeySeedSize AsPaymentKey
  randomBytes <- getRandomBytes seedSize
  let seed = mkSeedFromBytes randomBytes
      key = deterministicSigningKey AsPaymentKey seed
  return (PaymentWallet (Testnet (NetworkMagic (fromIntegral networkMagic))) key)

-- | Restore a testnet payment wallet from a Bech32 encoded signing key.
restoreTestnetPaymentWalletFromSigningKeyBech32Impl :: Int -> String -> IO WalletObject
restoreTestnetPaymentWalletFromSigningKeyBech32Impl networkMagic signingKeyHex = do
  let key = rightOrError $ deserialiseFromBech32 (Text.pack signingKeyHex)
  PaymentWallet (Testnet (NetworkMagic (fromIntegral networkMagic))) <$> toMonadFail key

-- | Get the Bech32 representation of the address. (Can be shared for receiving funds.)
getAddressBech32 :: WalletObject -> String
getAddressBech32 (PaymentWallet nid key) =
  Text.unpack $
    serialiseAddress $
      makeShelleyAddress
        nid
        (PaymentCredentialByKey (verificationKeyHash (getVerificationKey key)))
        NoStakeAddress

-- | Get the Bech32 representation of the verification key of the wallet. (Can be shared for verification.)
getBech32ForVerificationKeyImpl :: WalletObject -> String
getBech32ForVerificationKeyImpl (PaymentWallet _ key) =
  Text.unpack $ serialiseToBech32 (getVerificationKey key)

-- | Get the Bech32 representation of the signing key of the wallet. (Must be kept secret.)
getBech32ForSigningKeyImpl :: WalletObject -> String
getBech32ForSigningKeyImpl (PaymentWallet _ key) =
  Text.unpack $ serialiseToBech32 key

-- | Get the base16 representation of the hash of the verification key of the wallet.
getBase16ForVerificationKeyHashImpl :: WalletObject -> String
getBase16ForVerificationKeyHashImpl (PaymentWallet _ key) =
  Text.unpack $
    Text.decodeUtf8 (serialiseToRawBytesHex (verificationKeyHash (getVerificationKey key)))
