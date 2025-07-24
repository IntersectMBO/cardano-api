{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Wasm.Internal.Api.Address where

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
  , deserialiseFromRawBytesHex
  , fromNetworkMagic
  , generateSigningKey
  , makeShelleyAddress
  , serialiseAddress
  , serialiseToBech32
  , serialiseToRawBytesHex
  , toNetworkMagic
  )

import Cardano.Wasm.Internal.ExceptionHandling (rightOrError, toMonadFail)

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import GHC.Generics (Generic)

data AddressObject = PaymentAddress
  { paymentAddressNetworkId :: NetworkId
  , paymentAddressPaymentSigningKey :: SigningKey PaymentKey
  }

deriving instance Show AddressObject

deriving instance Generic AddressObject

instance ToJSON AddressObject where
  toJSON :: AddressObject -> Aeson.Value
  toJSON (PaymentAddress nid key) =
    let NetworkMagic nm = toNetworkMagic nid
     in Aeson.object
          [ "type" .= ("PaymentAddress" :: Text)
          , "networkMagic" .= (fromIntegral nm :: Int)
          , "signingKey" .= Text.decodeUtf8 (serialiseToRawBytesHex key)
          ]

instance FromJSON AddressObject where
  parseJSON :: Aeson.Value -> Aeson.Parser AddressObject
  parseJSON = Aeson.withObject "AddressObject" $ \o -> do
    (typ :: Text) <- o Aeson..: "type"
    case typ of
      "PaymentAddress" -> do
        keyHex <- o Aeson..: "signingKey"
        (nm :: Int) <- o Aeson..: "networkMagic"
        PaymentAddress (fromNetworkMagic (NetworkMagic (fromIntegral nm)))
          <$> toMonadFail (rightOrError $ deserialiseFromRawBytesHex (Text.encodeUtf8 keyHex))
      other -> fail $ "Unsupported address type: " ++ show other

-- | Generate a simple payment address for mainnet.
generateMainnetPaymentAddressImpl :: IO AddressObject
generateMainnetPaymentAddressImpl = do
  key <- generateSigningKey (AsPaymentKey)
  return (PaymentAddress Mainnet key)

-- | Generate a simple payment address for testnet, given the testnet's network magic.
generateTestnetPaymentAddressImpl :: Int -> IO AddressObject
generateTestnetPaymentAddressImpl nm = do
  key <- generateSigningKey (AsPaymentKey)
  return (PaymentAddress (Testnet (NetworkMagic (fromIntegral nm))) key)

-- | Get the Bech32 representation of the payment address. (Can be shared for receiving funds.)
getPaymentAddressBech32 :: AddressObject -> Text
getPaymentAddressBech32 (PaymentAddress nid key) =
  serialiseAddress $
    makeShelleyAddress
      nid
      (PaymentCredentialByKey (verificationKeyHash (getVerificationKey key)))
      NoStakeAddress

-- | Get the Bech32 representation of the verification key of the payment address. (Can be shared for verification.)
getBech32ForVerificationKeyImpl :: AddressObject -> Text
getBech32ForVerificationKeyImpl (PaymentAddress _ key) =
  serialiseToBech32 (getVerificationKey key)

-- | Get the Bech32 representation of the signing key of the payment address. (Must be kept secret.)
getBech32ForSigningKeyImpl :: AddressObject -> Text
getBech32ForSigningKeyImpl (PaymentAddress _ key) =
  serialiseToBech32 key
