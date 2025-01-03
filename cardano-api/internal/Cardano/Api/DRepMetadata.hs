{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

-- | DRep off-chain metadata
module Cardano.Api.DRepMetadata
  ( -- * DRep off-chain metadata
    DRepMetadata (..)
  , hashDRepMetadata

    -- * Data family instances
  , AsType (..)
  , Hash (..)
  , validateDRepAnchorData
  )
where

import           Cardano.Api.Eras
import           Cardano.Api.GeneralParsers (textWithMaxLength)
import           Cardano.Api.Hash
import           Cardano.Api.HasTypeProxy
import           Cardano.Api.Keys.Byron
import           Cardano.Api.Keys.Praos
import           Cardano.Api.Script
import           Cardano.Api.SerialiseRaw

import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Cardano.Ledger.Keys as Shelley

import           Data.Aeson (FromJSON, withObject, (.:), (.:?))
import qualified Data.Aeson as Aeson
import           Data.Aeson.Types (Parser)
import           Data.ByteString (ByteString)
import           Data.Either.Combinators (mapRight, maybeToRight)
import           Data.Text (Text)
import           GHC.Generics (Generic)

-- ----------------------------------------------------------------------------
-- DRep metadata
--

-- | A representation of the required fields for off-chain drep metadata.
newtype DRepMetadata = DRepMetadata
  { unDRepMetadata :: ByteString
  }
  deriving (Eq, Show)

newtype instance Hash DRepMetadata = DRepMetadataHash (Shelley.Hash StandardCrypto ByteString)
  deriving (Eq, Show)

instance HasTypeProxy DRepMetadata where
  data AsType DRepMetadata = AsDRepMetadata
  proxyToAsType :: Proxy DRepMetadata -> AsType DRepMetadata
  proxyToAsType _ = AsDRepMetadata

instance SerialiseAsRawBytes (Hash DRepMetadata) where
  serialiseToRawBytes :: Hash DRepMetadata -> ByteString
  serialiseToRawBytes (DRepMetadataHash h) = Crypto.hashToBytes h

  deserialiseFromRawBytes
    :: AsType (Hash DRepMetadata) -> ByteString -> Either SerialiseAsRawBytesError (Hash DRepMetadata)
  deserialiseFromRawBytes (AsHash AsDRepMetadata) bs =
    maybeToRight (SerialiseAsRawBytesError "Unable to deserialise Hash DRepMetadata") $
      DRepMetadataHash <$> Crypto.hashFromBytes bs

-- | Return the decoded metadata and the hash of the original bytes.
hashDRepMetadata
  :: ByteString
  -> (DRepMetadata, Hash DRepMetadata)
hashDRepMetadata bs =
  let md = DRepMetadata bs
      mdh = DRepMetadataHash (Crypto.hashWith id bs)
   in (md, mdh)

-- * DRep metadata validation

-- | Root document
data CIP119Common = CIP119Common
  { hashAlgorithm :: HashAlgorithm
  , body :: Body
  }
  deriving (Show, Generic)

instance FromJSON CIP119Common where
  parseJSON :: Aeson.Value -> Parser CIP119Common
  parseJSON = withObject "CIP119Common" $ \v ->
    CIP119Common
      <$> v .: "hashAlgorithm"
      <*> v .: "body"

-- Hash Algorithm (Enum)
data HashAlgorithm = Blake2b256
  deriving (Show, Generic)

instance FromJSON HashAlgorithm where
  parseJSON :: Aeson.Value -> Parser HashAlgorithm
  parseJSON = Aeson.withText "HashAlgorithm" $
    \case
      "blake2b-256" -> return Blake2b256
      _ -> fail "Invalid hashAlgorithm, it must be: blake2b-256"

-- Body of the metadata document
data Body = Body
  { paymentAddress :: Maybe Text
  , givenName :: Text
  , image :: Maybe ImageObject
  , objectives :: Maybe Text
  , motivations :: Maybe Text
  , qualifications :: Maybe Text
  , doNotList :: Maybe DoNotList
  , references :: Maybe [Reference]
  }
  deriving (Show, Generic)

instance FromJSON Body where
  parseJSON :: Aeson.Value -> Parser Body
  parseJSON = withObject "Body" $ \v ->
    Body
      <$> v .:? "paymentAddress"
      <*> (v .: "givenName" >>= textWithMaxLength "givenName" 80)
      <*> v .:? "image"
      <*> (v .:? "objectives" >>= traverse (textWithMaxLength "objectives" 1000))
      <*> (v .:? "motivations" >>= traverse (textWithMaxLength "motivations" 1000))
      <*> (v .:? "qualifications" >>= traverse (textWithMaxLength "qualifications" 1000))
      <*> v .:? "doNotList"
      <*> v .:? "references"

-- Profile picture
data ImageObject = ImageObject
  { contentUrl :: Text -- Base64 encoded image or URL
  , sha256 :: Maybe Text -- Only present for URL images
  }
  deriving (Show, Generic)

instance FromJSON ImageObject where
  parseJSON :: Aeson.Value -> Parser ImageObject
  parseJSON = withObject "ImageObject" $ \v ->
    ImageObject
      <$> v .: "contentUrl"
      <*> v .:? "sha256"

-- DoNotList Enum
data DoNotList = DoNotListTrue | DoNotListFalse
  deriving (Show, Generic)

instance FromJSON DoNotList where
  parseJSON :: Aeson.Value -> Parser DoNotList
  parseJSON = Aeson.withText "DoNotList" $
    \case
      "true" -> return DoNotListTrue
      "false" -> return DoNotListFalse
      _ -> fail "Invalid doNotList value, must be one of: true, false"

-- Reference type
data Reference = Reference
  { refType :: ReferenceType
  , label :: Text
  , uri :: Text
  }
  deriving (Show, Generic)

instance FromJSON Reference where
  parseJSON :: Aeson.Value -> Parser Reference
  parseJSON = withObject "Reference" $ \v ->
    Reference
      <$> v .: "@type"
      <*> v .: "label"
      <*> v .: "uri"

-- ReferenceType Enum
data ReferenceType = GovernanceMetadata | Other | Link | Identity
  deriving (Show, Generic)

instance FromJSON ReferenceType where
  parseJSON :: Aeson.Value -> Parser ReferenceType
  parseJSON = Aeson.withText "ReferenceType" $
    \case
      "GovernanceMetadata" -> return GovernanceMetadata
      "Other" -> return Other
      "Link" -> return Link
      "Identity" -> return Identity
      _ ->
        fail "Invalid reference type, must be one of: GovernanceMetadata, Other, Link, Identity"

validateDRepAnchorData :: DRepMetadata -> Either String ()
validateDRepAnchorData (DRepMetadata bytes) = mapRight (const ()) (Aeson.eitherDecodeStrict bytes :: Either String CIP119Common)
