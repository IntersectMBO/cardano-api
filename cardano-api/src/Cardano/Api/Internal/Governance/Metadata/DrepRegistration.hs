{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Internal.Governance.Metadata.DrepRegistration
  ( -- * DRep off-chain metadata
    CIP119 (..)
  )
where

import Cardano.Api.Internal.Governance.Metadata.Validation
  ( Authors
  , Body
  , GovActionMetadata (..)
  , HashAlgorithm
  )
import Cardano.Api.Internal.SerialiseJSON (textWithMaxLength)

import Data.Aeson (FromJSON, withObject, (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import GHC.Generics (Generic)

data CIP119 = DrepRegistrationMetadata

instance FromJSON (GovActionMetadata CIP119) where
  parseJSON :: Aeson.Value -> Parser (GovActionMetadata CIP119)
  parseJSON = withObject "CIP119Common" $ \v ->
    GovActionMetadata
      <$> v .: "hashAlgorithm"
      <*> pure Absent
      <*> v .: "body"

-- Hash Algorithm (Enum)
data instance HashAlgorithm CIP119 = Blake2b256
  deriving (Show, Generic)

instance FromJSON (HashAlgorithm CIP119) where
  parseJSON :: Aeson.Value -> Parser (HashAlgorithm CIP119)
  parseJSON = Aeson.withText "HashAlgorithm" $
    \case
      "blake2b-256" -> return Blake2b256
      _ -> fail "Invalid hashAlgorithm, it must be: blake2b-256"

-- Body of the metadata document
data instance Body CIP119 = Body
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

instance FromJSON (Body CIP119) where
  parseJSON :: Aeson.Value -> Parser (Body CIP119)
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

-- We don't need to validate Authors because it is optional in CIP-119
data instance Authors CIP119 = Absent
