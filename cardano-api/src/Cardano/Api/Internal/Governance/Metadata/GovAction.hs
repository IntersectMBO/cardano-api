{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Internal.Governance.Metadata.GovAction
  ( -- * Government action metadata
    CIP108 (..)
  )
where

import Cardano.Api.Internal.Governance.Metadata.Validation
  ( Authors
  , Body
  , GovActionMetadata (..)
  , HashAlgorithm
  )
import Cardano.Api.Internal.SerialiseJSON (textWithMaxLength)

import Data.Aeson (FromJSON, withArray, withObject, withText, (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, Value (..))
import Data.Text (Text)
import GHC.Generics (Generic)

data CIP108 = BaseGovActionMetadata

instance FromJSON (GovActionMetadata CIP108) where
  parseJSON :: Value -> Parser (GovActionMetadata CIP108)
  parseJSON = withObject "CIP108Common" $ \v ->
    GovActionMetadata
      <$> v .: "hashAlgorithm"
      <*> v .: "authors"
      <*> v .: "body"

-- Enum for HashAlgorithm

data instance HashAlgorithm CIP108 = Blake2b256
  deriving (Show, Generic)

instance FromJSON (HashAlgorithm CIP108) where
  parseJSON :: Value -> Parser (HashAlgorithm CIP108)
  parseJSON = withText "HashAlgorithm" $
    \case
      "blake2b-256" -> return Blake2b256
      _ -> fail "Invalid hashAlgorithm value, must be: blake2b-256"

-- Author object

newtype instance Authors CIP108 = Authors [Author]
  deriving (Show, Generic)

instance FromJSON (Authors CIP108) where
  parseJSON :: Value -> Parser (Authors CIP108)
  parseJSON = withArray "Authors" $ \arr ->
    Authors <$> Aeson.parseJSON (Array arr)

data Author = Author
  { name :: Maybe Text
  , witness :: Witness
  }
  deriving (Show, Generic)

instance FromJSON Author where
  parseJSON :: Value -> Parser Author
  parseJSON = withObject "Author" $ \v ->
    Author
      <$> v .:? "name"
      <*> v .: "witness"

-- Witness object
data Witness = Witness
  { witnessAlgorithm :: Maybe WitnessAlgorithm
  , publicKey :: Maybe Text
  , signature :: Maybe Text
  }
  deriving (Show, Generic)

instance FromJSON Witness where
  parseJSON :: Value -> Parser Witness
  parseJSON = withObject "Witness" $ \v ->
    Witness
      <$> v .:? "witnessAlgorithm"
      <*> v .:? "publicKey"
      <*> v .:? "signature"

-- Enum for WitnessAlgorithm
data WitnessAlgorithm = Ed25519 | CIP0008
  deriving (Show, Generic)

instance FromJSON WitnessAlgorithm where
  parseJSON :: Value -> Parser WitnessAlgorithm
  parseJSON = withText "WitnessAlgorithm" $
    \case
      "ed25519" -> return Ed25519
      "CIP-0008" -> return CIP0008
      _ -> fail "Invalid witnessAlgorithm value, must be: ed25519 or CIP-0008"

-- Body of the metadata document

data instance Body CIP108 = Body
  { title :: Text
  , abstract :: Text
  , motivation :: Text
  , rationale :: Text
  , references :: Maybe [Reference]
  }
  deriving (Show, Generic)

instance FromJSON (Body CIP108) where
  parseJSON :: Value -> Parser (Body CIP108)
  parseJSON = withObject "Body" $ \v ->
    Body
      <$> (v .: "title" >>= textWithMaxLength "title" 80)
      <*> (v .: "abstract" >>= textWithMaxLength "abstract" 2500)
      <*> v .: "motivation"
      <*> v .: "rationale"
      <*> v .:? "references"

-- Reference object
data Reference = Reference
  { refType :: ReferenceType
  , label :: Text
  , uri :: Text
  , referenceHash :: Maybe ReferenceHash
  }
  deriving (Show, Generic)

instance FromJSON Reference where
  parseJSON :: Value -> Parser Reference
  parseJSON = withObject "Reference" $ \v ->
    Reference
      <$> v .: "@type"
      <*> v .: "label"
      <*> v .: "uri"
      <*> v .:? "referenceHash"

-- Enum for ReferenceType
data ReferenceType = GovernanceMetadata | Other
  deriving (Show, Generic)

instance FromJSON ReferenceType where
  parseJSON :: Value -> Parser ReferenceType
  parseJSON = withText "ReferenceType" $
    \case
      "GovernanceMetadata" -> return GovernanceMetadata
      "Other" -> return Other
      _ -> fail "Invalid reference type, must be one of: GovernanceMetadata, Other"

-- ReferenceHash object
data ReferenceHash = ReferenceHash
  { referenceHashDigest :: Text
  , referenceHashAlgorithm :: HashAlgorithm CIP108
  }
  deriving (Show, Generic)

instance FromJSON ReferenceHash where
  parseJSON :: Value -> Parser ReferenceHash
  parseJSON = withObject "ReferenceHash" $ \v ->
    ReferenceHash
      <$> v .: "hashDigest"
      <*> v .: "hashAlgorithm"
