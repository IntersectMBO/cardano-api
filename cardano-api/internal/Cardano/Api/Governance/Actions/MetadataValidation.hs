{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Api.Governance.Actions.MetadataValidation (validateGovActionAnchorData) where

import           Cardano.Api.GeneralParsers (textWithMaxLength)

import           Data.Aeson (eitherDecodeStrict)
import           Data.Aeson.Types (FromJSON (..), Parser, Value, withObject, withText, (.:), (.:?))
import           Data.ByteString (ByteString)
import           Data.Either.Combinators (mapRight)
import           Data.Text (Text)
import           GHC.Generics (Generic)

validateGovActionAnchorData :: ByteString -> Either String ()
validateGovActionAnchorData bytes = mapRight (const ()) (eitherDecodeStrict bytes :: Either String CIP108Common)

-- Root object: CIP-108 Common
data CIP108Common = CIP108Common
  { hashAlgorithm :: HashAlgorithm
  , authors :: [Author]
  , body :: Body
  }
  deriving (Show, Generic)

instance FromJSON CIP108Common where
  parseJSON :: Value -> Parser CIP108Common
  parseJSON = withObject "CIP108Common" $ \v ->
    CIP108Common
      <$> v .: "hashAlgorithm"
      <*> v .: "authors"
      <*> v .: "body"

-- Enum for HashAlgorithm
data HashAlgorithm = Blake2b256
  deriving (Show, Generic)

instance FromJSON HashAlgorithm where
  parseJSON :: Value -> Parser HashAlgorithm
  parseJSON = withText "HashAlgorithm" $
    \case
      "blake2b-256" -> return Blake2b256
      _ -> fail "Invalid hashAlgorithm value, must be: blake2b-256"

-- Author object
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
data Body = Body
  { title :: Text
  , abstract :: Text
  , motivation :: Text
  , rationale :: Text
  , references :: Maybe [Reference]
  }
  deriving (Show, Generic)

instance FromJSON Body where
  parseJSON :: Value -> Parser Body
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
  , referenceHashAlgorithm :: HashAlgorithm
  }
  deriving (Show, Generic)

instance FromJSON ReferenceHash where
  parseJSON :: Value -> Parser ReferenceHash
  parseJSON = withObject "ReferenceHash" $ \v ->
    ReferenceHash
      <$> v .: "hashDigest"
      <*> v .: "hashAlgorithm"
