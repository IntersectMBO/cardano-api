{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Cardano.Api.Governance.Actions.MetadataValidation (validateGovActionAnchorData) where

import           Data.Aeson (eitherDecodeStrict)
import           Data.Aeson.Types (FromJSON (..), Parser, Value, withObject, withText, (.:), (.:?))
import           Data.ByteString (ByteString)
import           Data.Either.Combinators (mapRight)
import           Data.Text (Text)
import           GHC.Generics (Generic)

validateGovActionAnchorData :: ByteString -> Either String ()
validateGovActionAnchorData bytes = mapRight (const ()) (eitherDecodeStrict bytes :: Either String CIP100Common)

-- Root object: CIP-100 Common
data CIP100Common = CIP100Common
  { hashAlgorithm :: HashAlgorithm
  , authors :: [Author]
  , body :: Body
  }
  deriving (Show, Generic)

instance FromJSON CIP100Common where
  parseJSON :: Value -> Parser CIP100Common
  parseJSON = withObject "CIP100Common" $ \v ->
    CIP100Common
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
data WitnessAlgorithm = Ed25519
  deriving (Show, Generic)

instance FromJSON WitnessAlgorithm where
  parseJSON :: Value -> Parser WitnessAlgorithm
  parseJSON = withText "WitnessAlgorithm" $
    \case
      "ed25519" -> return Ed25519
      _ -> fail "Invalid witnessAlgorithm value, must be: ed25519"

-- Body of the metadata document
data Body = Body
  { references :: Maybe [Reference]
  , comment :: Maybe Text
  , externalUpdates :: Maybe [ExternalUpdate]
  }
  deriving (Show, Generic)

instance FromJSON Body where
  parseJSON :: Value -> Parser Body
  parseJSON = withObject "Body" $ \v ->
    Body
      <$> v .:? "references"
      <*> v .:? "comment"
      <*> v .:? "externalUpdatess"

-- Reference object
data Reference = Reference
  { referenceType :: ReferenceType
  , label :: Text
  , refUri :: Text
  }
  deriving (Show, Generic)

instance FromJSON Reference where
  parseJSON :: Value -> Parser Reference
  parseJSON = withObject "Reference" $ \v ->
    Reference
      <$> v .: "@type"
      <*> v .: "label"
      <*> v .: "uri"

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

-- ExternalUpdate object
data ExternalUpdate = ExternalUpdate
  { title :: Text
  , updateUri :: Text
  }
  deriving (Show, Generic)

instance FromJSON ExternalUpdate where
  parseJSON :: Value -> Parser ExternalUpdate
  parseJSON = withObject "ExternalUpdate" $ \v ->
    ExternalUpdate
      <$> v .: "title"
      <*> v .: "uri"
