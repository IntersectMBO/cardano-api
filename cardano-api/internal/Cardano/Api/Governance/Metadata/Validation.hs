{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Cardano.Api.Governance.Metadata.Validation (GovActionMetadata (..), Authors, Body, HashAlgorithm, validateGovActionAnchorData) where

import           Data.Aeson (FromJSON, eitherDecodeStrict)
import           Data.ByteString (ByteString)
import           Data.Either.Combinators (mapRight)

data GovActionMetadata cip
  = GovActionMetadata
  { hashAlgorithm :: HashAlgorithm cip
  , authors :: Authors cip
  , body :: Body cip
  }

data family Authors cip

data family Body cip

data family HashAlgorithm cip

validateGovActionAnchorData
  :: forall cip. FromJSON (GovActionMetadata cip) => cip -> ByteString -> Either String ()
validateGovActionAnchorData cip bytes = mapRight (const ()) (decodeGovAction cip bytes)
 where
  decodeGovAction :: cip -> ByteString -> Either String (GovActionMetadata cip)
  decodeGovAction _ = eitherDecodeStrict
