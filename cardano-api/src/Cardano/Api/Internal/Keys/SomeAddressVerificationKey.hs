{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Class of errors used in the Api.
module Cardano.Api.Internal.Keys.SomeAddressVerificationKey
  ( SomeAddressVerificationKey (..)
  , deserialiseAnyVerificationKey
  , deserialiseAnyVerificationKeyBech32
  , deserialiseAnyVerificationKeyTextEnvelope
  , renderSomeAddressVerificationKey
  , mapSomeAddressVerificationKey
  )
where

import Cardano.Api.Internal.Address
import Cardano.Api.Internal.DeserialiseAnyOf
import Cardano.Api.Internal.HasTypeProxy
import Cardano.Api.Internal.Keys.Byron
import Cardano.Api.Internal.Keys.Class
import Cardano.Api.Internal.Keys.Praos
import Cardano.Api.Internal.Keys.Shelley
import Cardano.Api.Internal.SerialiseBech32
import Cardano.Api.Internal.SerialiseTextEnvelope

import Cardano.Chain.Common qualified as Common
import Cardano.Crypto.Signing qualified as Crypto

import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Formatting (build, sformat, (%))

data SomeAddressVerificationKey
  = AByronVerificationKey (VerificationKey ByronKey)
  | APaymentVerificationKey (VerificationKey PaymentKey)
  | APaymentExtendedVerificationKey (VerificationKey PaymentExtendedKey)
  | AGenesisUTxOVerificationKey (VerificationKey GenesisUTxOKey)
  | AGenesisExtendedVerificationKey (VerificationKey GenesisExtendedKey)
  | AGenesisDelegateExtendedVerificationKey
      (VerificationKey GenesisDelegateExtendedKey)
  | AKesVerificationKey (VerificationKey KesKey)
  | AVrfVerificationKey (VerificationKey VrfKey)
  | AStakeVerificationKey (VerificationKey StakeKey)
  | AStakeExtendedVerificationKey (VerificationKey StakeExtendedKey)
  | AStakePoolVerificationKey (VerificationKey StakePoolKey)
  | AStakePoolExtendedVerificationKey (VerificationKey StakePoolExtendedKey)
  | ADRepVerificationKey (VerificationKey DRepKey)
  | ADRepExtendedVerificationKey (VerificationKey DRepExtendedKey)
  | ACommitteeColdVerificationKey (VerificationKey CommitteeColdKey)
  | ACommitteeColdExtendedVerificationKey (VerificationKey CommitteeColdExtendedKey)
  | ACommitteeHotVerificationKey (VerificationKey CommitteeHotKey)
  | ACommitteeHotExtendedVerificationKey (VerificationKey CommitteeHotExtendedKey)
  deriving Show

renderSomeAddressVerificationKey :: SomeAddressVerificationKey -> Text
renderSomeAddressVerificationKey =
  \case
    AByronVerificationKey vk -> prettyByronVerificationKey vk
    APaymentVerificationKey vk -> serialiseToBech32 vk
    APaymentExtendedVerificationKey vk -> serialiseToBech32 vk
    AGenesisUTxOVerificationKey vk -> serialiseToBech32 (castVerificationKey vk :: VerificationKey PaymentKey)
    AGenesisExtendedVerificationKey vk ->
      let genKey = (castVerificationKey vk :: VerificationKey GenesisKey)
          payKey = (castVerificationKey genKey :: VerificationKey PaymentKey)
       in serialiseToBech32 payKey
    AGenesisDelegateExtendedVerificationKey vk ->
      -- TODO: We could implement a CastVerificationKeyRole GenesisDelegateKey PaymentKey
      -- if we want to avoid casting twice.
      let genDelegKey = (castVerificationKey vk :: VerificationKey GenesisDelegateKey)
          stakePoolKey = castVerificationKey genDelegKey :: VerificationKey StakePoolKey
       in serialiseToBech32 stakePoolKey
    AKesVerificationKey vk -> serialiseToBech32 vk
    AVrfVerificationKey vk -> serialiseToBech32 vk
    AStakeVerificationKey vk -> serialiseToBech32 vk
    AStakePoolVerificationKey vk -> serialiseToBech32 vk
    AStakePoolExtendedVerificationKey vk -> serialiseToBech32 vk
    AStakeExtendedVerificationKey vk -> serialiseToBech32 vk
    ADRepVerificationKey vk -> serialiseToBech32 vk
    ADRepExtendedVerificationKey vk -> serialiseToBech32 vk
    ACommitteeColdVerificationKey vk -> serialiseToBech32 vk
    ACommitteeColdExtendedVerificationKey vk -> serialiseToBech32 vk
    ACommitteeHotVerificationKey vk -> serialiseToBech32 vk
    ACommitteeHotExtendedVerificationKey vk -> serialiseToBech32 vk

mapSomeAddressVerificationKey
  :: ()
  => (forall keyrole. Key keyrole => VerificationKey keyrole -> a)
  -> SomeAddressVerificationKey
  -> a
mapSomeAddressVerificationKey f = \case
  AByronVerificationKey vk -> f vk
  APaymentVerificationKey vk -> f vk
  APaymentExtendedVerificationKey vk -> f vk
  AGenesisUTxOVerificationKey vk -> f vk
  AKesVerificationKey vk -> f vk
  AGenesisDelegateExtendedVerificationKey vk -> f vk
  AGenesisExtendedVerificationKey vk -> f vk
  AVrfVerificationKey vk -> f vk
  AStakeVerificationKey vk -> f vk
  AStakePoolVerificationKey vk -> f vk
  AStakePoolExtendedVerificationKey vk -> f vk
  AStakeExtendedVerificationKey vk -> f vk
  ADRepVerificationKey vk -> f vk
  ADRepExtendedVerificationKey vk -> f vk
  ACommitteeColdVerificationKey vk -> f vk
  ACommitteeColdExtendedVerificationKey vk -> f vk
  ACommitteeHotVerificationKey vk -> f vk
  ACommitteeHotExtendedVerificationKey vk -> f vk

-- | Internal function to pretty render byron keys
prettyByronVerificationKey :: VerificationKey ByronKey -> Text
prettyByronVerificationKey (ByronVerificationKey vk) =
  sformat
    ( "    public key hash: "
        % build
        % "\npublic key (base64): "
        % Crypto.fullVerificationKeyF
        % "\n   public key (hex): "
        % Crypto.fullVerificationKeyHexF
    )
    (Common.addressHash vk)
    vk
    vk

deserialiseAnyVerificationKey
  :: ByteString -> Either InputDecodeError SomeAddressVerificationKey
deserialiseAnyVerificationKey bs =
  case deserialiseAnyVerificationKeyBech32 bs of
    Right vk -> Right vk
    Left _e ->
      case deserialiseAnyVerificationKeyTextEnvelope bs of
        Right vk -> Right vk
        Left _e -> Left InputInvalidError

deserialiseAnyVerificationKeyBech32
  :: ByteString -> Either Bech32DecodeError SomeAddressVerificationKey
deserialiseAnyVerificationKeyBech32 =
  deserialiseAnyOfFromBech32 allBech32VerKey . Text.decodeUtf8
 where
  allBech32VerKey
    :: [FromSomeType SerialiseAsBech32 SomeAddressVerificationKey]
  allBech32VerKey =
    [ FromSomeType (AsVerificationKey AsDRepKey) ADRepVerificationKey
    , FromSomeType (AsVerificationKey AsDRepExtendedKey) ADRepExtendedVerificationKey
    , FromSomeType (AsVerificationKey AsCommitteeColdKey) ACommitteeColdVerificationKey
    , FromSomeType (AsVerificationKey AsCommitteeColdExtendedKey) ACommitteeColdExtendedVerificationKey
    , FromSomeType (AsVerificationKey AsCommitteeHotKey) ACommitteeHotVerificationKey
    , FromSomeType (AsVerificationKey AsCommitteeHotExtendedKey) ACommitteeHotExtendedVerificationKey
    , FromSomeType (AsVerificationKey AsPaymentKey) APaymentVerificationKey
    , FromSomeType (AsVerificationKey AsPaymentExtendedKey) APaymentExtendedVerificationKey
    , FromSomeType (AsVerificationKey AsKesKey) AKesVerificationKey
    , FromSomeType (AsVerificationKey AsVrfKey) AVrfVerificationKey
    , FromSomeType (AsVerificationKey AsStakeKey) AStakeVerificationKey
    , FromSomeType (AsVerificationKey AsStakeExtendedKey) AStakeExtendedVerificationKey
    , FromSomeType (AsVerificationKey AsStakePoolKey) AStakePoolVerificationKey
    , FromSomeType (AsVerificationKey AsStakePoolExtendedKey) AStakePoolExtendedVerificationKey
    ]

deserialiseAnyVerificationKeyTextEnvelope
  :: ByteString -> Either TextEnvelopeError SomeAddressVerificationKey
deserialiseAnyVerificationKeyTextEnvelope bs =
  deserialiseFromTextEnvelopeAnyOf allTextEnvelopeCBOR
    =<< first TextEnvelopeAesonDecodeError (Aeson.eitherDecodeStrict' bs)
 where
  allTextEnvelopeCBOR
    :: [FromSomeType HasTextEnvelope SomeAddressVerificationKey]
  allTextEnvelopeCBOR =
    [ FromSomeType (AsVerificationKey AsByronKey) AByronVerificationKey
    , FromSomeType (AsVerificationKey AsDRepKey) ADRepVerificationKey
    , FromSomeType (AsVerificationKey AsDRepExtendedKey) ADRepExtendedVerificationKey
    , FromSomeType (AsVerificationKey AsCommitteeColdKey) ACommitteeColdVerificationKey
    , FromSomeType (AsVerificationKey AsCommitteeColdExtendedKey) ACommitteeColdExtendedVerificationKey
    , FromSomeType (AsVerificationKey AsCommitteeHotKey) ACommitteeHotVerificationKey
    , FromSomeType (AsVerificationKey AsCommitteeHotExtendedKey) ACommitteeHotExtendedVerificationKey
    , FromSomeType (AsVerificationKey AsPaymentKey) APaymentVerificationKey
    , FromSomeType (AsVerificationKey AsPaymentExtendedKey) APaymentExtendedVerificationKey
    , FromSomeType (AsVerificationKey AsStakeKey) AStakeVerificationKey
    , FromSomeType (AsVerificationKey AsStakeExtendedKey) AStakeExtendedVerificationKey
    , FromSomeType (AsVerificationKey AsStakePoolKey) AStakePoolVerificationKey
    , FromSomeType (AsVerificationKey AsStakePoolExtendedKey) AStakePoolExtendedVerificationKey
    , FromSomeType (AsVerificationKey AsGenesisUTxOKey) AGenesisUTxOVerificationKey
    , FromSomeType (AsVerificationKey AsGenesisExtendedKey) AGenesisExtendedVerificationKey
    ]
