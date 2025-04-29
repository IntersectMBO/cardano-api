{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Class of errors used in the Api.
module Cardano.Api.Internal.DeserialiseAnyOf
  ( InputFormat (..)
  , InputDecodeError (..)
  , deserialiseInput
  , deserialiseInputAnyOf
  , renderInputDecodeError
  -- TODO: Consider moving everything below
  , SomeAddressVerificationKey (..)
  , deserialiseAnyVerificationKey
  , deserialiseAnyVerificationKeyBech32
  , deserialiseAnyVerificationKeyTextEnvelope
  , renderSomeAddressVerificationKey
  , mapSomeAddressVerificationKey
  )
where

import Cardano.Api.Internal.Address
import Cardano.Api.Internal.Error
import Cardano.Api.Internal.HasTypeProxy
import Cardano.Api.Internal.Keys.Byron
import Cardano.Api.Internal.Keys.Class
import Cardano.Api.Internal.Keys.Praos
import Cardano.Api.Internal.Keys.Shelley
import Cardano.Api.Internal.SerialiseBech32
import Cardano.Api.Internal.SerialiseRaw
import Cardano.Api.Internal.SerialiseTextEnvelope

import Cardano.Chain.Common qualified as Common
import Cardano.Crypto.Signing qualified as Crypto

import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BSC
import Data.Char (toLower)
import Data.Data
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Formatting (build, sformat, (%))
import GHC.Exts (IsList (..))
import Prettyprinter

------------------------------------------------------------------------------
-- Formatted/encoded input deserialisation
------------------------------------------------------------------------------

-- | Input format/encoding.
data InputFormat a where
  -- | Bech32 encoding.
  InputFormatBech32 :: SerialiseAsBech32 a => InputFormat a
  -- | Hex/Base16 encoding.
  InputFormatHex :: SerialiseAsRawBytes a => InputFormat a
  -- TODO: Specify TextEnvelope CBOR hex

  -- | Text envelope format.
  InputFormatTextEnvelope :: HasTextEnvelope a => InputFormat a

-- TODO: Add constructor for TextEnvelope Bech32

-- | Input decoding error.
data InputDecodeError
  = -- | The provided data seems to be a valid text envelope, but some error
    -- occurred in deserialising it.
    InputTextEnvelopeError !TextEnvelopeError
  | -- | The provided data is valid Bech32, but some error occurred in
    -- deserialising it.
    InputBech32DecodeError !Bech32DecodeError
  | -- | The provided data does not represent a valid value of the provided
    -- type.
    InputInvalidError
  deriving (Eq, Show, Data)

instance Error InputDecodeError where
  prettyError = renderInputDecodeError

-- | Render an error message for a 'InputDecodeError'.
renderInputDecodeError :: InputDecodeError -> Doc ann
renderInputDecodeError = \case
  InputTextEnvelopeError textEnvErr ->
    prettyError textEnvErr
  InputBech32DecodeError decodeErr ->
    prettyError decodeErr
  InputInvalidError ->
    "Invalid key."

-- | The result of a deserialisation function.
--
-- Note that this type isn't intended to be exported, but only used as a
-- helper within the 'deserialiseInput' function.
data DeserialiseInputResult a
  = -- | Input successfully deserialised.
    DeserialiseInputSuccess !a
  | -- | The provided data is of the expected format/encoding, but an error
    -- occurred in deserialising it.
    DeserialiseInputError !InputDecodeError
  | -- | The provided data's formatting/encoding does not match that which was
    -- expected. This error is an indication that one could attempt to
    -- deserialise the input again, but instead expecting a different format.
    DeserialiseInputErrorFormatMismatch

-- | Deserialise an input of some type that is formatted in some way.
deserialiseInput
  :: forall a
   . NonEmpty (InputFormat a)
  -> ByteString
  -> Either InputDecodeError a
deserialiseInput acceptedFormats inputBs =
  go (toList acceptedFormats)
 where
  inputText :: Text
  inputText = Text.decodeUtf8 inputBs

  go :: [InputFormat a] -> Either InputDecodeError a
  go [] = Left InputInvalidError
  go (kf : kfs) =
    let res =
          case kf of
            InputFormatBech32 -> deserialiseBech32
            InputFormatHex -> deserialiseHex
            InputFormatTextEnvelope -> deserialiseTextEnvelope
     in case res of
          DeserialiseInputSuccess a -> Right a
          DeserialiseInputError err -> Left err
          DeserialiseInputErrorFormatMismatch -> go kfs

  deserialiseTextEnvelope :: HasTextEnvelope a => DeserialiseInputResult a
  deserialiseTextEnvelope = do
    let textEnvRes :: Either TextEnvelopeError a
        textEnvRes =
          deserialiseFromTextEnvelope
            =<< first TextEnvelopeAesonDecodeError (Aeson.eitherDecodeStrict' inputBs)
    case textEnvRes of
      Right res -> DeserialiseInputSuccess res
      -- The input was valid a text envelope, but there was a type mismatch
      -- error.
      Left err@TextEnvelopeTypeError{} ->
        DeserialiseInputError (InputTextEnvelopeError err)
      -- The input was not valid a text envelope.
      Left _ -> DeserialiseInputErrorFormatMismatch

  deserialiseBech32 :: SerialiseAsBech32 a => DeserialiseInputResult a
  deserialiseBech32 =
    case deserialiseFromBech32 inputText of
      Right res -> DeserialiseInputSuccess res
      -- The input was not valid Bech32.
      Left (Bech32DecodingError _) -> DeserialiseInputErrorFormatMismatch
      -- The input was valid Bech32, but some other error occurred.
      Left err -> DeserialiseInputError $ InputBech32DecodeError err

  deserialiseHex :: SerialiseAsRawBytes a => DeserialiseInputResult a
  deserialiseHex
    | isValidHex inputBs =
        case deserialiseFromRawBytesHex inputBs of
          Left _ -> DeserialiseInputError InputInvalidError
          Right x -> DeserialiseInputSuccess x
    | otherwise = DeserialiseInputErrorFormatMismatch

  isValidHex :: ByteString -> Bool
  isValidHex x =
    all ((`elem` hexAlpha) . toLower) (BSC.unpack x)
      && even (BSC.length x)
   where
    hexAlpha :: [Char]
    hexAlpha = "0123456789abcdef"

-- | Deserialise an input of some type that is formatted in some way.
--
-- The provided 'ByteString' can either be Bech32-encoded or in the text
-- envelope format.
deserialiseInputAnyOf
  :: forall b
   . [FromSomeType SerialiseAsBech32 b]
  -> [FromSomeType HasTextEnvelope b]
  -> ByteString
  -> Either InputDecodeError b
deserialiseInputAnyOf bech32Types textEnvTypes inputBs =
  case deserialiseBech32 `orTry` deserialiseTextEnvelope of
    DeserialiseInputSuccess res -> Right res
    DeserialiseInputError err -> Left err
    DeserialiseInputErrorFormatMismatch -> Left InputInvalidError
 where
  inputText :: Text
  inputText = Text.decodeUtf8 inputBs

  orTry
    :: DeserialiseInputResult b
    -> DeserialiseInputResult b
    -> DeserialiseInputResult b
  orTry x y =
    case x of
      DeserialiseInputSuccess _ -> x
      DeserialiseInputError _ -> x
      DeserialiseInputErrorFormatMismatch -> y

  deserialiseTextEnvelope :: DeserialiseInputResult b
  deserialiseTextEnvelope = do
    let textEnvRes :: Either TextEnvelopeError b
        textEnvRes =
          deserialiseFromTextEnvelopeAnyOf textEnvTypes
            =<< first TextEnvelopeAesonDecodeError (Aeson.eitherDecodeStrict' inputBs)
    case textEnvRes of
      Right res -> DeserialiseInputSuccess res
      -- The input was valid a text envelope, but there was a type mismatch
      -- error.
      Left err@TextEnvelopeTypeError{} ->
        DeserialiseInputError (InputTextEnvelopeError err)
      -- The input was not valid a text envelope.
      Left _ -> DeserialiseInputErrorFormatMismatch

  deserialiseBech32 :: DeserialiseInputResult b
  deserialiseBech32 =
    case deserialiseAnyOfFromBech32 bech32Types inputText of
      Right res -> DeserialiseInputSuccess res
      -- The input was not valid Bech32.
      Left (Bech32DecodingError _) -> DeserialiseInputErrorFormatMismatch
      -- The input was valid Bech32, but some other error occurred.
      Left err -> DeserialiseInputError $ InputBech32DecodeError err

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
