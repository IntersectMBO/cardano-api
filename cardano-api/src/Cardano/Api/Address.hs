{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | Cardano addresses: payment and stake addresses.
module Cardano.Api.Address
  ( -- * Payment addresses

    -- | Constructing and inspecting normal payment addresses
    Address (..)

    -- ** Byron addresses
  , ByronAddr
  , makeByronAddress

    -- ** Shelley addresses
  , ShelleyAddr
  , makeShelleyAddress
  , PaymentCredential (..)
  , StakeAddressReference (..)
  , StakeAddressPointer (..)

    -- ** Addresses in any era
  , AddressAny (..)
  , parseAddressAny

    -- ** Addresses in specific eras
  , AddressInEra (..)
  , AddressTypeInEra (..)
  , byronAddressInEra
  , shelleyAddressInEra
  , anyAddressInShelleyBasedEra
  , anyAddressInEra
  , toAddressAny
  , makeByronAddressInEra
  , makeShelleyAddressInEra

    -- * Stake addresses

    -- | Constructing and inspecting stake addresses
  , StakeAddress (..)
  , StakeCredential (..)
  , makeStakeAddress
  , stakeAddressCredential
  , StakeKey
  , StakeExtendedKey

    -- * Conversion functions
  , shelleyPayAddrToPlutusPubKHash

    -- * Internal conversion functions
  , toShelleyAddr
  , toShelleyStakeAddr
  , toShelleyStakeCredential
  , fromShelleyAddr
  , fromShelleyAddrIsSbe
  , fromShelleyAddrToAny
  , fromShelleyPaymentCredential
  , fromShelleyStakeAddr
  , fromShelleyStakeCredential
  , fromShelleyStakeReference

    -- * Serialising addresses
  , SerialiseAddress (..)

    -- * Data family instances
  , AsType
    ( AsByronAddr
    , AsShelleyAddr
    , AsByronAddress
    , AsShelleyAddress
    , AsAddress
    , AsAddressAny
    , AsAddressInEra
    , AsStakeAddress
    )

    -- * Helpers
  , isKeyAddress
  )
where

import Cardano.Api.Byron.Internal.Key
import Cardano.Api.Era
import Cardano.Api.HasTypeProxy
import Cardano.Api.Key.Internal
import Cardano.Api.Monad.Error
import Cardano.Api.Network.Internal.NetworkId
import Cardano.Api.Parser.Text qualified as P
import Cardano.Api.Plutus.Internal.Script
import Cardano.Api.Serialise.Bech32
import Cardano.Api.Serialise.Raw

import Cardano.Chain.Common qualified as Byron
import Cardano.Ledger.Address qualified as Shelley
import Cardano.Ledger.BaseTypes qualified as Shelley
import Cardano.Ledger.Credential qualified as Shelley
import Cardano.Ledger.Plutus.TxInfo qualified as Plutus
import PlutusLedgerApi.V1 qualified as PlutusAPI

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData (..), deepseq)
import Data.Aeson (FromJSON (..), ToJSON (..), withText, (.=))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (first)
import Data.ByteString.Base58 qualified as Base58
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Either.Combinators (rightToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text

-- ----------------------------------------------------------------------------
-- Address Serialisation
--

-- | Address serialisation uses different serialisation formats for different
-- kinds of addresses, so it needs its own class.
--
-- In particular, Byron addresses are typically formatted in base 58, while
-- Shelley addresses (payment and stake) are formatted using Bech32.
class HasTypeProxy addr => SerialiseAddress addr where
  serialiseAddress :: addr -> Text

  deserialiseAddress :: AsType addr -> Text -> Maybe addr

-- TODO: consider adding data AddressDecodeError

-- ----------------------------------------------------------------------------
-- Payment address types
--

-- | A type used as a tag to distinguish Byron addresses.
data ByronAddr

-- | A type used as a tag to distinguish Shelley addresses.
data ShelleyAddr

instance HasTypeProxy ByronAddr where
  data AsType ByronAddr = AsByronAddr
  proxyToAsType _ = AsByronAddr

instance HasTypeProxy ShelleyAddr where
  data AsType ShelleyAddr = AsShelleyAddr
  proxyToAsType _ = AsShelleyAddr

-- ----------------------------------------------------------------------------
-- Payment addresses
--

-- | Addresses are used as locations where assets live. The address determines
-- the rights needed to spend assets at the address: in particular holding some
-- signing key or being able to satisfy the conditions of a script.
--
-- There are currently two types of address:
--
-- * Byron addresses, which use the type tag 'ByronAddr'; and
-- * Shelley addresses, which use the type tag 'ShelleyAddr'. Notably, Shelley
--   addresses support scripts and stake delegation.
--
-- The /address type/ is subtly from the /ledger era/ in which each
-- address type is valid: while Byron addresses are the only choice in the
-- Byron era, the Shelley era and all subsequent eras support both Byron and
-- Shelley addresses. The 'Address' type param only says the type of the address
-- (either Byron or Shelley). The 'AddressInEra' type connects the address type
-- with the era in which it is supported.
data Address addrtype where
  -- | Byron addresses were the only supported address type in the original
  -- Byron era.
  ByronAddress
    :: Byron.Address
    -> Address ByronAddr
  -- | Shelley addresses allow delegation. Shelley addresses were introduced
  -- in Shelley era and are thus supported from the Shelley era onwards
  ShelleyAddress
    :: Shelley.Network
    -> Shelley.PaymentCredential
    -> Shelley.StakeReference
    -> Address ShelleyAddr

-- Note that the two ledger credential types here are parametrised by
-- the era, but in fact this is a phantom type parameter and they are
-- the same for all eras. See 'toShelleyAddr' below.

deriving instance Eq (Address addrtype)

deriving instance Ord (Address addrtype)

deriving instance Show (Address addrtype)

instance NFData (Address addrtype) where
  rnf = \case
    ByronAddress address -> deepseq address ()
    ShelleyAddress n pc sr -> deepseq (deepseq (deepseq n pc) sr) ()

instance HasTypeProxy addrtype => HasTypeProxy (Address addrtype) where
  data AsType (Address addrtype) = AsAddress (AsType addrtype)
  proxyToAsType _ = AsAddress (proxyToAsType (Proxy :: Proxy addrtype))

{-# DEPRECATED AsByronAddress "Use AsAddress AsByronAddr instead" #-}
pattern AsByronAddress :: AsType (Address ByronAddr)
pattern AsByronAddress = AsAddress AsByronAddr

{-# COMPLETE AsByronAddress #-}

{-# DEPRECATED AsShelleyAddress "Use AsAddress AsShelleyAddr instead" #-}
pattern AsShelleyAddress :: AsType (Address ShelleyAddr)
pattern AsShelleyAddress = AsAddress AsShelleyAddr

{-# COMPLETE AsShelleyAddress #-}

instance SerialiseAsRawBytes (Address ByronAddr) where
  serialiseToRawBytes (ByronAddress addr) =
    Shelley.serialiseAddr
      . Shelley.AddrBootstrap
      . Shelley.BootstrapAddress
      $ addr

  deserialiseFromRawBytes (AsAddress AsByronAddr) bs =
    case Shelley.decodeAddr bs :: Maybe Shelley.Addr of
      Nothing -> Left (SerialiseAsRawBytesError "Unable to deserialise Address ByronAddr")
      Just Shelley.Addr{} -> Left (SerialiseAsRawBytesError "Unable to deserialise Address ByronAddr")
      Just (Shelley.AddrBootstrap (Shelley.BootstrapAddress addr)) ->
        Right (ByronAddress addr)

instance SerialiseAsRawBytes (Address ShelleyAddr) where
  serialiseToRawBytes (ShelleyAddress nw pc scr) =
    Shelley.serialiseAddr (Shelley.Addr nw pc scr)

  deserialiseFromRawBytes (AsAddress AsShelleyAddr) bs =
    case Shelley.decodeAddr bs of
      Nothing ->
        Left (SerialiseAsRawBytesError "Unable to deserialise bootstrap Address ShelleyAddr")
      Just Shelley.AddrBootstrap{} -> Left (SerialiseAsRawBytesError "Unable to deserialise bootstrap Address ShelleyAddr")
      Just (Shelley.Addr nw pc scr) -> Right (ShelleyAddress nw pc scr)

instance SerialiseAsBech32 (Address ShelleyAddr) where
  bech32PrefixFor (ShelleyAddress Shelley.Mainnet _ _) = unsafeHumanReadablePartFromText "addr"
  bech32PrefixFor (ShelleyAddress Shelley.Testnet _ _) = unsafeHumanReadablePartFromText "addr_test"

  bech32PrefixesPermitted (AsAddress AsShelleyAddr) = unsafeHumanReadablePartFromText <$> ["addr", "addr_test"]

instance SerialiseAddress (Address ByronAddr) where
  serialiseAddress addr@ByronAddress{} =
    Text.decodeLatin1
      . Base58.encodeBase58 Base58.bitcoinAlphabet
      . serialiseToRawBytes
      $ addr

  deserialiseAddress (AsAddress AsByronAddr) txt = do
    bs <- Base58.decodeBase58 Base58.bitcoinAlphabet (Text.encodeUtf8 txt)
    rightToMaybe (deserialiseFromRawBytes (AsAddress AsByronAddr) bs)

instance SerialiseAddress (Address ShelleyAddr) where
  serialiseAddress addr@ShelleyAddress{} =
    serialiseToBech32 addr

  deserialiseAddress (AsAddress AsShelleyAddr) t =
    either (const Nothing) Just $
      deserialiseFromBech32 t

instance ToJSON (Address ShelleyAddr) where
  toJSON = Aeson.String . serialiseAddress

instance ToJSON (Address ByronAddr) where
  toJSON = Aeson.String . serialiseAddress

instance FromJSON (Address ByronAddr) where
  parseJSON = Aeson.withText "Address" $ \txt ->
    maybe
      (fail "Cardano.Api.Address.FromJSON: Invalid Byron address.")
      pure
      (deserialiseAddress AsByronAddress txt)

instance FromJSON (Address ShelleyAddr) where
  parseJSON = Aeson.withText "Address" $ \txt ->
    maybe
      (fail "Cardano.Api.Address.FromJSON: Invalid Shelley address.")
      pure
      (deserialiseAddress AsShelleyAddress txt)

makeByronAddress
  :: NetworkId
  -> VerificationKey ByronKey
  -> Address ByronAddr
makeByronAddress nw (ByronVerificationKey vk) =
  ByronAddress $
    Byron.makeVerKeyAddress
      (toByronNetworkMagic nw)
      vk

makeShelleyAddress
  :: NetworkId
  -> PaymentCredential
  -> StakeAddressReference
  -> Address ShelleyAddr
makeShelleyAddress nw pc scr =
  ShelleyAddress
    (toShelleyNetwork nw)
    (toShelleyPaymentCredential pc)
    (toShelleyStakeReference scr)

-- ----------------------------------------------------------------------------
-- Either type of address
--

-- | Either a Byron address or a Shelley address.
--
-- Sometimes we need to be able to work with either of the two types of
-- address (Byron or Shelley addresses), but without reference to an era in
-- which the address will be used. This type serves that purpose.
data AddressAny
  = AddressByron !(Address ByronAddr)
  | AddressShelley !(Address ShelleyAddr)
  deriving (Eq, Ord, Show)

instance HasTypeProxy AddressAny where
  data AsType AddressAny = AsAddressAny
  proxyToAsType _ = AsAddressAny

instance SerialiseAsRawBytes AddressAny where
  serialiseToRawBytes (AddressByron addr) = serialiseToRawBytes addr
  serialiseToRawBytes (AddressShelley addr) = serialiseToRawBytes addr

  deserialiseFromRawBytes AsAddressAny bs =
    case Shelley.decodeAddr bs of
      Nothing -> Left (SerialiseAsRawBytesError "Unable to deserialise AddressAny")
      Just (Shelley.AddrBootstrap (Shelley.BootstrapAddress addr)) ->
        Right (AddressByron (ByronAddress addr))
      Just (Shelley.Addr nw pc scr) ->
        Right (AddressShelley (ShelleyAddress nw pc scr))

instance SerialiseAddress AddressAny where
  serialiseAddress (AddressByron addr) = serialiseAddress addr
  serialiseAddress (AddressShelley addr) = serialiseAddress addr

  deserialiseAddress AsAddressAny t =
    (AddressByron <$> deserialiseAddress (AsAddress AsByronAddr) t)
      <|> (AddressShelley <$> deserialiseAddress (AsAddress AsShelleyAddr) t)

fromShelleyAddrToAny :: Shelley.Addr -> AddressAny
fromShelleyAddrToAny (Shelley.AddrBootstrap (Shelley.BootstrapAddress addr)) =
  AddressByron $ ByronAddress addr
fromShelleyAddrToAny (Shelley.Addr nw pc scr) =
  AddressShelley $ ShelleyAddress nw pc scr

-- ----------------------------------------------------------------------------
-- Addresses in the context of a ledger era
--

-- | An 'Address' that can be used in a particular ledger era.
--
-- All current ledger eras support Byron addresses. Shelley addresses are
-- supported in the 'ShelleyEra' and later eras.
data AddressInEra era where
  AddressInEra
    :: AddressTypeInEra addrtype era
    -> Address addrtype
    -> AddressInEra era

instance NFData (AddressInEra era) where
  rnf (AddressInEra t a) = deepseq (deepseq t a) ()

instance IsCardanoEra era => ToJSON (AddressInEra era) where
  toJSON = Aeson.String . serialiseAddress

instance IsShelleyBasedEra era => FromJSON (AddressInEra era) where
  parseJSON =
    let sbe = shelleyBasedEra @era
     in withText "AddressInEra" $ \txt -> do
          addressAny <- P.runParserFail parseAddressAny txt
          pure $ anyAddressInShelleyBasedEra sbe addressAny

-- | Parser for any address, supports both bech32 and base58 encodings
parseAddressAny :: SerialiseAddress addr => P.Parser addr
parseAddressAny = do
  str <- lexPlausibleAddressString
  case deserialiseAddress asType str of
    Nothing -> fail $ "invalid address: " <> Text.unpack str
    Just addr -> pure addr
 where
  lexPlausibleAddressString :: P.Parser Text
  lexPlausibleAddressString =
    Text.pack <$> P.many1 (P.satisfy isPlausibleAddressChar)
  -- Covers both base58 and bech32 (with constrained prefixes)
  isPlausibleAddressChar c =
    isAsciiLower c
      || isAsciiUpper c
      || isDigit c
      || c == '_'

instance Eq (AddressInEra era) where
  (==)
    (AddressInEra ByronAddressInAnyEra addr1)
    (AddressInEra ByronAddressInAnyEra addr2) = addr1 == addr2
  (==)
    (AddressInEra ShelleyAddressInEra{} addr1)
    (AddressInEra ShelleyAddressInEra{} addr2) = addr1 == addr2
  (==)
    (AddressInEra ByronAddressInAnyEra _)
    (AddressInEra ShelleyAddressInEra{} _) = False
  (==)
    (AddressInEra ShelleyAddressInEra{} _)
    (AddressInEra ByronAddressInAnyEra _) = False

instance Ord (AddressInEra era) where
  compare
    (AddressInEra ByronAddressInAnyEra addr1)
    (AddressInEra ByronAddressInAnyEra addr2) = compare addr1 addr2
  compare
    (AddressInEra ShelleyAddressInEra{} addr1)
    (AddressInEra ShelleyAddressInEra{} addr2) = compare addr1 addr2
  compare
    (AddressInEra ByronAddressInAnyEra _)
    (AddressInEra ShelleyAddressInEra{} _) = LT
  compare
    (AddressInEra ShelleyAddressInEra{} _)
    (AddressInEra ByronAddressInAnyEra _) = GT

deriving instance Show (AddressInEra era)

data AddressTypeInEra addrtype era where
  ByronAddressInAnyEra :: AddressTypeInEra ByronAddr era
  ShelleyAddressInEra
    :: ShelleyBasedEra era
    -> AddressTypeInEra ShelleyAddr era

deriving instance Show (AddressTypeInEra addrtype era)

instance NFData (AddressTypeInEra addrtype era) where
  rnf = \case
    ByronAddressInAnyEra -> ()
    ShelleyAddressInEra sbe -> deepseq sbe ()

instance HasTypeProxy era => HasTypeProxy (AddressInEra era) where
  data AsType (AddressInEra era) = AsAddressInEra (AsType era)
  proxyToAsType _ = AsAddressInEra (proxyToAsType (Proxy :: Proxy era))

instance IsCardanoEra era => SerialiseAsRawBytes (AddressInEra era) where
  serialiseToRawBytes (AddressInEra ByronAddressInAnyEra addr) =
    serialiseToRawBytes addr
  serialiseToRawBytes (AddressInEra ShelleyAddressInEra{} addr) =
    serialiseToRawBytes addr

  deserialiseFromRawBytes _ bs =
    first (const (SerialiseAsRawBytesError "Unable to deserialise AddressInEra era")) $
      anyAddressInEra cardanoEra
        =<< first unSerialiseAsRawBytesError (deserialiseFromRawBytes AsAddressAny bs)

instance IsCardanoEra era => SerialiseAddress (AddressInEra era) where
  serialiseAddress (AddressInEra ByronAddressInAnyEra addr) =
    serialiseAddress addr
  serialiseAddress (AddressInEra ShelleyAddressInEra{} addr) =
    serialiseAddress addr

  deserialiseAddress _ t =
    rightToMaybe . anyAddressInEra cardanoEra =<< deserialiseAddress AsAddressAny t

byronAddressInEra :: Address ByronAddr -> AddressInEra era
byronAddressInEra = AddressInEra ByronAddressInAnyEra

shelleyAddressInEra
  :: ()
  => ShelleyBasedEra era
  -> Address ShelleyAddr
  -> AddressInEra era
shelleyAddressInEra sbe =
  shelleyBasedEraConstraints sbe $
    AddressInEra (ShelleyAddressInEra sbe)

anyAddressInShelleyBasedEra
  :: ()
  => ShelleyBasedEra era
  -> AddressAny
  -> AddressInEra era
anyAddressInShelleyBasedEra sbe = \case
  AddressByron addr -> byronAddressInEra addr
  AddressShelley addr -> shelleyAddressInEra sbe addr

anyAddressInEra
  :: CardanoEra era
  -> AddressAny
  -> Either String (AddressInEra era)
anyAddressInEra era = \case
  AddressByron addr ->
    pure $ AddressInEra ByronAddressInAnyEra addr
  AddressShelley addr -> do
    sbe <- forEraMaybeEon era ?! "Expected Byron based era address"
    shelleyBasedEraConstraints sbe $
      pure $
        AddressInEra (ShelleyAddressInEra sbe) addr

toAddressAny :: Address addr -> AddressAny
toAddressAny a@ShelleyAddress{} = AddressShelley a
toAddressAny a@ByronAddress{} = AddressByron a

makeByronAddressInEra
  :: NetworkId
  -> VerificationKey ByronKey
  -> AddressInEra era
makeByronAddressInEra nw vk =
  byronAddressInEra (makeByronAddress nw vk)

makeShelleyAddressInEra
  :: ()
  => ShelleyBasedEra era
  -> NetworkId
  -> PaymentCredential
  -> StakeAddressReference
  -> AddressInEra era
makeShelleyAddressInEra sbe nw pc scr =
  shelleyAddressInEra sbe (makeShelleyAddress nw pc scr)

-- ----------------------------------------------------------------------------
-- Stake addresses
--

data StakeAddress where
  StakeAddress
    :: Shelley.Network
    -> Shelley.StakeCredential
    -> StakeAddress
  deriving (Eq, Ord, Show)

data PaymentCredential
  = PaymentCredentialByKey (Hash PaymentKey)
  | PaymentCredentialByScript ScriptHash
  deriving (Eq, Ord, Show)

data StakeCredential
  = StakeCredentialByKey (Hash StakeKey)
  | StakeCredentialByScript ScriptHash
  deriving (Eq, Ord, Show)

instance ToJSON StakeCredential where
  toJSON =
    Aeson.object
      . \case
        StakeCredentialByKey keyHash ->
          ["stakingKeyHash" .= serialiseToRawBytesHexText keyHash]
        StakeCredentialByScript scriptHash ->
          ["stakingScriptHash" .= serialiseToRawBytesHexText scriptHash]

data StakeAddressReference
  = StakeAddressByValue StakeCredential
  | StakeAddressByPointer StakeAddressPointer
  | NoStakeAddress
  deriving (Eq, Show)

newtype StakeAddressPointer = StakeAddressPointer
  { unStakeAddressPointer :: Shelley.Ptr
  }
  deriving (Eq, Show)

instance HasTypeProxy StakeAddress where
  data AsType StakeAddress = AsStakeAddress
  proxyToAsType _ = AsStakeAddress

instance SerialiseAsRawBytes StakeAddress where
  serialiseToRawBytes (StakeAddress nw sc) =
    Shelley.serialiseRewardAccount (Shelley.RewardAccount nw sc)

  deserialiseFromRawBytes AsStakeAddress bs =
    case Shelley.deserialiseRewardAccount bs of
      Nothing -> Left (SerialiseAsRawBytesError "Unable to deserialise StakeAddress")
      Just (Shelley.RewardAccount nw sc) -> Right (StakeAddress nw sc)

instance SerialiseAsBech32 StakeAddress where
  bech32PrefixFor (StakeAddress Shelley.Mainnet _) = unsafeHumanReadablePartFromText "stake"
  bech32PrefixFor (StakeAddress Shelley.Testnet _) = unsafeHumanReadablePartFromText "stake_test"

  bech32PrefixesPermitted AsStakeAddress = unsafeHumanReadablePartFromText <$> ["stake", "stake_test"]

instance SerialiseAddress StakeAddress where
  serialiseAddress addr@StakeAddress{} =
    serialiseToBech32 addr

  deserialiseAddress AsStakeAddress t =
    either (const Nothing) Just $
      deserialiseFromBech32 t

instance ToJSON StakeAddress where
  toJSON s = Aeson.String $ serialiseAddress s

instance FromJSON StakeAddress where
  parseJSON = withText "StakeAddress" $ \str ->
    case deserialiseAddress AsStakeAddress str of
      Nothing ->
        fail $ "Error while deserialising StakeAddress: " <> Text.unpack str
      Just sAddr -> pure sAddr

makeStakeAddress
  :: NetworkId
  -> StakeCredential
  -> StakeAddress
makeStakeAddress nw sc =
  StakeAddress
    (toShelleyNetwork nw)
    (toShelleyStakeCredential sc)

-- ----------------------------------------------------------------------------
-- Helpers
--

-- | Is the UTxO at the address only spendable via a key witness.
isKeyAddress :: AddressInEra era -> Bool
isKeyAddress (AddressInEra ByronAddressInAnyEra _) = True
isKeyAddress (AddressInEra (ShelleyAddressInEra _) (ShelleyAddress _ pCred _)) =
  case fromShelleyPaymentCredential pCred of
    PaymentCredentialByKey _ -> True
    PaymentCredentialByScript _ -> False

-- | Converts a Shelley payment address to a Plutus public key hash.
shelleyPayAddrToPlutusPubKHash :: Address ShelleyAddr -> Maybe PlutusAPI.PubKeyHash
shelleyPayAddrToPlutusPubKHash (ShelleyAddress _ payCred _) =
  case payCred of
    Shelley.ScriptHashObj _ -> Nothing
    Shelley.KeyHashObj kHash -> Just $ Plutus.transKeyHash kHash

-- ----------------------------------------------------------------------------
-- Internal conversion functions
--

toShelleyAddr :: AddressInEra era -> Shelley.Addr
toShelleyAddr (AddressInEra ByronAddressInAnyEra (ByronAddress addr)) =
  Shelley.AddrBootstrap (Shelley.BootstrapAddress addr)
toShelleyAddr
  ( AddressInEra
      (ShelleyAddressInEra _)
      (ShelleyAddress nw pc scr)
    ) =
    Shelley.Addr nw pc scr

toShelleyStakeAddr :: StakeAddress -> Shelley.RewardAccount
toShelleyStakeAddr (StakeAddress nw sc) =
  Shelley.RewardAccount
    { Shelley.raNetwork = nw
    , Shelley.raCredential = sc
    }

toShelleyPaymentCredential
  :: PaymentCredential
  -> Shelley.PaymentCredential
toShelleyPaymentCredential (PaymentCredentialByKey (PaymentKeyHash kh)) =
  Shelley.KeyHashObj kh
toShelleyPaymentCredential (PaymentCredentialByScript sh) =
  Shelley.ScriptHashObj (toShelleyScriptHash sh)

toShelleyStakeCredential
  :: StakeCredential
  -> Shelley.StakeCredential
toShelleyStakeCredential (StakeCredentialByKey (StakeKeyHash kh)) =
  Shelley.KeyHashObj kh
toShelleyStakeCredential (StakeCredentialByScript sh) =
  Shelley.ScriptHashObj (toShelleyScriptHash sh)

toShelleyStakeReference
  :: StakeAddressReference
  -> Shelley.StakeReference
toShelleyStakeReference (StakeAddressByValue stakecred) =
  Shelley.StakeRefBase (toShelleyStakeCredential stakecred)
toShelleyStakeReference (StakeAddressByPointer ptr) =
  Shelley.StakeRefPtr (unStakeAddressPointer ptr)
toShelleyStakeReference NoStakeAddress =
  Shelley.StakeRefNull

fromShelleyAddrIsSbe
  :: ()
  => ShelleyBasedEra era
  -> Shelley.Addr
  -> AddressInEra era
fromShelleyAddrIsSbe sbe = \case
  Shelley.AddrBootstrap (Shelley.BootstrapAddress addr) ->
    AddressInEra ByronAddressInAnyEra (ByronAddress addr)
  Shelley.Addr nw pc scr ->
    shelleyBasedEraConstraints sbe $
      AddressInEra (ShelleyAddressInEra sbe) (ShelleyAddress nw pc scr)

fromShelleyAddr
  :: ShelleyBasedEra era
  -> Shelley.Addr
  -> AddressInEra era
fromShelleyAddr _ (Shelley.AddrBootstrap (Shelley.BootstrapAddress addr)) =
  AddressInEra ByronAddressInAnyEra (ByronAddress addr)
fromShelleyAddr sbe (Shelley.Addr nw pc scr) =
  shelleyBasedEraConstraints sbe $
    AddressInEra
      (ShelleyAddressInEra sbe)
      (ShelleyAddress nw pc scr)

fromShelleyStakeAddr :: Shelley.RewardAccount -> StakeAddress
fromShelleyStakeAddr (Shelley.RewardAccount nw sc) = StakeAddress nw sc

fromShelleyStakeCredential
  :: Shelley.StakeCredential
  -> StakeCredential
fromShelleyStakeCredential (Shelley.KeyHashObj kh) =
  StakeCredentialByKey (StakeKeyHash kh)
fromShelleyStakeCredential (Shelley.ScriptHashObj sh) =
  StakeCredentialByScript (fromShelleyScriptHash sh)

fromShelleyPaymentCredential
  :: Shelley.PaymentCredential
  -> PaymentCredential
fromShelleyPaymentCredential (Shelley.KeyHashObj kh) =
  PaymentCredentialByKey (PaymentKeyHash kh)
fromShelleyPaymentCredential (Shelley.ScriptHashObj sh) =
  PaymentCredentialByScript (ScriptHash sh)

fromShelleyStakeReference
  :: Shelley.StakeReference
  -> StakeAddressReference
fromShelleyStakeReference (Shelley.StakeRefBase stakecred) =
  StakeAddressByValue (fromShelleyStakeCredential stakecred)
fromShelleyStakeReference (Shelley.StakeRefPtr ptr) =
  StakeAddressByPointer (StakeAddressPointer ptr)
fromShelleyStakeReference Shelley.StakeRefNull =
  NoStakeAddress

-- | Get a stake credential from a stake address.
-- This drops the network information.
stakeAddressCredential :: StakeAddress -> StakeCredential
stakeAddressCredential (StakeAddress _ scred) = fromShelleyStakeCredential scred
