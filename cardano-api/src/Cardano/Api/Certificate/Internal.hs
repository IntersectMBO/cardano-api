{-# LANGUAGE NamedFieldPuns #-}

-- | Stake pool parameters, relays, and metadata references used by transaction
-- certificates and queries. The era-parameterized certificate type itself lives
-- in "Cardano.Api.Experimental.Tx.Internal.Certificate".
module Cardano.Api.Certificate.Internal
  ( PoolId

    -- * Registering stake pools
  , StakePoolParameters (..)
  , StakePoolRelay (..)
  , StakePoolMetadataReference (..)

    -- * Internal conversion functions
  , toShelleyPoolParams
  , fromShelleyPoolParams
  , fromShelleyStakePoolState
  )
where

import Cardano.Api.Address
import Cardano.Api.Certificate.Internal.StakePoolMetadata
import Cardano.Api.Key.Internal
import Cardano.Api.Key.Internal.Praos
import Cardano.Api.Ledger.Internal.Reexport qualified as Ledger

import Cardano.Base.IP (mkIPv4, mkIPv6, unIPv4, unIPv6)
import Cardano.Ledger.Coin qualified as L
import Cardano.Ledger.State qualified as Ledger

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as SBS
import Data.IP (IPv4, IPv6)
import Data.Maybe
import Data.MemPack.Buffer (byteArrayFromShortByteString, byteArrayToShortByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import GHC.Exts (IsList (..))
import Network.Socket (PortNumber)

-- ----------------------------------------------------------------------------
-- Stake pool parameters
--

type PoolId = Hash StakePoolKey

data StakePoolParameters
  = StakePoolParameters
  { stakePoolId :: PoolId
  , stakePoolVRF :: Hash VrfKey
  , stakePoolCost :: L.Coin
  , stakePoolMargin :: Rational
  , stakePoolRewardAccount :: StakeAddress
  , stakePoolPledge :: L.Coin
  , stakePoolOwners :: [Hash StakeKey]
  , stakePoolRelays :: [StakePoolRelay]
  , stakePoolMetadata :: Maybe StakePoolMetadataReference
  }
  deriving (Eq, Show)

data StakePoolRelay
  = -- | One or both of IPv4 & IPv6
    StakePoolRelayIp
      (Maybe IPv4)
      (Maybe IPv6)
      (Maybe PortNumber)
  | -- | An DNS name pointing to a @A@ or @AAAA@ record.
    StakePoolRelayDnsARecord
      ByteString
      (Maybe PortNumber)
  | -- | A DNS name pointing to a @SRV@ record.
    StakePoolRelayDnsSrvRecord
      ByteString
  deriving (Eq, Show)

data StakePoolMetadataReference
  = StakePoolMetadataReference
  { stakePoolMetadataURL :: Text
  , stakePoolMetadataHash :: Hash StakePoolMetadata
  }
  deriving (Eq, Show)

-- ----------------------------------------------------------------------------
-- Internal conversion functions
--

toShelleyPoolParams :: StakePoolParameters -> Ledger.StakePoolParams
toShelleyPoolParams
  StakePoolParameters
    { stakePoolId = StakePoolKeyHash poolkh
    , stakePoolVRF = VrfKeyHash vrfkh
    , stakePoolCost
    , stakePoolMargin
    , stakePoolRewardAccount
    , stakePoolPledge
    , stakePoolOwners
    , stakePoolRelays
    , stakePoolMetadata
    } =
    -- TODO: validate pool parameters such as the PoolMargin below, but also
    -- do simple client-side sanity checks, e.g. on the pool metadata url
    Ledger.StakePoolParams
      { Ledger.sppId = poolkh
      , Ledger.sppVrf = Ledger.toVRFVerKeyHash vrfkh
      , Ledger.sppPledge = stakePoolPledge
      , Ledger.sppCost = stakePoolCost
      , Ledger.sppMargin =
          fromMaybe
            (error "toShelleyPoolParams: invalid PoolMargin")
            (Ledger.boundRational stakePoolMargin)
      , Ledger.sppAccountAddress = toShelleyStakeAddr stakePoolRewardAccount
      , Ledger.sppOwners =
          fromList
            [kh | StakeKeyHash kh <- stakePoolOwners]
      , Ledger.sppRelays =
          fromList
            (map toShelleyStakePoolRelay stakePoolRelays)
      , Ledger.sppMetadata =
          toShelleyPoolMetadata
            <$> Ledger.maybeToStrictMaybe stakePoolMetadata
      }
   where
    toShelleyStakePoolRelay :: StakePoolRelay -> Ledger.StakePoolRelay
    toShelleyStakePoolRelay (StakePoolRelayIp mipv4 mipv6 mport) =
      Ledger.SingleHostAddr
        (fromIntegral <$> Ledger.maybeToStrictMaybe mport)
        (Ledger.maybeToStrictMaybe (mkIPv4 <$> mipv4))
        (Ledger.maybeToStrictMaybe (mkIPv6 <$> mipv6))
    toShelleyStakePoolRelay (StakePoolRelayDnsARecord dnsname mport) =
      Ledger.SingleHostName
        (fromIntegral <$> Ledger.maybeToStrictMaybe mport)
        (toShelleyDnsName dnsname)
    toShelleyStakePoolRelay (StakePoolRelayDnsSrvRecord dnsname) =
      Ledger.MultiHostName
        (toShelleyDnsName dnsname)

    toShelleyPoolMetadata :: StakePoolMetadataReference -> Ledger.PoolMetadata
    toShelleyPoolMetadata
      StakePoolMetadataReference
        { stakePoolMetadataURL
        , stakePoolMetadataHash = StakePoolMetadataHash mdh
        } =
        Ledger.PoolMetadata
          { Ledger.pmUrl = toShelleyUrl stakePoolMetadataURL
          , Ledger.pmHash = byteArrayFromShortByteString . SBS.toShort $ Ledger.hashToBytes mdh
          }

    toShelleyDnsName :: ByteString -> Ledger.DnsName
    toShelleyDnsName name =
      fromMaybe (error "toShelleyDnsName: invalid dns name. TODO: proper validation")
        . Ledger.textToDns (BS.length name)
        $ Text.decodeLatin1 name

    toShelleyUrl :: Text -> Ledger.Url
    toShelleyUrl url =
      fromMaybe (error "toShelleyUrl: invalid url. TODO: proper validation") $
        Ledger.textToUrl (Text.length url) url

fromShelleyPoolParams
  :: Ledger.StakePoolParams
  -> StakePoolParameters
fromShelleyPoolParams
  Ledger.StakePoolParams
    { Ledger.sppId
    , Ledger.sppVrf
    , Ledger.sppPledge
    , Ledger.sppCost
    , Ledger.sppMargin
    , Ledger.sppAccountAddress
    , Ledger.sppOwners
    , Ledger.sppRelays
    , Ledger.sppMetadata
    } =
    StakePoolParameters
      { stakePoolId = StakePoolKeyHash sppId
      , stakePoolVRF = VrfKeyHash (Ledger.fromVRFVerKeyHash sppVrf)
      , stakePoolCost = sppCost
      , stakePoolMargin = Ledger.unboundRational sppMargin
      , stakePoolRewardAccount = fromShelleyStakeAddr sppAccountAddress
      , stakePoolPledge = sppPledge
      , stakePoolOwners = map StakeKeyHash (toList sppOwners)
      , stakePoolRelays =
          map
            fromShelleyStakePoolRelay
            (toList sppRelays)
      , stakePoolMetadata =
          fromShelleyPoolMetadata
            <$> Ledger.strictMaybeToMaybe sppMetadata
      }
   where
    fromShelleyStakePoolRelay :: Ledger.StakePoolRelay -> StakePoolRelay
    fromShelleyStakePoolRelay (Ledger.SingleHostAddr mport mipv4 mipv6) =
      StakePoolRelayIp
        (fmap unIPv4 (Ledger.strictMaybeToMaybe mipv4))
        (fmap unIPv6 (Ledger.strictMaybeToMaybe mipv6))
        (fromIntegral . Ledger.portToWord16 <$> Ledger.strictMaybeToMaybe mport)
    fromShelleyStakePoolRelay (Ledger.SingleHostName mport dnsname) =
      StakePoolRelayDnsARecord
        (fromShelleyDnsName dnsname)
        (fromIntegral . Ledger.portToWord16 <$> Ledger.strictMaybeToMaybe mport)
    fromShelleyStakePoolRelay (Ledger.MultiHostName dnsname) =
      StakePoolRelayDnsSrvRecord
        (fromShelleyDnsName dnsname)

    fromShelleyPoolMetadata :: Ledger.PoolMetadata -> StakePoolMetadataReference
    fromShelleyPoolMetadata
      Ledger.PoolMetadata
        { Ledger.pmUrl
        , Ledger.pmHash
        } =
        StakePoolMetadataReference
          { stakePoolMetadataURL = Ledger.urlToText pmUrl
          , stakePoolMetadataHash =
              StakePoolMetadataHash
                . fromMaybe (error "fromShelleyPoolMetadata: invalid hash. TODO: proper validation")
                . Ledger.hashFromBytes
                . SBS.fromShort
                . byteArrayToShortByteString
                $ pmHash
          }

    -- TODO: change the ledger rep of the DNS name to use ShortByteString
    fromShelleyDnsName :: Ledger.DnsName -> ByteString
    fromShelleyDnsName =
      Text.encodeUtf8
        . Ledger.dnsToText

fromShelleyStakePoolState
  :: Ledger.Network
  -> Ledger.KeyHash Ledger.StakePool
  -> Ledger.StakePoolState
  -> StakePoolParameters
fromShelleyStakePoolState networkId poolId =
  fromShelleyPoolParams . Ledger.stakePoolStateToStakePoolParams networkId poolId
