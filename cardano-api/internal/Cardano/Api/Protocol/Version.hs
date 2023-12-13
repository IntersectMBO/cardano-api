{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

-- | This module defines the protocol versions corresponding to the eras in the Cardano blockchain.
module Cardano.Api.Protocol.Version
  ( BabbageEra
  , ConwayEra
  , Era (..)
  , MaxSupportedVersion
  , MinSupportedVersion
  , SupportedProtocolVersionRange
  , UseEra
  , VersionToSbe
  , getProtocolVersion
  , protocolVersionToSbe
  ) where

import           Cardano.Api.Eon.ShelleyBasedEra (ShelleyBasedEra (..))
import qualified Cardano.Api.Eras.Core as Api

import           GHC.TypeLits

-- | Users typically interact with the latest features on the mainnet or experiment with features
-- from the upcoming era. Hence, the protocol versions are limited to the current mainnet era
-- and the next era (upcoming era).
--
-- - 'MinSupportedVersion': Represents the minimum protocol version, aligning with the current
--   era on mainnet (Babbage era).
-- - 'MaxSupportedVersion': Represents the maximum protocol version, corresponding to the next
--   era planned for Cardano's mainnet (Conway era).

-- | The minimum supported protocol version, corresponding to the Babbage era on Cardano's mainnet.
type MinSupportedVersion = 8 :: Nat

-- | The maximum supported protocol version, representing the upcoming Conway era.
type MaxSupportedVersion = 9 :: Nat

type BabbageEra = 8 :: Nat
type ConwayEra = 9 :: Nat

type SupportedProtocolVersionRange (version :: Nat) =
    ( MinSupportedVersion <= version
    , version <= MaxSupportedVersion
    )

-- Allows us to gradually change the api without breaking things.
-- This will eventually be removed.
type family VersionToSbe (version :: Nat) where
  VersionToSbe BabbageEra = Api.BabbageEra
  VersionToSbe ConwayEra = Api.ConwayEra

-- | Represents the eras in Cardano's blockchain history.
--
-- Instead of enumerating every possible era, we use two constructors:
-- 'CurrentEra' and 'UpcomingEra'. This design simplifies the handling
-- of eras, especially for 'cardano-api' consumers who are primarily concerned
-- with the current mainnet era and the next era for an upcoming hardfork.
--
-- Usage:
-- - 'CurrentEra': Reflects the era currently active on mainnet.
-- - 'UpcomingEra': Represents the era planned for the next hardfork.
--
-- After a hardfork, 'cardano-api' should be updated promptly to reflect
-- the new mainnet era in 'CurrentEra'.
--
-- Each era is associated with a specific protocol version:
-- - 'BabbageEra' for 'CurrentEra'
-- - 'ConwayEra' for 'UpcomingEra'
data Era (version :: Nat) where
  -- | The era currently active on Cardano's mainnet.
  CurrentEra
    :: SupportedProtocolVersionRange BabbageEra
    => Era BabbageEra
  -- | The era planned for the next hardfork on Cardano's mainnet.
  UpcomingEra
    :: SupportedProtocolVersionRange ConwayEra
    => Era ConwayEra


protocolVersionToSbe
  :: Era version
  -> ShelleyBasedEra (VersionToSbe version)
protocolVersionToSbe CurrentEra = ShelleyBasedEraBabbage
protocolVersionToSbe UpcomingEra = ShelleyBasedEraConway

-------------------------------------------------------------------------

-- | Type class interface for the 'Era' type.

class UseEra (version :: Nat) where
  getProtocolVersion :: Era version

instance UseEra BabbageEra where
  getProtocolVersion = CurrentEra

instance UseEra ConwayEra where
  getProtocolVersion = UpcomingEra
