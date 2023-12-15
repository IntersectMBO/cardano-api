{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}



module Cardano.Api.Protocol.Version where

import           Cardano.Api.Eon.ShelleyBasedEra (ShelleyBasedEra (..))
import qualified Cardano.Api.Eras.Core as Api
import           Cardano.Api.Script
import           Cardano.Api.TxBody

import qualified Data.Set as Set
import           GHC.TypeLits

-- Users interacting with Cardano are likely only interested in using the latest
-- features available on mainnet and experimenting with the upcoming era as this becomes
-- available. Therefore we restrict the choices of protocol version to what is currently
-- on mainnet and what is in the upcoming era.

-- | Minimum supported version. Corresponds to Babbage era.
type MinSupportedVersion = 8 :: Nat

-- | Maximum supported version. Corresponds to Conway era.
type MaxSupportedVersion = 9 :: Nat

type BabbageEra = 8 :: Nat
type ConwayEra = 9 :: Nat
type PostConwayEra = 10 :: Nat

type SupportedProtocolVersionRange (version :: Nat) =
    ( MinSupportedVersion <= version
    , version <= MaxSupportedVersion
    )

-- Will eventually disappear
type family RequiresCurrent (version :: Nat) = era | era -> version where
  RequiresCurrent BabbageEra = Api.BabbageEra

data SomeProtocolVersion (version :: Nat) where
  CurrentProtocolVersion
    :: SupportedProtocolVersionRange BabbageEra
    => SomeProtocolVersion BabbageEra
  UpcomingProtocolVersion
    :: SupportedProtocolVersionRange ConwayEra
    => SomeProtocolVersion ConwayEra


type family VersionToEra (version :: Nat) where
  VersionToEra BabbageEra = Api.BabbageEra
  VersionToEra ConwayEra = Api.ConwayEra

protocolVersionToSbe
  :: SomeProtocolVersion version
  -> ShelleyBasedEra (VersionToEra version)
protocolVersionToSbe CurrentProtocolVersion = ShelleyBasedEraBabbage
protocolVersionToSbe UpcomingProtocolVersion = ShelleyBasedEraConway

-- An Example. Functions exposed to users should be generic in version.
validateTxBodyContent'
  :: SomeProtocolVersion version
  -> TxBodyContent BuildTx (VersionToEra version)
  -> Either TxBodyError ()
validateTxBodyContent' p txBodContent@TxBodyContent {
                             txIns,
                             txInsCollateral,
                             txOuts,
                             txProtocolParams,
                             txMintValue,
                             txMetadata} = do

  let sbe = protocolVersionToSbe p
      witnesses = collectTxBodyScriptWitnesses sbe txBodContent
      languages = Set.fromList
                    [ toAlonzoLanguage (AnyPlutusScriptVersion v)
                    | (_, AnyScriptWitness (PlutusScriptWitness _ v _ _ _ _)) <- witnesses
                    ]

  validateTxIns txIns
  validateTxOuts sbe txOuts
  validateMetadata txMetadata
  validateMintValue txMintValue
  validateTxInsCollateral txInsCollateral languages
  validateProtocolParameters txProtocolParams languages

  case p of
    CurrentProtocolVersion -> do
      guardShelleyTxInsOverflow (map fst txIns)
      validateTxIns' p txIns
    UpcomingProtocolVersion -> pure ()

-- RequiresCurrent allows modification of existing cardano-api until the
-- refactor is complete. Note that functions which are not era dependent will
-- not have SomeProtocolVersion as a parameter.
validateTxIns'
  :: SomeProtocolVersion BabbageEra
  -> [(TxIn, BuildTxWith BuildTx (Witness WitCtxTxIn (RequiresCurrent BabbageEra)))]
  -> Either TxBodyError ()
validateTxIns' _ _txIns =
  sequence_ [
            ]


newtype UpdatedWitness (version :: Nat) = UpdatedWitness ()

-- For functionality specific to an era we use concrete types
futureValidateTxIns
  :: SomeProtocolVersion BabbageEra
  -> [(TxIn, BuildTxWith BuildTx (UpdatedWitness BabbageEra))]
  -> Either TxBodyError ()
futureValidateTxIns p _txIns =
  case p of
    CurrentProtocolVersion -> sequence_ []

-- This will give a type error when we update CurrentProtocolVersion BabbageEra
-- to CurrentProtocolVersion ConwayEra
example
  :: SomeProtocolVersion version
  -> [(TxIn, BuildTxWith BuildTx (UpdatedWitness version))]
  -> Either TxBodyError ()
example p' txins =
  case p' of
    CurrentProtocolVersion -> futureValidateTxIns p' txins
    UpcomingProtocolVersion -> pure ()
