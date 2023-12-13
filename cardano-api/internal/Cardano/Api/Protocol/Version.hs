{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}



module Cardano.Api.Protocol.Version where

import           Cardano.Api.Eon.ShelleyBasedEra
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

type SupportedProtocolVersionRange version =
    ( MinSupportedVersion <= version
    , version <= MaxSupportedVersion
    )

data SomeProtocolVersion version where
  CurrentProtocolVersion
    :: SupportedProtocolVersionRange BabbageEra
    => SomeProtocolVersion BabbageEra
  ExperimentalProtocolVersion
    :: SupportedProtocolVersionRange ConwayEra
    => SomeProtocolVersion ConwayEra


type family VersionToEra version where
  VersionToEra BabbageEra = Api.BabbageEra
  VersionToEra ConwayEra = Api.ConwayEra

protocolVersionToSbe
  :: SomeProtocolVersion version
  -> ShelleyBasedEra (VersionToEra version)
protocolVersionToSbe CurrentProtocolVersion = ShelleyBasedEraBabbage
protocolVersionToSbe ExperimentalProtocolVersion = ShelleyBasedEraConway

-- An Example
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
    CurrentProtocolVersion ->
      guardShelleyTxInsOverflow (map fst txIns)
    ExperimentalProtocolVersion -> pure ()

