{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Consensus modes. The node supports several different modes with different
-- combinations of consensus protocols and ledger eras.
module Cardano.Api.Consensus.Internal.Mode
  ( -- * The protocols supported in each era
    ConsensusProtocol
  , ChainDepStateProtocol

    -- * Connection parameters for each mode
  , ConsensusModeParams (..)
  , Byron.EpochSlots (..)

    -- * Conversions to and from types in the consensus library
  , ConsensusCryptoForBlock
  , ConsensusBlockForEra
  , toConsensusEraIndex
  , fromConsensusEraIndex
  )
where

import Cardano.Api.Era.Internal.Core

import Cardano.Chain.Slotting qualified as Byron (EpochSlots (..))
import Cardano.Protocol.Crypto (StandardCrypto)
import Ouroboros.Consensus.Byron.ByronHFC qualified as Consensus
import Ouroboros.Consensus.Byron.Ledger qualified as Consensus
import Ouroboros.Consensus.Cardano.Block qualified as Consensus
import Ouroboros.Consensus.HardFork.Combinator as Consensus
  ( EraIndex (..)
  , eraIndexSucc
  , eraIndexZero
  )
import Ouroboros.Consensus.Protocol.Praos qualified as Consensus
import Ouroboros.Consensus.Protocol.TPraos qualified as Consensus
import Ouroboros.Consensus.Shelley.HFEras qualified as Consensus
import Ouroboros.Consensus.Shelley.ShelleyHFC qualified as Consensus

import Data.SOP (K (K))
import Data.SOP.Strict (NS (S, Z))

-- ----------------------------------------------------------------------------
-- Consensus modes
--

-- | The consensus-mode-specific parameters needed to connect to a local node
-- that is using each consensus mode.
--
-- It is in fact only the Byron era that requires extra parameters, but this is
-- of course inherited by the 'CardanoMode' that uses the Byron era. The reason
-- this parameter is needed stems from unfortunate design decisions from the
-- legacy Byron era. The slots per epoch are needed to be able to /decode/
-- epoch boundary blocks from the Byron era.
--
-- It is possible in future that we may be able to eliminate this parameter by
-- discovering it from the node during the initial handshake.
data ConsensusModeParams where
  CardanoModeParams
    :: Byron.EpochSlots
    -> ConsensusModeParams

deriving instance Show ConsensusModeParams

-- ----------------------------------------------------------------------------
-- Consensus conversion functions
--

-- | A closed type family that maps between the consensus mode (from this API)
-- and the block type used by the consensus libraries.
type family ConsensusBlockForEra era where
  ConsensusBlockForEra ByronEra = Consensus.ByronBlock
  ConsensusBlockForEra ShelleyEra = Consensus.StandardShelleyBlock
  ConsensusBlockForEra AllegraEra = Consensus.StandardAllegraBlock
  ConsensusBlockForEra MaryEra = Consensus.StandardMaryBlock
  ConsensusBlockForEra AlonzoEra = Consensus.StandardAlonzoBlock
  ConsensusBlockForEra BabbageEra = Consensus.StandardBabbageBlock
  ConsensusBlockForEra ConwayEra = Consensus.StandardConwayBlock
  ConsensusBlockForEra DijkstraEra = Consensus.StandardDijkstraBlock

type family ConsensusCryptoForBlock block where
  ConsensusCryptoForBlock Consensus.ByronBlockHFC = StandardCrypto
  ConsensusCryptoForBlock
    (Consensus.ShelleyBlockHFC (Consensus.TPraos StandardCrypto) Consensus.ShelleyEra) =
    Consensus.ShelleyEra
  ConsensusCryptoForBlock (Consensus.CardanoBlock StandardCrypto) = StandardCrypto

type family ConsensusProtocol era where
  ConsensusProtocol ShelleyEra = Consensus.TPraos StandardCrypto
  ConsensusProtocol AllegraEra = Consensus.TPraos StandardCrypto
  ConsensusProtocol MaryEra = Consensus.TPraos StandardCrypto
  ConsensusProtocol AlonzoEra = Consensus.TPraos StandardCrypto
  ConsensusProtocol BabbageEra = Consensus.Praos StandardCrypto
  ConsensusProtocol ConwayEra = Consensus.Praos StandardCrypto
  ConsensusProtocol DijkstraEra = Consensus.Praos StandardCrypto

type family ChainDepStateProtocol era where
  ChainDepStateProtocol ShelleyEra = Consensus.TPraosState
  ChainDepStateProtocol AllegraEra = Consensus.TPraosState
  ChainDepStateProtocol MaryEra = Consensus.TPraosState
  ChainDepStateProtocol AlonzoEra = Consensus.TPraosState
  ChainDepStateProtocol BabbageEra = Consensus.PraosState
  ChainDepStateProtocol ConwayEra = Consensus.PraosState

eraIndex0 :: Consensus.EraIndex (x0 : xs)
eraIndex0 = Consensus.eraIndexZero

eraIndex1 :: Consensus.EraIndex (x1 : x0 : xs)
eraIndex1 = eraIndexSucc eraIndex0

eraIndex2 :: Consensus.EraIndex (x2 : x1 : x0 : xs)
eraIndex2 = eraIndexSucc eraIndex1

eraIndex3 :: Consensus.EraIndex (x3 : x2 : x1 : x0 : xs)
eraIndex3 = eraIndexSucc eraIndex2

eraIndex4 :: Consensus.EraIndex (x4 : x3 : x2 : x1 : x0 : xs)
eraIndex4 = eraIndexSucc eraIndex3

eraIndex5 :: Consensus.EraIndex (x5 : x4 : x3 : x2 : x1 : x0 : xs)
eraIndex5 = eraIndexSucc eraIndex4

eraIndex6 :: Consensus.EraIndex (x6 : x5 : x4 : x3 : x2 : x1 : x0 : xs)
eraIndex6 = eraIndexSucc eraIndex5

eraIndex7 :: Consensus.EraIndex (x7 : x6 : x5 : x4 : x3 : x2 : x1 : x0 : xs)
eraIndex7 = eraIndexSucc eraIndex6

toConsensusEraIndex
  :: ()
  => Consensus.CardanoBlock StandardCrypto ~ Consensus.HardForkBlock xs
  => CardanoEra era
  -> Consensus.EraIndex xs
toConsensusEraIndex = \case
  ByronEra -> eraIndex0
  ShelleyEra -> eraIndex1
  AllegraEra -> eraIndex2
  MaryEra -> eraIndex3
  AlonzoEra -> eraIndex4
  BabbageEra -> eraIndex5
  ConwayEra -> eraIndex6
  DijkstraEra -> eraIndex7

fromConsensusEraIndex
  :: ()
  => Consensus.EraIndex (Consensus.CardanoEras StandardCrypto)
  -> AnyCardanoEra
fromConsensusEraIndex = \case
  Consensus.EraIndex (Z (K ())) ->
    AnyCardanoEra ByronEra
  Consensus.EraIndex (S (Z (K ()))) ->
    AnyCardanoEra ShelleyEra
  Consensus.EraIndex (S (S (Z (K ())))) ->
    AnyCardanoEra AllegraEra
  Consensus.EraIndex (S (S (S (Z (K ()))))) ->
    AnyCardanoEra MaryEra
  Consensus.EraIndex (S (S (S (S (Z (K ())))))) ->
    AnyCardanoEra AlonzoEra
  Consensus.EraIndex (S (S (S (S (S (Z (K ()))))))) ->
    AnyCardanoEra BabbageEra
  Consensus.EraIndex (S (S (S (S (S (S (Z (K ())))))))) ->
    AnyCardanoEra ConwayEra
  Consensus.EraIndex (S (S (S (S (S (S (S _))))))) -> 
    AnyCardanoEra DijkstraEra