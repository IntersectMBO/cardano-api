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
--
module Cardano.Api.Modes (

    -- * Consensus modes
    CardanoMode,
    ConsensusMode(..),
    AnyConsensusMode(..),
    renderMode,

    -- * The eras supported by each mode
    EraInMode(..),
    eraInModeToEra,
    toEraInMode,

    -- * The protocols supported in each era
    ConsensusProtocol,
    ChainDepStateProtocol,

    -- * Connection parameters for each mode
    ConsensusModeParams(..),
    AnyConsensusModeParams(..),
    Byron.EpochSlots(..),

    -- * Conversions to and from types in the consensus library
    ConsensusCryptoForBlock,
    ConsensusBlockForMode,
    ConsensusBlockForEra,
    toConsensusEraIndex,
    fromConsensusEraIndex,
  ) where

import           Cardano.Api.Eras.Core

import qualified Cardano.Chain.Slotting as Byron (EpochSlots (..))
import           Cardano.Ledger.Crypto (StandardCrypto)
import qualified Ouroboros.Consensus.Byron.Ledger as Consensus
import qualified Ouroboros.Consensus.Cardano.Block as Consensus
import qualified Ouroboros.Consensus.Cardano.ByronHFC as Consensus
import           Ouroboros.Consensus.HardFork.Combinator as Consensus (EraIndex (..), eraIndexSucc,
                   eraIndexZero)
import qualified Ouroboros.Consensus.Protocol.Praos as Consensus
import qualified Ouroboros.Consensus.Protocol.TPraos as Consensus
import qualified Ouroboros.Consensus.Shelley.HFEras as Consensus
import qualified Ouroboros.Consensus.Shelley.ShelleyHFC as Consensus

import           Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value)
import           Data.Aeson.Types (Parser, prependFailure, typeMismatch)
import           Data.SOP (K (K))
import           Data.SOP.Strict (NS (S, Z))
import           Data.Text (Text)

-- ----------------------------------------------------------------------------
-- Consensus modes
--

-- | The Cardano consensus mode consists of all the eras currently in use on
-- the Cardano mainnet. This is currently: the 'ByronEra'; 'ShelleyEra',
-- 'AllegraEra' and 'MaryEra', in that order.
--
-- This mode will be extended with new eras as the Cardano mainnet develops.
--
data CardanoMode

data AnyConsensusModeParams where
  AnyConsensusModeParams :: ConsensusModeParams mode -> AnyConsensusModeParams

deriving instance Show AnyConsensusModeParams

-- | This GADT provides a value-level representation of all the consensus modes.
-- This enables pattern matching on the era to allow them to be treated in a
-- non-uniform way.
--
data ConsensusMode mode where
     CardanoMode :: ConsensusMode CardanoMode


deriving instance Show (ConsensusMode mode)

data AnyConsensusMode where
  AnyConsensusMode :: ConsensusMode mode -> AnyConsensusMode

deriving instance Show AnyConsensusMode

renderMode :: AnyConsensusMode -> Text
renderMode (AnyConsensusMode CardanoMode) = "CardanoMode"

toEraInMode :: CardanoEra era -> ConsensusMode CardanoMode -> Maybe (EraInMode era CardanoMode)
toEraInMode ByronEra   CardanoMode = Just ByronEraInCardanoMode
toEraInMode ShelleyEra CardanoMode = Just ShelleyEraInCardanoMode
toEraInMode AllegraEra CardanoMode = Just AllegraEraInCardanoMode
toEraInMode MaryEra    CardanoMode = Just MaryEraInCardanoMode
toEraInMode AlonzoEra  CardanoMode = Just AlonzoEraInCardanoMode
toEraInMode BabbageEra CardanoMode = Just BabbageEraInCardanoMode
toEraInMode ConwayEra  CardanoMode = Just ConwayEraInCardanoMode

-- | A representation of which 'CardanoEra's are included in each
-- 'ConsensusMode'.
--
data EraInMode era mode where
     ByronEraInCardanoMode   :: EraInMode ByronEra   CardanoMode
     ShelleyEraInCardanoMode :: EraInMode ShelleyEra CardanoMode
     AllegraEraInCardanoMode :: EraInMode AllegraEra CardanoMode
     MaryEraInCardanoMode    :: EraInMode MaryEra    CardanoMode
     AlonzoEraInCardanoMode  :: EraInMode AlonzoEra  CardanoMode
     BabbageEraInCardanoMode :: EraInMode BabbageEra CardanoMode
     ConwayEraInCardanoMode  :: EraInMode ConwayEra  CardanoMode

deriving instance Show (EraInMode era mode)

deriving instance Eq (EraInMode era mode)

instance FromJSON (EraInMode ByronEra CardanoMode) where
  parseJSON "ByronEraInCardanoMode" = pure ByronEraInCardanoMode
  parseJSON invalid =
      invalidJSONFailure "ByronEraInCardanoMode"
                         "parsing 'EraInMode ByronEra CardanoMode' failed, "
                         invalid

instance FromJSON (EraInMode ShelleyEra CardanoMode) where
  parseJSON "ShelleyEraInCardanoMode" = pure ShelleyEraInCardanoMode
  parseJSON invalid =
      invalidJSONFailure "ShelleyEraInCardanoMode"
                         "parsing 'EraInMode ShelleyEra CardanoMode' failed, "
                         invalid

instance FromJSON (EraInMode AllegraEra CardanoMode) where
  parseJSON "AllegraEraInCardanoMode" = pure AllegraEraInCardanoMode
  parseJSON invalid =
      invalidJSONFailure "AllegraEraInCardanoMode"
                         "parsing 'EraInMode AllegraEra CardanoMode' failed, "
                         invalid

instance FromJSON (EraInMode MaryEra CardanoMode) where
  parseJSON "MaryEraInCardanoMode" = pure MaryEraInCardanoMode
  parseJSON invalid =
      invalidJSONFailure "MaryEraInCardanoMode"
                         "parsing 'EraInMode MaryEra CardanoMode' failed, "
                         invalid

instance FromJSON (EraInMode AlonzoEra CardanoMode) where
  parseJSON "AlonzoEraInCardanoMode" = pure AlonzoEraInCardanoMode
  parseJSON invalid =
      invalidJSONFailure "AlonzoEraInCardanoMode"
                         "parsing 'EraInMode AlonzoEra CardanoMode' failed, "
                         invalid

instance FromJSON (EraInMode BabbageEra CardanoMode) where
  parseJSON "BabbageEraInCardanoMode" = pure BabbageEraInCardanoMode
  parseJSON invalid =
      invalidJSONFailure "BabbageEraInCardanoMode"
                         "parsing 'EraInMode Babbage CardanoMode' failed, "
                         invalid

instance FromJSON (EraInMode ConwayEra CardanoMode) where
  parseJSON "ConwayEraInCardanoMode" = pure ConwayEraInCardanoMode
  parseJSON invalid =
      invalidJSONFailure "ConwayEraInCardanoMode"
                         "parsing 'EraInMode Conway CardanoMode' failed, "
                         invalid

invalidJSONFailure :: String -> String -> Value -> Parser a
invalidJSONFailure expectedType errorMsg invalidValue =
    prependFailure errorMsg
                   (typeMismatch expectedType invalidValue)

instance ToJSON (EraInMode era mode) where
  toJSON ByronEraInCardanoMode  = "ByronEraInCardanoMode"
  toJSON ShelleyEraInCardanoMode = "ShelleyEraInCardanoMode"
  toJSON AllegraEraInCardanoMode = "AllegraEraInCardanoMode"
  toJSON MaryEraInCardanoMode = "MaryEraInCardanoMode"
  toJSON AlonzoEraInCardanoMode = "AlonzoEraInCardanoMode"
  toJSON BabbageEraInCardanoMode = "BabbageEraInCardanoMode"
  toJSON ConwayEraInCardanoMode = "ConwayEraInCardanoMode"

eraInModeToEra :: EraInMode era mode -> CardanoEra era
eraInModeToEra ByronEraInCardanoMode   = ByronEra
eraInModeToEra ShelleyEraInCardanoMode = ShelleyEra
eraInModeToEra AllegraEraInCardanoMode = AllegraEra
eraInModeToEra MaryEraInCardanoMode    = MaryEra
eraInModeToEra AlonzoEraInCardanoMode  = AlonzoEra
eraInModeToEra BabbageEraInCardanoMode = BabbageEra
eraInModeToEra ConwayEraInCardanoMode  = ConwayEra

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
--
data ConsensusModeParams mode where

     CardanoModeParams
       :: Byron.EpochSlots
       -> ConsensusModeParams CardanoMode

deriving instance Show (ConsensusModeParams mode)

-- ----------------------------------------------------------------------------
-- Consensus conversion functions
--

-- | A closed type family that maps between the consensus mode (from this API)
-- and the block type used by the consensus libraries.
--
type family ConsensusBlockForMode mode where
  ConsensusBlockForMode CardanoMode = Consensus.CardanoBlock StandardCrypto

type family ConsensusBlockForEra era where
  ConsensusBlockForEra ByronEra   = Consensus.ByronBlock
  ConsensusBlockForEra ShelleyEra = Consensus.StandardShelleyBlock
  ConsensusBlockForEra AllegraEra = Consensus.StandardAllegraBlock
  ConsensusBlockForEra MaryEra    = Consensus.StandardMaryBlock
  ConsensusBlockForEra AlonzoEra  = Consensus.StandardAlonzoBlock
  ConsensusBlockForEra BabbageEra = Consensus.StandardBabbageBlock
  ConsensusBlockForEra ConwayEra = Consensus.StandardConwayBlock

type family ConsensusCryptoForBlock block where
  ConsensusCryptoForBlock Consensus.ByronBlockHFC = StandardCrypto
  ConsensusCryptoForBlock (Consensus.ShelleyBlockHFC (Consensus.TPraos StandardCrypto) Consensus.StandardShelley) = Consensus.StandardShelley
  ConsensusCryptoForBlock (Consensus.CardanoBlock StandardCrypto) = StandardCrypto

type family ConsensusProtocol era where
  ConsensusProtocol ShelleyEra = Consensus.TPraos StandardCrypto
  ConsensusProtocol AllegraEra = Consensus.TPraos StandardCrypto
  ConsensusProtocol MaryEra = Consensus.TPraos StandardCrypto
  ConsensusProtocol AlonzoEra = Consensus.TPraos StandardCrypto
  ConsensusProtocol BabbageEra = Consensus.Praos StandardCrypto
  ConsensusProtocol ConwayEra = Consensus.Praos StandardCrypto

type family ChainDepStateProtocol era where
  ChainDepStateProtocol ShelleyEra = Consensus.TPraosState StandardCrypto
  ChainDepStateProtocol AllegraEra = Consensus.TPraosState StandardCrypto
  ChainDepStateProtocol MaryEra = Consensus.TPraosState StandardCrypto
  ChainDepStateProtocol AlonzoEra = Consensus.TPraosState StandardCrypto
  ChainDepStateProtocol BabbageEra = Consensus.PraosState StandardCrypto
  ChainDepStateProtocol ConwayEra = Consensus.PraosState StandardCrypto

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

toConsensusEraIndex :: ()
  => ConsensusBlockForMode CardanoMode ~ Consensus.HardForkBlock xs
  => CardanoEra era
  -> Consensus.EraIndex xs
toConsensusEraIndex = \case
  ByronEra    -> eraIndex0
  ShelleyEra  -> eraIndex1
  AllegraEra  -> eraIndex2
  MaryEra     -> eraIndex3
  AlonzoEra   -> eraIndex4
  BabbageEra  -> eraIndex5
  ConwayEra   -> eraIndex6


fromConsensusEraIndex :: ()
  => ConsensusMode CardanoMode
  -> Consensus.EraIndex (Consensus.CardanoEras StandardCrypto)
  -> AnyCardanoEra
fromConsensusEraIndex CardanoMode = \case
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
