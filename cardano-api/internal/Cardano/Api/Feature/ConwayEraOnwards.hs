{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Feature.ConwayEraOnwards
  ( ConwayEraOnwards(..)
  , IsConwayEraOnwards(..)
  , AnyConwayEraOnwards(..)
  , conwayEraOnwardsConstraints
  , conwayEraOnwardsToCardanoEra
  , conwayEraOnwardsToShelleyBasedEra
  ) where

import           Cardano.Api.Eras.Core
import           Cardano.Api.Modes
import           Cardano.Api.Query.Types

import           Cardano.Binary
import qualified Cardano.Crypto.Hash.Blake2b as Blake2b
import qualified Cardano.Crypto.Hash.Class as C
import qualified Cardano.Crypto.VRF as C
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.BaseTypes as L
import qualified Cardano.Ledger.Conway.TxCert as L
import qualified Cardano.Ledger.Core as L
import qualified Cardano.Ledger.SafeHash as L
import qualified Ouroboros.Consensus.Protocol.Abstract as Consensus
import qualified Ouroboros.Consensus.Protocol.Praos.Common as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus

import           Data.Aeson
import           Data.Typeable (Typeable)

class IsShelleyBasedEra era => IsConwayEraOnwards era where
  conwayEraOnwards :: ConwayEraOnwards era

data ConwayEraOnwards era where
  ConwayEraOnwardsConway :: ConwayEraOnwards ConwayEra

deriving instance Show (ConwayEraOnwards era)
deriving instance Eq (ConwayEraOnwards era)

instance IsConwayEraOnwards ConwayEra where
  conwayEraOnwards = ConwayEraOnwardsConway

instance FeatureInEra ConwayEraOnwards where
  featureInEra no yes = \case
    ByronEra    -> no
    ShelleyEra  -> no
    AllegraEra  -> no
    MaryEra     -> no
    AlonzoEra   -> no
    BabbageEra  -> no
    ConwayEra   -> yes ConwayEraOnwardsConway

instance ToCardanoEra ConwayEraOnwards where
  toCardanoEra = \case
    ConwayEraOnwardsConway -> ConwayEra

data AnyConwayEraOnwards where
  AnyConwayEraOnwards :: ConwayEraOnwards era -> AnyConwayEraOnwards

deriving instance Show AnyConwayEraOnwards

type ConwayEraOnwardsConstraints era ledgerera =
  ( C.HashAlgorithm (L.HASH (L.EraCrypto ledgerera))
  , C.Signable (L.VRF (L.EraCrypto ledgerera)) L.Seed
  , Consensus.PraosProtocolSupportsNode (ConsensusProtocol era)
  , Consensus.ShelleyCompatible (ConsensusProtocol era) ledgerera
  , L.ADDRHASH (Consensus.PraosProtocolSupportsNodeCrypto (ConsensusProtocol era)) ~ Blake2b.Blake2b_224
  , L.ConwayEraTxBody ledgerera
  , L.ConwayEraTxCert ledgerera
  , L.Crypto (L.EraCrypto ledgerera)
  , L.Era ledgerera
  , L.EraCrypto ledgerera ~ L.StandardCrypto
  , L.EraGov ledgerera
  , L.EraPParams ledgerera
  , L.EraTx ledgerera
  , L.EraTxBody ledgerera
  , L.HashAnnotated (L.TxBody ledgerera) L.EraIndependentTxBody L.StandardCrypto
  , L.ShelleyEraTxBody ledgerera
  , L.TxCert ledgerera ~ L.ConwayTxCert ledgerera
  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , FromCBOR (DebugLedgerState era)
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , IsConwayEraOnwards era
  , ToJSON (DebugLedgerState era)
  , Typeable era
  )

conwayEraOnwardsConstraints :: ()
  => ShelleyLedgerEra era ~ ledgerera
  => ConwayEraOnwards era
  -> (ConwayEraOnwardsConstraints era ledgerera => a)
  -> a
conwayEraOnwardsConstraints = \case
  ConwayEraOnwardsConway -> id

conwayEraOnwardsToCardanoEra :: ConwayEraOnwards era -> CardanoEra era
conwayEraOnwardsToCardanoEra = shelleyBasedToCardanoEra . conwayEraOnwardsToShelleyBasedEra

conwayEraOnwardsToShelleyBasedEra :: ConwayEraOnwards era -> ShelleyBasedEra era
conwayEraOnwardsToShelleyBasedEra = \case
  ConwayEraOnwardsConway -> ShelleyBasedEraConway
