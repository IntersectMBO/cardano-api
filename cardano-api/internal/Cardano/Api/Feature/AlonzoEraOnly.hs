{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Feature.AlonzoEraOnly
  ( AlonzoEraOnly(..)
  , IsAlonzoEraOnly(..)
  , AnyAlonzoEraOnly(..)
  , alonzoEraOnlyConstraints
  , alonzoEraOnlyToCardanoEra
  , alonzoEraOnlyToShelleyBasedEra
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
import qualified Cardano.Ledger.Core as L
import qualified Cardano.Ledger.SafeHash as L
import qualified Ouroboros.Consensus.Protocol.Abstract as Consensus
import qualified Ouroboros.Consensus.Protocol.Praos.Common as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus

import           Data.Aeson
import           Data.Typeable (Typeable)

class IsShelleyBasedEra era => IsAlonzoEraOnly era where
  alonzoEraOnly :: AlonzoEraOnly era

data AlonzoEraOnly era where
  AlonzoEraOnlyAlonzo  :: AlonzoEraOnly AlonzoEra

deriving instance Show (AlonzoEraOnly era)
deriving instance Eq (AlonzoEraOnly era)

instance IsAlonzoEraOnly AlonzoEra where
  alonzoEraOnly = AlonzoEraOnlyAlonzo

instance FeatureInEra AlonzoEraOnly where
  featureInEra no yes = \case
    ByronEra    -> no
    ShelleyEra  -> no
    AllegraEra  -> no
    MaryEra     -> no
    AlonzoEra   -> yes AlonzoEraOnlyAlonzo
    BabbageEra  -> no
    ConwayEra   -> no

instance ToCardanoEra AlonzoEraOnly where
  toCardanoEra = \case
    AlonzoEraOnlyAlonzo  -> AlonzoEra

data AnyAlonzoEraOnly where
  AnyAlonzoEraOnly :: AlonzoEraOnly era -> AnyAlonzoEraOnly

deriving instance Show AnyAlonzoEraOnly

type AlonzoEraOnlyConstraints era =
  ( C.HashAlgorithm (L.HASH (L.EraCrypto (ShelleyLedgerEra era)))
  , C.Signable (L.VRF (L.EraCrypto (ShelleyLedgerEra era))) L.Seed
  , Consensus.PraosProtocolSupportsNode (ConsensusProtocol era)
  , Consensus.ShelleyCompatible (ConsensusProtocol era) (ShelleyLedgerEra era)
  , L.ADDRHASH (Consensus.PraosProtocolSupportsNodeCrypto (ConsensusProtocol era)) ~ Blake2b.Blake2b_224
  , L.AlonzoEraPParams (ShelleyLedgerEra era)
  , L.Crypto (L.EraCrypto (ShelleyLedgerEra era))
  , L.Era (ShelleyLedgerEra era)
  , L.EraCrypto (ShelleyLedgerEra era) ~ L.StandardCrypto
  , L.EraPParams (ShelleyLedgerEra era)
  , L.EraTx (ShelleyLedgerEra era)
  , L.EraTxBody (ShelleyLedgerEra era)
  , L.ExactEra L.AlonzoEra (ShelleyLedgerEra era)
  , L.HashAnnotated (L.TxBody (ShelleyLedgerEra era)) L.EraIndependentTxBody L.StandardCrypto
  , L.ShelleyEraTxBody (ShelleyLedgerEra era)
  , L.ShelleyEraTxCert (ShelleyLedgerEra era)

  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , FromCBOR (DebugLedgerState era)
  , IsAlonzoEraOnly era
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , ToJSON (DebugLedgerState era)
  , Typeable era
  )

alonzoEraOnlyConstraints :: ()
  => AlonzoEraOnly era
  -> (AlonzoEraOnlyConstraints era => a)
  -> a
alonzoEraOnlyConstraints = \case
  AlonzoEraOnlyAlonzo  -> id

alonzoEraOnlyToCardanoEra :: AlonzoEraOnly era -> CardanoEra era
alonzoEraOnlyToCardanoEra = shelleyBasedToCardanoEra . alonzoEraOnlyToShelleyBasedEra

alonzoEraOnlyToShelleyBasedEra :: AlonzoEraOnly era -> ShelleyBasedEra era
alonzoEraOnlyToShelleyBasedEra = \case
  AlonzoEraOnlyAlonzo  -> ShelleyBasedEraAlonzo
