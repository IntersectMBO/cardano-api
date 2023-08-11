{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Feature.AlonzoEraOnwards
  ( AlonzoEraOnwards(..)
  , IsAlonzoEraOnwards(..)
  , AnyAlonzoEraOnwards(..)
  , alonzoEraOnwardsConstraints
  , alonzoEraOnwardsToCardanoEra
  , alonzoEraOnwardsToShelleyBasedEra
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

class IsShelleyBasedEra era => IsAlonzoEraOnwards era where
  alonzoEraOnwards :: AlonzoEraOnwards era

data AlonzoEraOnwards era where
  AlonzoEraOnwardsAlonzo  :: AlonzoEraOnwards AlonzoEra
  AlonzoEraOnwardsBabbage :: AlonzoEraOnwards BabbageEra
  AlonzoEraOnwardsConway  :: AlonzoEraOnwards ConwayEra

deriving instance Show (AlonzoEraOnwards era)
deriving instance Eq (AlonzoEraOnwards era)

instance IsAlonzoEraOnwards AlonzoEra where
  alonzoEraOnwards = AlonzoEraOnwardsAlonzo

instance IsAlonzoEraOnwards BabbageEra where
  alonzoEraOnwards = AlonzoEraOnwardsBabbage

instance IsAlonzoEraOnwards ConwayEra where
  alonzoEraOnwards = AlonzoEraOnwardsConway

instance FeatureInEra AlonzoEraOnwards where
  featureInEra no yes = \case
    ByronEra    -> no
    ShelleyEra  -> no
    AllegraEra  -> no
    MaryEra     -> no
    AlonzoEra   -> yes AlonzoEraOnwardsAlonzo
    BabbageEra  -> yes AlonzoEraOnwardsBabbage
    ConwayEra   -> yes AlonzoEraOnwardsConway

instance ToCardanoEra AlonzoEraOnwards where
  toCardanoEra = \case
    AlonzoEraOnwardsAlonzo  -> AlonzoEra
    AlonzoEraOnwardsBabbage -> BabbageEra
    AlonzoEraOnwardsConway  -> ConwayEra

data AnyAlonzoEraOnwards where
  AnyAlonzoEraOnwards :: AlonzoEraOnwards era -> AnyAlonzoEraOnwards

deriving instance Show AnyAlonzoEraOnwards

type AlonzoEraOnwardsConstraints era =
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
  , L.HashAnnotated (L.TxBody (ShelleyLedgerEra era)) L.EraIndependentTxBody L.StandardCrypto
  , L.ShelleyEraTxBody (ShelleyLedgerEra era)
  , L.ShelleyEraTxCert (ShelleyLedgerEra era)

  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , FromCBOR (DebugLedgerState era)
  , IsAlonzoEraOnwards era
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , ToJSON (DebugLedgerState era)
  , Typeable era
  )

alonzoEraOnwardsConstraints :: ()
  => AlonzoEraOnwards era
  -> (AlonzoEraOnwardsConstraints era => a)
  -> a
alonzoEraOnwardsConstraints = \case
  AlonzoEraOnwardsAlonzo  -> id
  AlonzoEraOnwardsBabbage -> id
  AlonzoEraOnwardsConway  -> id

alonzoEraOnwardsToCardanoEra :: AlonzoEraOnwards era -> CardanoEra era
alonzoEraOnwardsToCardanoEra = shelleyBasedToCardanoEra . alonzoEraOnwardsToShelleyBasedEra

alonzoEraOnwardsToShelleyBasedEra :: AlonzoEraOnwards era -> ShelleyBasedEra era
alonzoEraOnwardsToShelleyBasedEra = \case
  AlonzoEraOnwardsAlonzo  -> ShelleyBasedEraAlonzo
  AlonzoEraOnwardsBabbage -> ShelleyBasedEraBabbage
  AlonzoEraOnwardsConway  -> ShelleyBasedEraConway
