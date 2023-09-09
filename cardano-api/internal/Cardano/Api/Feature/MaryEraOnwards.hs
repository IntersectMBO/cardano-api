{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Feature.MaryEraOnwards
  ( MaryEraOnwards(..)
  , IsMaryEraOnwards(..)
  , AnyMaryEraOnwards(..)
  , maryEraOnwardsConstraints
  , maryEraOnwardsToCardanoEra
  , maryEraOnwardsToShelleyBasedEra
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

class IsShelleyBasedEra era => IsMaryEraOnwards era where
  maryEraOnwards :: MaryEraOnwards era

data MaryEraOnwards era where
  MaryEraOnwardsMary    :: MaryEraOnwards MaryEra
  MaryEraOnwardsAlonzo  :: MaryEraOnwards AlonzoEra
  MaryEraOnwardsBabbage :: MaryEraOnwards BabbageEra
  MaryEraOnwardsConway  :: MaryEraOnwards ConwayEra

deriving instance Show (MaryEraOnwards era)
deriving instance Eq (MaryEraOnwards era)

instance IsMaryEraOnwards MaryEra where
  maryEraOnwards = MaryEraOnwardsMary

instance IsMaryEraOnwards AlonzoEra where
  maryEraOnwards = MaryEraOnwardsAlonzo

instance IsMaryEraOnwards BabbageEra where
  maryEraOnwards = MaryEraOnwardsBabbage

instance IsMaryEraOnwards ConwayEra where
  maryEraOnwards = MaryEraOnwardsConway

instance FeatureInEra MaryEraOnwards where
  featureInEra no yes = \case
    ByronEra    -> no
    ShelleyEra  -> no
    AllegraEra  -> no
    MaryEra     -> yes MaryEraOnwardsMary
    AlonzoEra   -> yes MaryEraOnwardsAlonzo
    BabbageEra  -> yes MaryEraOnwardsBabbage
    ConwayEra   -> yes MaryEraOnwardsConway

instance ToCardanoEra MaryEraOnwards where
  toCardanoEra = \case
    MaryEraOnwardsMary    -> MaryEra
    MaryEraOnwardsAlonzo  -> AlonzoEra
    MaryEraOnwardsBabbage -> BabbageEra
    MaryEraOnwardsConway  -> ConwayEra

data AnyMaryEraOnwards where
  AnyMaryEraOnwards :: MaryEraOnwards era -> AnyMaryEraOnwards

deriving instance Show AnyMaryEraOnwards

type MaryEraOnwardsConstraints era =
  ( C.HashAlgorithm (L.HASH (L.EraCrypto (ShelleyLedgerEra era)))
  , C.Signable (L.VRF (L.EraCrypto (ShelleyLedgerEra era))) L.Seed
  , Consensus.PraosProtocolSupportsNode (ConsensusProtocol era)
  , Consensus.ShelleyCompatible (ConsensusProtocol era) (ShelleyLedgerEra era)
  , L.ADDRHASH (Consensus.PraosProtocolSupportsNodeCrypto (ConsensusProtocol era)) ~ Blake2b.Blake2b_224
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
  , IsMaryEraOnwards era
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , ToJSON (DebugLedgerState era)
  , Typeable era
  )

maryEraOnwardsConstraints :: ()
  => MaryEraOnwards era
  -> (MaryEraOnwardsConstraints era => a)
  -> a
maryEraOnwardsConstraints = \case
  MaryEraOnwardsMary    -> id
  MaryEraOnwardsAlonzo  -> id
  MaryEraOnwardsBabbage -> id
  MaryEraOnwardsConway  -> id

maryEraOnwardsToCardanoEra :: MaryEraOnwards era -> CardanoEra era
maryEraOnwardsToCardanoEra = shelleyBasedToCardanoEra . maryEraOnwardsToShelleyBasedEra

maryEraOnwardsToShelleyBasedEra :: MaryEraOnwards era -> ShelleyBasedEra era
maryEraOnwardsToShelleyBasedEra = \case
  MaryEraOnwardsMary    -> ShelleyBasedEraMary
  MaryEraOnwardsAlonzo  -> ShelleyBasedEraAlonzo
  MaryEraOnwardsBabbage -> ShelleyBasedEraBabbage
  MaryEraOnwardsConway  -> ShelleyBasedEraConway
