{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Feature.ShelleyToAllegraEra
  ( ShelleyToAllegraEra(..)
  , IsShelleyToAllegraEra(..)
  , AnyShelleyToAllegraEra(..)
  , shelleyToAllegraEraConstraints
  , shelleyToAllegraEraToCardanoEra
  , shelleyToAllegraEraToShelleyBasedEra
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
import qualified Cardano.Ledger.Shelley.TxCert as L
import qualified Ouroboros.Consensus.Protocol.Abstract as Consensus
import qualified Ouroboros.Consensus.Protocol.Praos.Common as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus

import           Data.Aeson
import           Data.Typeable (Typeable)

class IsShelleyBasedEra era => IsShelleyToAllegraEra era where
  shelleyToAllegraEra :: ShelleyToAllegraEra era

data ShelleyToAllegraEra era where
  ShelleyToAllegraEraShelley :: ShelleyToAllegraEra ShelleyEra
  ShelleyToAllegraEraAllegra :: ShelleyToAllegraEra AllegraEra

deriving instance Show (ShelleyToAllegraEra era)
deriving instance Eq (ShelleyToAllegraEra era)

instance IsShelleyToAllegraEra ShelleyEra where
  shelleyToAllegraEra = ShelleyToAllegraEraShelley

instance IsShelleyToAllegraEra AllegraEra where
  shelleyToAllegraEra = ShelleyToAllegraEraAllegra

instance FeatureInEra ShelleyToAllegraEra where
  featureInEra no yes = \case
    ByronEra    -> no
    ShelleyEra  -> yes ShelleyToAllegraEraShelley
    AllegraEra  -> yes ShelleyToAllegraEraAllegra
    MaryEra     -> no
    AlonzoEra   -> no
    BabbageEra  -> no
    ConwayEra   -> no

instance ToCardanoEra ShelleyToAllegraEra where
  toCardanoEra = \case
    ShelleyToAllegraEraShelley  -> ShelleyEra
    ShelleyToAllegraEraAllegra  -> AllegraEra

type ShelleyToAllegraEraConstraints era =
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
  , L.ProtVerAtMost (ShelleyLedgerEra era) 4
  , L.ShelleyEraTxBody (ShelleyLedgerEra era)
  , L.ShelleyEraTxCert (ShelleyLedgerEra era)
  , L.TxCert (ShelleyLedgerEra era) ~ L.ShelleyTxCert (ShelleyLedgerEra era)
  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , FromCBOR (DebugLedgerState era)
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , IsShelleyToAllegraEra era
  , ToJSON (DebugLedgerState era)
  , Typeable era
  )

data AnyShelleyToAllegraEra where
  AnyShelleyToAllegraEra :: ShelleyToAllegraEra era -> AnyShelleyToAllegraEra

deriving instance Show AnyShelleyToAllegraEra

shelleyToAllegraEraConstraints :: ()
  => ShelleyToAllegraEra era
  -> (ShelleyToAllegraEraConstraints era => a)
  -> a
shelleyToAllegraEraConstraints = \case
  ShelleyToAllegraEraShelley -> id
  ShelleyToAllegraEraAllegra -> id

shelleyToAllegraEraToCardanoEra :: ShelleyToAllegraEra era -> CardanoEra era
shelleyToAllegraEraToCardanoEra = shelleyBasedToCardanoEra . shelleyToAllegraEraToShelleyBasedEra

shelleyToAllegraEraToShelleyBasedEra :: ShelleyToAllegraEra era -> ShelleyBasedEra era
shelleyToAllegraEraToShelleyBasedEra = \case
  ShelleyToAllegraEraShelley -> ShelleyBasedEraShelley
  ShelleyToAllegraEraAllegra -> ShelleyBasedEraAllegra
