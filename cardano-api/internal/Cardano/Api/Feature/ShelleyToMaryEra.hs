{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Feature.ShelleyToMaryEra
  ( ShelleyToMaryEra(..)
  , IsShelleyToMaryEra(..)
  , AnyShelleyToMaryEra(..)
  , shelleyToMaryEraConstraints
  , shelleyToMaryEraToCardanoEra
  , shelleyToMaryEraToShelleyBasedEra
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

class IsShelleyBasedEra era => IsShelleyToMaryEra era where
  shelleyToMaryEra :: ShelleyToMaryEra era

data ShelleyToMaryEra era where
  ShelleyToMaryEraShelley :: ShelleyToMaryEra ShelleyEra
  ShelleyToMaryEraAllegra :: ShelleyToMaryEra AllegraEra
  ShelleyToMaryEraMary    :: ShelleyToMaryEra MaryEra

deriving instance Show (ShelleyToMaryEra era)
deriving instance Eq (ShelleyToMaryEra era)

instance IsShelleyToMaryEra ShelleyEra where
  shelleyToMaryEra = ShelleyToMaryEraShelley

instance IsShelleyToMaryEra AllegraEra where
  shelleyToMaryEra = ShelleyToMaryEraAllegra

instance IsShelleyToMaryEra MaryEra where
  shelleyToMaryEra = ShelleyToMaryEraMary

instance FeatureInEra ShelleyToMaryEra where
  featureInEra no yes = \case
    ByronEra    -> no
    ShelleyEra  -> yes ShelleyToMaryEraShelley
    AllegraEra  -> yes ShelleyToMaryEraAllegra
    MaryEra     -> yes ShelleyToMaryEraMary
    AlonzoEra   -> no
    BabbageEra  -> no
    ConwayEra   -> no

instance ToCardanoEra ShelleyToMaryEra where
  toCardanoEra = \case
    ShelleyToMaryEraShelley  -> ShelleyEra
    ShelleyToMaryEraAllegra  -> AllegraEra
    ShelleyToMaryEraMary     -> MaryEra

type ShelleyToMaryEraConstraints era =
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
  , L.ProtVerAtMost (ShelleyLedgerEra era) 5
  , L.ShelleyEraTxBody (ShelleyLedgerEra era)
  , L.ShelleyEraTxCert (ShelleyLedgerEra era)
  , L.TxCert (ShelleyLedgerEra era) ~ L.ShelleyTxCert (ShelleyLedgerEra era)
  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , FromCBOR (DebugLedgerState era)
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , IsShelleyToMaryEra era
  , ToJSON (DebugLedgerState era)
  , Typeable era
  )

data AnyShelleyToMaryEra where
  AnyShelleyToMaryEra :: ShelleyToMaryEra era -> AnyShelleyToMaryEra

deriving instance Show AnyShelleyToMaryEra

shelleyToMaryEraConstraints :: ()
  => ShelleyToMaryEra era
  -> (ShelleyToMaryEraConstraints era => a)
  -> a
shelleyToMaryEraConstraints = \case
  ShelleyToMaryEraShelley -> id
  ShelleyToMaryEraAllegra -> id
  ShelleyToMaryEraMary    -> id

shelleyToMaryEraToCardanoEra :: ShelleyToMaryEra era -> CardanoEra era
shelleyToMaryEraToCardanoEra = shelleyBasedToCardanoEra . shelleyToMaryEraToShelleyBasedEra

shelleyToMaryEraToShelleyBasedEra :: ShelleyToMaryEra era -> ShelleyBasedEra era
shelleyToMaryEraToShelleyBasedEra = \case
  ShelleyToMaryEraShelley -> ShelleyBasedEraShelley
  ShelleyToMaryEraAllegra -> ShelleyBasedEraAllegra
  ShelleyToMaryEraMary    -> ShelleyBasedEraMary
