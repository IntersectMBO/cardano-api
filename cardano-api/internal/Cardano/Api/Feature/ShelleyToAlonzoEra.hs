{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Feature.ShelleyToAlonzoEra
  ( ShelleyToAlonzoEra(..)
  , IsShelleyToAlonzoEra(..)
  , AnyShelleyToAlonzoEra(..)
  , shelleyToAlonzoEraConstraints
  , shelleyToAlonzoEraToCardanoEra
  , shelleyToAlonzoEraToShelleyBasedEra
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

class IsShelleyBasedEra era => IsShelleyToAlonzoEra era where
  shelleyToAlonzoEra :: ShelleyToAlonzoEra era

data ShelleyToAlonzoEra era where
  ShelleyToAlonzoEraShelley :: ShelleyToAlonzoEra ShelleyEra
  ShelleyToAlonzoEraAllegra :: ShelleyToAlonzoEra AllegraEra
  ShelleyToAlonzoEraMary :: ShelleyToAlonzoEra MaryEra
  ShelleyToAlonzoEraAlonzo :: ShelleyToAlonzoEra AlonzoEra

deriving instance Show (ShelleyToAlonzoEra era)
deriving instance Eq (ShelleyToAlonzoEra era)

instance IsShelleyToAlonzoEra ShelleyEra where
  shelleyToAlonzoEra = ShelleyToAlonzoEraShelley

instance IsShelleyToAlonzoEra AllegraEra where
  shelleyToAlonzoEra = ShelleyToAlonzoEraAllegra

instance IsShelleyToAlonzoEra MaryEra where
  shelleyToAlonzoEra = ShelleyToAlonzoEraMary

instance IsShelleyToAlonzoEra AlonzoEra where
  shelleyToAlonzoEra = ShelleyToAlonzoEraAlonzo

instance FeatureInEra ShelleyToAlonzoEra where
  featureInEra no yes = \case
    ByronEra    -> no
    ShelleyEra  -> yes ShelleyToAlonzoEraShelley
    AllegraEra  -> yes ShelleyToAlonzoEraAllegra
    MaryEra     -> yes ShelleyToAlonzoEraMary
    AlonzoEra   -> yes ShelleyToAlonzoEraAlonzo
    BabbageEra  -> no
    ConwayEra   -> no

instance ToCardanoEra ShelleyToAlonzoEra where
  toCardanoEra = \case
    ShelleyToAlonzoEraShelley  -> ShelleyEra
    ShelleyToAlonzoEraAllegra  -> AllegraEra
    ShelleyToAlonzoEraMary     -> MaryEra
    ShelleyToAlonzoEraAlonzo   -> AlonzoEra

type ShelleyToAlonzoEraConstraints era =
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
  , L.ProtVerAtMost (ShelleyLedgerEra era) 6
  , L.ShelleyEraTxBody (ShelleyLedgerEra era)
  , L.ShelleyEraTxCert (ShelleyLedgerEra era)
  , L.TxCert (ShelleyLedgerEra era) ~ L.ShelleyTxCert (ShelleyLedgerEra era)
  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , FromCBOR (DebugLedgerState era)
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , IsShelleyToAlonzoEra era
  , ToJSON (DebugLedgerState era)
  , Typeable era
  )

data AnyShelleyToAlonzoEra where
  AnyShelleyToAlonzoEra :: ShelleyToAlonzoEra era -> AnyShelleyToAlonzoEra

deriving instance Show AnyShelleyToAlonzoEra

shelleyToAlonzoEraConstraints :: ()
  => ShelleyToAlonzoEra era
  -> (ShelleyToAlonzoEraConstraints era => a)
  -> a
shelleyToAlonzoEraConstraints = \case
  ShelleyToAlonzoEraShelley -> id
  ShelleyToAlonzoEraAllegra -> id
  ShelleyToAlonzoEraMary    -> id
  ShelleyToAlonzoEraAlonzo  -> id

shelleyToAlonzoEraToCardanoEra :: ShelleyToAlonzoEra era -> CardanoEra era
shelleyToAlonzoEraToCardanoEra = shelleyBasedToCardanoEra . shelleyToAlonzoEraToShelleyBasedEra

shelleyToAlonzoEraToShelleyBasedEra :: ShelleyToAlonzoEra era -> ShelleyBasedEra era
shelleyToAlonzoEraToShelleyBasedEra = \case
  ShelleyToAlonzoEraShelley -> ShelleyBasedEraShelley
  ShelleyToAlonzoEraAllegra -> ShelleyBasedEraAllegra
  ShelleyToAlonzoEraMary    -> ShelleyBasedEraMary
  ShelleyToAlonzoEraAlonzo  -> ShelleyBasedEraAlonzo
