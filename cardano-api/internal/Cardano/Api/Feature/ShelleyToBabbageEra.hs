{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Feature.ShelleyToBabbageEra
  ( ShelleyToBabbageEra(..)
  , IsShelleyToBabbageEra(..)
  , AnyShelleyToBabbageEra(..)
  , shelleyToBabbageEraConstraints
  , shelleyToBabbageEraToCardanoEra
  , shelleyToBabbageEraToShelleyBasedEra
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

class IsShelleyBasedEra era => IsShelleyToBabbageEra era where
  shelleyToBabbageEra :: ShelleyToBabbageEra era

data ShelleyToBabbageEra era where
  ShelleyToBabbageEraShelley :: ShelleyToBabbageEra ShelleyEra
  ShelleyToBabbageEraAllegra :: ShelleyToBabbageEra AllegraEra
  ShelleyToBabbageEraMary :: ShelleyToBabbageEra MaryEra
  ShelleyToBabbageEraAlonzo :: ShelleyToBabbageEra AlonzoEra
  ShelleyToBabbageEraBabbage :: ShelleyToBabbageEra BabbageEra

deriving instance Show (ShelleyToBabbageEra era)
deriving instance Eq (ShelleyToBabbageEra era)

instance IsShelleyToBabbageEra ShelleyEra where
  shelleyToBabbageEra = ShelleyToBabbageEraShelley

instance IsShelleyToBabbageEra AllegraEra where
  shelleyToBabbageEra = ShelleyToBabbageEraAllegra

instance IsShelleyToBabbageEra MaryEra where
  shelleyToBabbageEra = ShelleyToBabbageEraMary

instance IsShelleyToBabbageEra AlonzoEra where
  shelleyToBabbageEra = ShelleyToBabbageEraAlonzo

instance IsShelleyToBabbageEra BabbageEra where
  shelleyToBabbageEra = ShelleyToBabbageEraBabbage

instance FeatureInEra ShelleyToBabbageEra where
  featureInEra no yes = \case
    ByronEra    -> no
    ShelleyEra  -> yes ShelleyToBabbageEraShelley
    AllegraEra  -> yes ShelleyToBabbageEraAllegra
    MaryEra     -> yes ShelleyToBabbageEraMary
    AlonzoEra   -> yes ShelleyToBabbageEraAlonzo
    BabbageEra  -> yes ShelleyToBabbageEraBabbage
    ConwayEra   -> no

instance ToCardanoEra ShelleyToBabbageEra where
  toCardanoEra = \case
    ShelleyToBabbageEraShelley  -> ShelleyEra
    ShelleyToBabbageEraAllegra  -> AllegraEra
    ShelleyToBabbageEraMary     -> MaryEra
    ShelleyToBabbageEraAlonzo   -> AlonzoEra
    ShelleyToBabbageEraBabbage  -> BabbageEra

type ShelleyToBabbageEraConstraints era =
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
  , L.ProtVerAtMost (ShelleyLedgerEra era) 7
  , L.ShelleyEraTxBody (ShelleyLedgerEra era)
  , L.ShelleyEraTxCert (ShelleyLedgerEra era)
  , L.TxCert (ShelleyLedgerEra era) ~ L.ShelleyTxCert (ShelleyLedgerEra era)
  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , FromCBOR (DebugLedgerState era)
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , IsShelleyToBabbageEra era
  , ToJSON (DebugLedgerState era)
  , Typeable era
  )

data AnyShelleyToBabbageEra where
  AnyShelleyToBabbageEra :: ShelleyToBabbageEra era -> AnyShelleyToBabbageEra

deriving instance Show AnyShelleyToBabbageEra

shelleyToBabbageEraConstraints :: ()
  => ShelleyToBabbageEra era
  -> (ShelleyToBabbageEraConstraints era => a)
  -> a
shelleyToBabbageEraConstraints = \case
  ShelleyToBabbageEraShelley -> id
  ShelleyToBabbageEraAllegra -> id
  ShelleyToBabbageEraMary    -> id
  ShelleyToBabbageEraAlonzo  -> id
  ShelleyToBabbageEraBabbage -> id

shelleyToBabbageEraToCardanoEra :: ShelleyToBabbageEra era -> CardanoEra era
shelleyToBabbageEraToCardanoEra = shelleyBasedToCardanoEra . shelleyToBabbageEraToShelleyBasedEra

shelleyToBabbageEraToShelleyBasedEra :: ShelleyToBabbageEra era -> ShelleyBasedEra era
shelleyToBabbageEraToShelleyBasedEra = \case
  ShelleyToBabbageEraShelley -> ShelleyBasedEraShelley
  ShelleyToBabbageEraAllegra -> ShelleyBasedEraAllegra
  ShelleyToBabbageEraMary    -> ShelleyBasedEraMary
  ShelleyToBabbageEraAlonzo  -> ShelleyBasedEraAlonzo
  ShelleyToBabbageEraBabbage -> ShelleyBasedEraBabbage
