{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Era.Internal.Eon.ConwayEraOnwards
  ( ConwayEraOnwards (..)
  , conwayEraOnwardsConstraints
  , conwayEraOnwardsToShelleyBasedEra
  , conwayEraOnwardsToBabbageEraOnwards
  , ConwayEraOnwardsConstraints
  , IsConwayBasedEra (..)
  )
where

import Cardano.Api.Consensus.Internal.Mode
import Cardano.Api.Era.Internal.Core
import Cardano.Api.Era.Internal.Eon.AllegraEraOnwards (AllegraEraOnwards (..))
import Cardano.Api.Era.Internal.Eon.BabbageEraOnwards
import Cardano.Api.Era.Internal.Eon.Convert
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Query.Internal.Type.DebugLedgerState

import Cardano.Binary
import Cardano.Crypto.Hash.Blake2b qualified as Blake2b
import Cardano.Crypto.Hash.Class qualified as C
import Cardano.Crypto.VRF qualified as C
import Cardano.Ledger.Alonzo.Scripts qualified as L
import Cardano.Ledger.Alonzo.UTxO qualified as L
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Conway.Core qualified as L
import Cardano.Ledger.Conway.Governance qualified as L
import Cardano.Ledger.Conway.State qualified as L
import Cardano.Ledger.Conway.TxCert qualified as L
import Cardano.Ledger.Mary.Value qualified as L
import Cardano.Protocol.Crypto qualified as L
import Ouroboros.Consensus.Protocol.Abstract qualified as Consensus
import Ouroboros.Consensus.Protocol.Praos.Common qualified as Consensus
import Ouroboros.Consensus.Shelley.Ledger qualified as Consensus

import Data.Aeson
import Data.Type.Equality
import Data.Typeable (Typeable)

data ConwayEraOnwards era where
  ConwayEraOnwardsConway :: ConwayEraOnwards ConwayEra
  ConwayEraOnwardsDijkstra :: ConwayEraOnwards DijkstraEra

deriving instance Show (ConwayEraOnwards era)

deriving instance Eq (ConwayEraOnwards era)

deriving instance Ord (ConwayEraOnwards era)

instance Eon ConwayEraOnwards where
  inEonForEra no yes = \case
    ByronEra -> no
    ShelleyEra -> no
    AllegraEra -> no
    MaryEra -> no
    AlonzoEra -> no
    BabbageEra -> no
    ConwayEra -> yes ConwayEraOnwardsConway
    DijkstraEra -> yes ConwayEraOnwardsDijkstra

instance ToCardanoEra ConwayEraOnwards where
  toCardanoEra = \case
    ConwayEraOnwardsConway -> ConwayEra
    ConwayEraOnwardsDijkstra -> DijkstraEra

instance Convert ConwayEraOnwards CardanoEra where
  convert = toCardanoEra

instance Convert ConwayEraOnwards ShelleyBasedEra where
  convert = \case
    ConwayEraOnwardsConway -> ShelleyBasedEraConway
    ConwayEraOnwardsDijkstra -> ShelleyBasedEraDijkstra

instance Convert ConwayEraOnwards AllegraEraOnwards where
  convert = \case
    ConwayEraOnwardsConway -> AllegraEraOnwardsConway
    ConwayEraOnwardsDijkstra -> AllegraEraOnwardsDijkstra

instance Convert ConwayEraOnwards BabbageEraOnwards where
  convert = \case
    ConwayEraOnwardsConway -> BabbageEraOnwardsConway
    ConwayEraOnwardsDijkstra -> BabbageEraOnwardsDijkstra

type ConwayEraOnwardsConstraints era =
  ( L.ConwayEraCertState (ShelleyLedgerEra era)
  , C.HashAlgorithm L.HASH
  , C.Signable (L.VRF L.StandardCrypto) L.Seed
  , Consensus.PraosProtocolSupportsNode (ConsensusProtocol era)
  , Consensus.ShelleyBlock (ConsensusProtocol era) (ShelleyLedgerEra era) ~ ConsensusBlockForEra era
  , Consensus.ShelleyCompatible (ConsensusProtocol era) (ShelleyLedgerEra era)
  , L.ADDRHASH ~ Blake2b.Blake2b_224
  , L.AlonzoEraTxOut (ShelleyLedgerEra era)
  , L.AlonzoEraTxWits (ShelleyLedgerEra era)
  , L.BabbageEraTxBody (ShelleyLedgerEra era)
  , L.ConwayEraGov (ShelleyLedgerEra era)
  , L.ConwayEraPParams (ShelleyLedgerEra era)
  , L.ConwayEraTxBody (ShelleyLedgerEra era)
  , L.ConwayEraTxCert (ShelleyLedgerEra era)
  , L.Era (ShelleyLedgerEra era)
  , L.EraGov (ShelleyLedgerEra era)
  , L.EraPParams (ShelleyLedgerEra era)
  , L.EraTx (ShelleyLedgerEra era)
  , L.EraTxBody (ShelleyLedgerEra era)
  , L.EraTxOut (ShelleyLedgerEra era)
  , L.EraUTxO (ShelleyLedgerEra era)
  , L.GovState (ShelleyLedgerEra era) ~ L.ConwayGovState (ShelleyLedgerEra era)
  , L.HashAnnotated (L.TxBody (ShelleyLedgerEra era)) L.EraIndependentTxBody
  , L.MaryEraTxBody (ShelleyLedgerEra era)
  , L.Script (ShelleyLedgerEra era) ~ L.AlonzoScript (ShelleyLedgerEra era)
  , L.ScriptsNeeded (ShelleyLedgerEra era) ~ L.AlonzoScriptsNeeded (ShelleyLedgerEra era)
  , L.ShelleyEraTxCert (ShelleyLedgerEra era)
  , L.TxCert (ShelleyLedgerEra era) ~ L.ConwayTxCert (ShelleyLedgerEra era)
  , L.Value (ShelleyLedgerEra era) ~ L.MaryValue
  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , FromCBOR (DebugLedgerState era)
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , ToJSON (Consensus.ChainDepState (ConsensusProtocol era))
  , ToJSON (DebugLedgerState era)
  , Typeable era
  , (era == ByronEra) ~ False
  )

conwayEraOnwardsConstraints
  :: ()
  => ConwayEraOnwards era
  -> (ConwayEraOnwardsConstraints era => a)
  -> a
conwayEraOnwardsConstraints = \case
  ConwayEraOnwardsConway -> id
  ConwayEraOnwardsDijkstra -> id

{-# DEPRECATED conwayEraOnwardsToShelleyBasedEra "Use 'convert' instead." #-}
conwayEraOnwardsToShelleyBasedEra :: ConwayEraOnwards era -> ShelleyBasedEra era
conwayEraOnwardsToShelleyBasedEra = convert

{-# DEPRECATED conwayEraOnwardsToBabbageEraOnwards "Use 'convert' instead." #-}
conwayEraOnwardsToBabbageEraOnwards :: ConwayEraOnwards era -> BabbageEraOnwards era
conwayEraOnwardsToBabbageEraOnwards = convert

class IsBabbageBasedEra era => IsConwayBasedEra era where
  conwayBasedEra :: ConwayEraOnwards era

instance IsConwayBasedEra ConwayEra where
  conwayBasedEra = ConwayEraOnwardsConway
