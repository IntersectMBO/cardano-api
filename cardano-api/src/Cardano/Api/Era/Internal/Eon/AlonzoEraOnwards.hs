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

module Cardano.Api.Era.Internal.Eon.AlonzoEraOnwards
  ( AlonzoEraOnwards (..)
  , alonzoEraOnwardsConstraints
  , alonzoEraOnwardsToShelleyBasedEra
  , AlonzoEraOnwardsConstraints
  , IsAlonzoBasedEra (..)
  )
where

import Cardano.Api.Consensus.Internal.Mode
import Cardano.Api.Era.Internal.Core
import Cardano.Api.Era.Internal.Eon.Convert
import Cardano.Api.Era.Internal.Eon.MaryEraOnwards
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Query.Internal.Type.DebugLedgerState

import Cardano.Binary
import Cardano.Crypto.Hash.Blake2b qualified as Blake2b
import Cardano.Crypto.Hash.Class qualified as C
import Cardano.Crypto.VRF qualified as C
import Cardano.Ledger.Alonzo.Plutus.Context qualified as Plutus
import Cardano.Ledger.Alonzo.Scripts qualified as L
import Cardano.Ledger.Alonzo.Tx qualified as L
import Cardano.Ledger.Alonzo.TxWits qualified as L
import Cardano.Ledger.Alonzo.UTxO qualified as L
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Core qualified as L
import Cardano.Ledger.Mary.Value qualified as L
import Cardano.Ledger.State qualified as L
import Cardano.Protocol.Crypto qualified as L
import Ouroboros.Consensus.Protocol.Abstract qualified as Consensus
import Ouroboros.Consensus.Protocol.Praos.Common qualified as Consensus
import Ouroboros.Consensus.Shelley.Ledger qualified as Consensus

import Data.Aeson
import Data.Type.Equality
import Data.Typeable (Typeable)

data AlonzoEraOnwards era where
  AlonzoEraOnwardsAlonzo :: AlonzoEraOnwards AlonzoEra
  AlonzoEraOnwardsBabbage :: AlonzoEraOnwards BabbageEra
  AlonzoEraOnwardsConway :: AlonzoEraOnwards ConwayEra
  AlonzoEraOnwardsDijkstra :: AlonzoEraOnwards DijkstraEra

deriving instance Show (AlonzoEraOnwards era)

deriving instance Eq (AlonzoEraOnwards era)

instance Eon AlonzoEraOnwards where
  inEonForEra no yes = \case
    ByronEra -> no
    ShelleyEra -> no
    AllegraEra -> no
    MaryEra -> no
    AlonzoEra -> yes AlonzoEraOnwardsAlonzo
    BabbageEra -> yes AlonzoEraOnwardsBabbage
    ConwayEra -> yes AlonzoEraOnwardsConway
    DijkstraEra -> yes AlonzoEraOnwardsDijkstra

instance ToCardanoEra AlonzoEraOnwards where
  toCardanoEra = \case
    AlonzoEraOnwardsAlonzo -> AlonzoEra
    AlonzoEraOnwardsBabbage -> BabbageEra
    AlonzoEraOnwardsConway -> ConwayEra
    AlonzoEraOnwardsDijkstra -> DijkstraEra

instance Convert AlonzoEraOnwards CardanoEra where
  convert = toCardanoEra

instance Convert AlonzoEraOnwards ShelleyBasedEra where
  convert = \case
    AlonzoEraOnwardsAlonzo -> ShelleyBasedEraAlonzo
    AlonzoEraOnwardsBabbage -> ShelleyBasedEraBabbage
    AlonzoEraOnwardsConway -> ShelleyBasedEraConway
    AlonzoEraOnwardsDijkstra -> ShelleyBasedEraDijkstra

type AlonzoEraOnwardsConstraints era =
  ( C.HashAlgorithm L.HASH
  , C.Signable (L.VRF L.StandardCrypto) L.Seed
  , Consensus.PraosProtocolSupportsNode (ConsensusProtocol era)
  , Consensus.ShelleyBlock (ConsensusProtocol era) (ShelleyLedgerEra era) ~ ConsensusBlockForEra era
  , Consensus.ShelleyCompatible (ConsensusProtocol era) (ShelleyLedgerEra era)
  , L.ADDRHASH ~ Blake2b.Blake2b_224
  , L.AlonzoEraPParams (ShelleyLedgerEra era)
  , L.AlonzoEraTx (ShelleyLedgerEra era)
  , L.AlonzoEraTxBody (ShelleyLedgerEra era)
  , L.AlonzoEraTxOut (ShelleyLedgerEra era)
  , L.AlonzoEraTxWits (ShelleyLedgerEra era)
  , L.Era (ShelleyLedgerEra era)
  , L.EraPParams (ShelleyLedgerEra era)
  , L.EraTx (ShelleyLedgerEra era)
  , L.EraTxBody (ShelleyLedgerEra era)
  , L.EraTxOut (ShelleyLedgerEra era)
  , L.EraUTxO (ShelleyLedgerEra era)
  , L.HashAnnotated (L.TxBody L.TopTx (ShelleyLedgerEra era)) L.EraIndependentTxBody
  , L.MaryEraTxBody (ShelleyLedgerEra era)
  , Plutus.EraPlutusContext (ShelleyLedgerEra era)
  , L.Script (ShelleyLedgerEra era) ~ L.AlonzoScript (ShelleyLedgerEra era)
  , L.ScriptsNeeded (ShelleyLedgerEra era) ~ L.AlonzoScriptsNeeded (ShelleyLedgerEra era)
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

alonzoEraOnwardsConstraints
  :: AlonzoEraOnwards era
  -> (AlonzoEraOnwardsConstraints era => a)
  -> a
alonzoEraOnwardsConstraints = \case
  AlonzoEraOnwardsAlonzo -> id
  AlonzoEraOnwardsBabbage -> id
  AlonzoEraOnwardsConway -> id
  AlonzoEraOnwardsDijkstra -> id

{-# DEPRECATED alonzoEraOnwardsToShelleyBasedEra "Use 'convert' instead." #-}
alonzoEraOnwardsToShelleyBasedEra :: AlonzoEraOnwards era -> ShelleyBasedEra era
alonzoEraOnwardsToShelleyBasedEra = convert

class IsMaryBasedEra era => IsAlonzoBasedEra era where
  alonzoBasedEra :: AlonzoEraOnwards era

instance IsAlonzoBasedEra AlonzoEra where
  alonzoBasedEra = AlonzoEraOnwardsAlonzo

instance IsAlonzoBasedEra BabbageEra where
  alonzoBasedEra = AlonzoEraOnwardsBabbage

instance IsAlonzoBasedEra ConwayEra where
  alonzoBasedEra = AlonzoEraOnwardsConway
