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

module Cardano.Api.Era.Internal.Eon.AllegraEraOnwards
  ( AllegraEraOnwards (..)
  , allegraEraOnwardsConstraints
  , allegraEraOnwardsToShelleyBasedEra
  , AllegraEraOnwardsConstraints
  , IsAllegraBasedEra (..)
  )
where

import Cardano.Api.Consensus.Internal.Mode
import Cardano.Api.Era.Internal.Core
import Cardano.Api.Era.Internal.Eon.Convert
import Cardano.Api.Era.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Query.Internal.Type.DebugLedgerState

import Cardano.Binary
import Cardano.Crypto.Hash.Blake2b qualified as Blake2b
import Cardano.Crypto.Hash.Class qualified as C
import Cardano.Crypto.VRF qualified as C
import Cardano.Ledger.Api qualified as L
import Cardano.Ledger.BaseTypes qualified as L
import Cardano.Ledger.Core qualified as L
import Cardano.Protocol.Crypto qualified as L
import Ouroboros.Consensus.Protocol.Abstract qualified as Consensus
import Ouroboros.Consensus.Protocol.Praos.Common qualified as Consensus
import Ouroboros.Consensus.Shelley.Ledger qualified as Consensus

import Data.Aeson
import Data.Type.Equality
import Data.Typeable (Typeable)

data AllegraEraOnwards era where
  AllegraEraOnwardsAllegra :: AllegraEraOnwards AllegraEra
  AllegraEraOnwardsMary :: AllegraEraOnwards MaryEra
  AllegraEraOnwardsAlonzo :: AllegraEraOnwards AlonzoEra
  AllegraEraOnwardsBabbage :: AllegraEraOnwards BabbageEra
  AllegraEraOnwardsConway :: AllegraEraOnwards ConwayEra
  AllegraEraOnwardsDijkstra :: AllegraEraOnwards DijkstraEra

deriving instance Show (AllegraEraOnwards era)

deriving instance Eq (AllegraEraOnwards era)

instance Eon AllegraEraOnwards where
  inEonForEra no yes = \case
    ByronEra -> no
    ShelleyEra -> no
    AllegraEra -> yes AllegraEraOnwardsAllegra
    MaryEra -> yes AllegraEraOnwardsMary
    AlonzoEra -> yes AllegraEraOnwardsAlonzo
    BabbageEra -> yes AllegraEraOnwardsBabbage
    ConwayEra -> yes AllegraEraOnwardsConway
    DijkstraEra -> yes AllegraEraOnwardsDijkstra

instance ToCardanoEra AllegraEraOnwards where
  toCardanoEra = \case
    AllegraEraOnwardsAllegra -> AllegraEra
    AllegraEraOnwardsMary -> MaryEra
    AllegraEraOnwardsAlonzo -> AlonzoEra
    AllegraEraOnwardsBabbage -> BabbageEra
    AllegraEraOnwardsConway -> ConwayEra
    AllegraEraOnwardsDijkstra -> DijkstraEra

instance Convert AllegraEraOnwards CardanoEra where
  convert = toCardanoEra

instance Convert AllegraEraOnwards ShelleyBasedEra where
  convert = \case
    AllegraEraOnwardsAllegra -> ShelleyBasedEraAllegra
    AllegraEraOnwardsMary -> ShelleyBasedEraMary
    AllegraEraOnwardsAlonzo -> ShelleyBasedEraAlonzo
    AllegraEraOnwardsBabbage -> ShelleyBasedEraBabbage
    AllegraEraOnwardsConway -> ShelleyBasedEraConway
    AllegraEraOnwardsDijkstra -> ShelleyBasedEraDijkstra

type AllegraEraOnwardsConstraints era =
  ( C.HashAlgorithm L.HASH
  , C.Signable (L.VRF L.StandardCrypto) L.Seed
  , Consensus.PraosProtocolSupportsNode (ConsensusProtocol era)
  , Consensus.ShelleyBlock (ConsensusProtocol era) (ShelleyLedgerEra era) ~ ConsensusBlockForEra era
  , Consensus.ShelleyCompatible (ConsensusProtocol era) (ShelleyLedgerEra era)
  , L.ADDRHASH ~ Blake2b.Blake2b_224
  , L.Era (ShelleyLedgerEra era)
  , L.EraPParams (ShelleyLedgerEra era)
  , L.EraTx (ShelleyLedgerEra era)
  , L.EraTxBody (ShelleyLedgerEra era)
  , L.EraTxOut (ShelleyLedgerEra era)
  , L.HashAnnotated (L.TxBody (ShelleyLedgerEra era)) L.EraIndependentTxBody
  , L.AllegraEraTxBody (ShelleyLedgerEra era)
  , L.ShelleyEraTxCert (ShelleyLedgerEra era)
  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , FromCBOR (DebugLedgerState era)
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , ToJSON (Consensus.ChainDepState (ConsensusProtocol era))
  , ToJSON (DebugLedgerState era)
  , Typeable era
  , (era == ByronEra) ~ False
  )

allegraEraOnwardsConstraints
  :: ()
  => AllegraEraOnwards era
  -> (AllegraEraOnwardsConstraints era => a)
  -> a
allegraEraOnwardsConstraints = \case
  AllegraEraOnwardsAllegra -> id
  AllegraEraOnwardsMary -> id
  AllegraEraOnwardsAlonzo -> id
  AllegraEraOnwardsBabbage -> id
  AllegraEraOnwardsConway -> id
  _ -> const $ error "allegraEraOnwardsConstraints: Dijkstra era not supported"

{-# DEPRECATED allegraEraOnwardsToShelleyBasedEra "Use 'convert' instead." #-}
allegraEraOnwardsToShelleyBasedEra :: AllegraEraOnwards era -> ShelleyBasedEra era
allegraEraOnwardsToShelleyBasedEra = convert

class IsShelleyBasedEra era => IsAllegraBasedEra era where
  allegraBasedEra :: AllegraEraOnwards era

instance IsAllegraBasedEra AllegraEra where
  allegraBasedEra = AllegraEraOnwardsAllegra

instance IsAllegraBasedEra MaryEra where
  allegraBasedEra = AllegraEraOnwardsMary

instance IsAllegraBasedEra AlonzoEra where
  allegraBasedEra = AllegraEraOnwardsAlonzo

instance IsAllegraBasedEra BabbageEra where
  allegraBasedEra = AllegraEraOnwardsBabbage

instance IsAllegraBasedEra ConwayEra where
  allegraBasedEra = AllegraEraOnwardsConway
