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

module Cardano.Api.Internal.Eon.MaryEraOnwards
  ( MaryEraOnwards (..)
  , maryEraOnwardsConstraints
  , maryEraOnwardsToShelleyBasedEra
  , MaryEraOnwardsConstraints
  , IsMaryBasedEra (..)
  )
where

import Cardano.Api.Internal.Eon.AllegraEraOnwards
import Cardano.Api.Internal.Eon.Convert
import Cardano.Api.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Internal.Eras.Core
import Cardano.Api.Internal.Modes
import Cardano.Api.Internal.Query.Types

import Cardano.Binary
import Cardano.Crypto.Hash.Blake2b qualified as Blake2b
import Cardano.Crypto.Hash.Class qualified as C
import Cardano.Crypto.VRF qualified as C
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
import Data.Typeable (Typeable)

data MaryEraOnwards era where
  MaryEraOnwardsMary :: MaryEraOnwards MaryEra
  MaryEraOnwardsAlonzo :: MaryEraOnwards AlonzoEra
  MaryEraOnwardsBabbage :: MaryEraOnwards BabbageEra
  MaryEraOnwardsConway :: MaryEraOnwards ConwayEra

deriving instance Show (MaryEraOnwards era)

deriving instance Eq (MaryEraOnwards era)

instance Eon MaryEraOnwards where
  inEonForEra no yes = \case
    ByronEra -> no
    ShelleyEra -> no
    AllegraEra -> no
    MaryEra -> yes MaryEraOnwardsMary
    AlonzoEra -> yes MaryEraOnwardsAlonzo
    BabbageEra -> yes MaryEraOnwardsBabbage
    ConwayEra -> yes MaryEraOnwardsConway

instance ToCardanoEra MaryEraOnwards where
  toCardanoEra = \case
    MaryEraOnwardsMary -> MaryEra
    MaryEraOnwardsAlonzo -> AlonzoEra
    MaryEraOnwardsBabbage -> BabbageEra
    MaryEraOnwardsConway -> ConwayEra

instance Convert MaryEraOnwards CardanoEra where
  convert = toCardanoEra

instance Convert MaryEraOnwards ShelleyBasedEra where
  convert = \case
    MaryEraOnwardsMary -> ShelleyBasedEraMary
    MaryEraOnwardsAlonzo -> ShelleyBasedEraAlonzo
    MaryEraOnwardsBabbage -> ShelleyBasedEraBabbage
    MaryEraOnwardsConway -> ShelleyBasedEraConway

type MaryEraOnwardsConstraints era =
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
  , L.EraUTxO (ShelleyLedgerEra era)
  , L.HashAnnotated (L.TxBody (ShelleyLedgerEra era)) L.EraIndependentTxBody
  , L.MaryEraTxBody (ShelleyLedgerEra era)
  , L.ShelleyEraTxCert (ShelleyLedgerEra era)
  , L.Value (ShelleyLedgerEra era) ~ L.MaryValue
  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , FromCBOR (DebugLedgerState era)
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , ToJSON (DebugLedgerState era)
  , Typeable era
  )

maryEraOnwardsConstraints
  :: ()
  => MaryEraOnwards era
  -> (MaryEraOnwardsConstraints era => a)
  -> a
maryEraOnwardsConstraints = \case
  MaryEraOnwardsMary -> id
  MaryEraOnwardsAlonzo -> id
  MaryEraOnwardsBabbage -> id
  MaryEraOnwardsConway -> id

{-# DEPRECATED maryEraOnwardsToShelleyBasedEra "Use 'convert' instead." #-}
maryEraOnwardsToShelleyBasedEra :: MaryEraOnwards era -> ShelleyBasedEra era
maryEraOnwardsToShelleyBasedEra = convert

class IsAllegraBasedEra era => IsMaryBasedEra era where
  maryBasedEra :: MaryEraOnwards era

instance IsMaryBasedEra MaryEra where
  maryBasedEra = MaryEraOnwardsMary

instance IsMaryBasedEra AlonzoEra where
  maryBasedEra = MaryEraOnwardsAlonzo

instance IsMaryBasedEra BabbageEra where
  maryBasedEra = MaryEraOnwardsBabbage

instance IsMaryBasedEra ConwayEra where
  maryBasedEra = MaryEraOnwardsConway
