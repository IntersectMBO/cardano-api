{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Eon.AlonzoEraOnwards
  ( AlonzoEraOnwards (..)
  , alonzoEraOnwardsConstraints
  , alonzoEraOnwardsToShelleyBasedEra
  , AlonzoEraOnwardsConstraints
  , IsAlonzoBasedEra (..)
  )
where

import           Cardano.Api.Eon.MaryEraOnwards
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eras.Core
import           Cardano.Api.Modes
import           Cardano.Api.Query.Types

import           Cardano.Binary
import qualified Cardano.Crypto.Hash.Blake2b as Blake2b
import qualified Cardano.Crypto.Hash.Class as C
import qualified Cardano.Crypto.VRF as C
import qualified Cardano.Ledger.Alonzo.Plutus.Context as Plutus
import qualified Cardano.Ledger.Alonzo.Scripts as L
import qualified Cardano.Ledger.Alonzo.Tx as L
import qualified Cardano.Ledger.Alonzo.TxWits as L
import qualified Cardano.Ledger.Alonzo.UTxO as L
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.BaseTypes as L
import qualified Cardano.Ledger.Core as L
import qualified Cardano.Ledger.Mary.Value as L
import qualified Cardano.Ledger.SafeHash as L
import qualified Cardano.Ledger.UTxO as L
import qualified Ouroboros.Consensus.Protocol.Abstract as Consensus
import qualified Ouroboros.Consensus.Protocol.Praos.Common as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus

import           Data.Aeson
import           Data.Typeable (Typeable)

data AlonzoEraOnwards era where
  AlonzoEraOnwardsAlonzo :: AlonzoEraOnwards AlonzoEra
  AlonzoEraOnwardsBabbage :: AlonzoEraOnwards BabbageEra
  AlonzoEraOnwardsConway :: AlonzoEraOnwards ConwayEra

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

instance ToCardanoEra AlonzoEraOnwards where
  toCardanoEra = \case
    AlonzoEraOnwardsAlonzo -> AlonzoEra
    AlonzoEraOnwardsBabbage -> BabbageEra
    AlonzoEraOnwardsConway -> ConwayEra

type AlonzoEraOnwardsConstraints era =
  ( C.HashAlgorithm (L.HASH (L.EraCrypto (ShelleyLedgerEra era)))
  , C.Signable (L.VRF (L.EraCrypto (ShelleyLedgerEra era))) L.Seed
  , Consensus.PraosProtocolSupportsNode (ConsensusProtocol era)
  , Consensus.ShelleyBlock (ConsensusProtocol era) (ShelleyLedgerEra era) ~ ConsensusBlockForEra era
  , Consensus.ShelleyCompatible (ConsensusProtocol era) (ShelleyLedgerEra era)
  , L.ADDRHASH (Consensus.PraosProtocolSupportsNodeCrypto (ConsensusProtocol era)) ~ Blake2b.Blake2b_224
  , L.AlonzoEraPParams (ShelleyLedgerEra era)
  , L.AlonzoEraTx (ShelleyLedgerEra era)
  , L.AlonzoEraTxBody (ShelleyLedgerEra era)
  , L.AlonzoEraTxOut (ShelleyLedgerEra era)
  , L.AlonzoEraTxWits (ShelleyLedgerEra era)
  , L.Crypto (L.EraCrypto (ShelleyLedgerEra era))
  , L.Era (ShelleyLedgerEra era)
  , L.EraCrypto (ShelleyLedgerEra era) ~ L.StandardCrypto
  , L.EraPParams (ShelleyLedgerEra era)
  , L.EraTx (ShelleyLedgerEra era)
  , L.EraTxBody (ShelleyLedgerEra era)
  , L.EraTxOut (ShelleyLedgerEra era)
  , L.EraUTxO (ShelleyLedgerEra era)
  , L.HashAnnotated (L.TxBody (ShelleyLedgerEra era)) L.EraIndependentTxBody L.StandardCrypto
  , L.MaryEraTxBody (ShelleyLedgerEra era)
  , Plutus.EraPlutusContext (ShelleyLedgerEra era)
  , L.Script (ShelleyLedgerEra era) ~ L.AlonzoScript (ShelleyLedgerEra era)
  , L.ScriptsNeeded (ShelleyLedgerEra era) ~ L.AlonzoScriptsNeeded (ShelleyLedgerEra era)
  , L.ShelleyEraTxCert (ShelleyLedgerEra era)
  , L.Value (ShelleyLedgerEra era) ~ L.MaryValue L.StandardCrypto
  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , FromCBOR (DebugLedgerState era)
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , ToJSON (DebugLedgerState era)
  , Typeable era
  )

alonzoEraOnwardsConstraints
  :: AlonzoEraOnwards era
  -> (AlonzoEraOnwardsConstraints era => a)
  -> a
alonzoEraOnwardsConstraints = \case
  AlonzoEraOnwardsAlonzo -> id
  AlonzoEraOnwardsBabbage -> id
  AlonzoEraOnwardsConway -> id

alonzoEraOnwardsToShelleyBasedEra :: AlonzoEraOnwards era -> ShelleyBasedEra era
alonzoEraOnwardsToShelleyBasedEra = \case
  AlonzoEraOnwardsAlonzo -> ShelleyBasedEraAlonzo
  AlonzoEraOnwardsBabbage -> ShelleyBasedEraBabbage
  AlonzoEraOnwardsConway -> ShelleyBasedEraConway

class IsMaryBasedEra era => IsAlonzoBasedEra era where
  alonzoBasedEra :: AlonzoEraOnwards era

instance IsAlonzoBasedEra AlonzoEra where
  alonzoBasedEra = AlonzoEraOnwardsAlonzo

instance IsAlonzoBasedEra BabbageEra where
  alonzoBasedEra = AlonzoEraOnwardsBabbage

instance IsAlonzoBasedEra ConwayEra where
  alonzoBasedEra = AlonzoEraOnwardsConway
