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
  ( AlonzoEraOnwards(..)
  , alonzoEraOnwardsConstraints
  , alonzoEraOnwardsToCardanoEra
  , alonzoEraOnwardsToShelleyBasedEra

  , AlonzoEraOnwardsConstraints
  ) where

import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eras.Core
import           Cardano.Api.Modes
import           Cardano.Api.Query.Types

import           Cardano.Binary
import qualified Cardano.Crypto.Hash.Blake2b as Blake2b
import qualified Cardano.Crypto.Hash.Class as C
import qualified Cardano.Crypto.VRF as C
import qualified Cardano.Ledger.Alonzo.Language as L
import qualified Cardano.Ledger.Alonzo.Scripts as L
import qualified Cardano.Ledger.Alonzo.Tx as L
import qualified Cardano.Ledger.Alonzo.TxInfo as L
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
  AlonzoEraOnwardsAlonzo  :: AlonzoEraOnwards AlonzoEra
  AlonzoEraOnwardsBabbage :: AlonzoEraOnwards BabbageEra
  AlonzoEraOnwardsConway  :: AlonzoEraOnwards ConwayEra

deriving instance Show (AlonzoEraOnwards era)
deriving instance Eq (AlonzoEraOnwards era)

instance Eon AlonzoEraOnwards where
  inEonForEra no yes = \case
    ByronEra    -> no
    ShelleyEra  -> no
    AllegraEra  -> no
    MaryEra     -> no
    AlonzoEra   -> yes AlonzoEraOnwardsAlonzo
    BabbageEra  -> yes AlonzoEraOnwardsBabbage
    ConwayEra   -> yes AlonzoEraOnwardsConway

instance ToCardanoEra AlonzoEraOnwards where
  toCardanoEra = \case
    AlonzoEraOnwardsAlonzo  -> AlonzoEra
    AlonzoEraOnwardsBabbage -> BabbageEra
    AlonzoEraOnwardsConway  -> ConwayEra

type AlonzoEraOnwardsConstraints era =
  ( C.HashAlgorithm (L.HASH (L.EraCrypto (LedgerEra era)))
  , C.Signable (L.VRF (L.EraCrypto (LedgerEra era))) L.Seed
  , Consensus.PraosProtocolSupportsNode (ConsensusProtocol era)
  , Consensus.ShelleyBlock (ConsensusProtocol era) (LedgerEra era) ~ ConsensusBlockForEra era
  , Consensus.ShelleyCompatible (ConsensusProtocol era) (LedgerEra era)
  , L.ADDRHASH (Consensus.PraosProtocolSupportsNodeCrypto (ConsensusProtocol era)) ~ Blake2b.Blake2b_224
  , L.AlonzoEraPParams (LedgerEra era)
  , L.AlonzoEraTx (LedgerEra era)
  , L.AlonzoEraTxBody (LedgerEra era)
  , L.AlonzoEraTxOut (LedgerEra era)
  , L.AlonzoEraTxWits (LedgerEra era)
  , L.Crypto (L.EraCrypto (LedgerEra era))
  , L.Era (LedgerEra era)
  , L.EraCrypto (LedgerEra era) ~ L.StandardCrypto
  , L.EraPlutusContext 'L.PlutusV1 (LedgerEra era)
  , L.EraPParams (LedgerEra era)
  , L.EraTx (LedgerEra era)
  , L.EraTxBody (LedgerEra era)
  , L.EraUTxO (LedgerEra era)
  , L.ExtendedUTxO (LedgerEra era)
  , L.HashAnnotated (L.TxBody (LedgerEra era)) L.EraIndependentTxBody L.StandardCrypto
  , L.MaryEraTxBody (LedgerEra era)
  , L.Script (LedgerEra era) ~ L.AlonzoScript (LedgerEra era)
  , L.ScriptsNeeded (LedgerEra era) ~ L.AlonzoScriptsNeeded (LedgerEra era)
  , L.ShelleyEraTxBody (LedgerEra era)
  , L.ShelleyEraTxCert (LedgerEra era)
  , L.Value (LedgerEra era) ~ L.MaryValue L.StandardCrypto

  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , FromCBOR (DebugLedgerState era)
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , ToJSON (DebugLedgerState era)
  , Typeable era
  )

alonzoEraOnwardsConstraints :: ()
  => AlonzoEraOnwards era
  -> (AlonzoEraOnwardsConstraints era => a)
  -> a
alonzoEraOnwardsConstraints = \case
  AlonzoEraOnwardsAlonzo  -> id
  AlonzoEraOnwardsBabbage -> id
  AlonzoEraOnwardsConway  -> id

alonzoEraOnwardsToCardanoEra :: AlonzoEraOnwards era -> CardanoEra era
alonzoEraOnwardsToCardanoEra = shelleyBasedToCardanoEra . alonzoEraOnwardsToShelleyBasedEra

alonzoEraOnwardsToShelleyBasedEra :: AlonzoEraOnwards era -> ShelleyBasedEra era
alonzoEraOnwardsToShelleyBasedEra = \case
  AlonzoEraOnwardsAlonzo  -> ShelleyBasedEraAlonzo
  AlonzoEraOnwardsBabbage -> ShelleyBasedEraBabbage
  AlonzoEraOnwardsConway  -> ShelleyBasedEraConway
