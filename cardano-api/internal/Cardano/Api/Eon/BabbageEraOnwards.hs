{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Eon.BabbageEraOnwards
  ( BabbageEraOnwards (..)
  , babbageEraOnwardsConstraints
  , babbageEraOnwardsToShelleyBasedEra
  , BabbageEraOnwardsConstraints
  , IsBabbageBasedEra (..)
  )
where

import           Cardano.Api.Eon.AlonzoEraOnwards
import           Cardano.Api.Eon.Convert
import           Cardano.Api.Eon.MaryEraOnwards
import           Cardano.Api.Eon.ShelleyBasedEra
import           Cardano.Api.Eras.Core
import           Cardano.Api.Modes
import           Cardano.Api.Query.Types

import           Cardano.Binary
import qualified Cardano.Crypto.Hash.Blake2b as Blake2b
import qualified Cardano.Crypto.Hash.Class as C
import qualified Cardano.Crypto.VRF as C
import qualified Cardano.Ledger.Alonzo.Scripts as L
import qualified Cardano.Ledger.Alonzo.UTxO as L
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.Babbage.TxOut as L
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

data BabbageEraOnwards era where
  BabbageEraOnwardsBabbage :: BabbageEraOnwards BabbageEra
  BabbageEraOnwardsConway :: BabbageEraOnwards ConwayEra

deriving instance Show (BabbageEraOnwards era)

deriving instance Eq (BabbageEraOnwards era)

instance Eon BabbageEraOnwards where
  inEonForEra no yes = \case
    ByronEra -> no
    ShelleyEra -> no
    AllegraEra -> no
    MaryEra -> no
    AlonzoEra -> no
    BabbageEra -> yes BabbageEraOnwardsBabbage
    ConwayEra -> yes BabbageEraOnwardsConway

instance ToCardanoEra BabbageEraOnwards where
  toCardanoEra = \case
    BabbageEraOnwardsBabbage -> BabbageEra
    BabbageEraOnwardsConway -> ConwayEra

instance Convert BabbageEraOnwards CardanoEra where
  convert = toCardanoEra

instance Convert BabbageEraOnwards ShelleyBasedEra where
  convert = \case
    BabbageEraOnwardsBabbage -> ShelleyBasedEraBabbage
    BabbageEraOnwardsConway -> ShelleyBasedEraConway

instance Convert BabbageEraOnwards MaryEraOnwards where
  convert = \case
    BabbageEraOnwardsBabbage -> MaryEraOnwardsBabbage
    BabbageEraOnwardsConway -> MaryEraOnwardsConway

type BabbageEraOnwardsConstraints era =
  ( C.HashAlgorithm (L.HASH (L.EraCrypto (ShelleyLedgerEra era)))
  , C.Signable (L.VRF (L.EraCrypto (ShelleyLedgerEra era))) L.Seed
  , Consensus.PraosProtocolSupportsNode (ConsensusProtocol era)
  , Consensus.ShelleyBlock (ConsensusProtocol era) (ShelleyLedgerEra era) ~ ConsensusBlockForEra era
  , Consensus.ShelleyCompatible (ConsensusProtocol era) (ShelleyLedgerEra era)
  , L.ADDRHASH (Consensus.PraosProtocolSupportsNodeCrypto (ConsensusProtocol era)) ~ Blake2b.Blake2b_224
  , L.AlonzoEraTxOut (ShelleyLedgerEra era)
  , L.BabbageEraPParams (ShelleyLedgerEra era)
  , L.BabbageEraTxBody (ShelleyLedgerEra era)
  , L.BabbageEraTxOut (ShelleyLedgerEra era)
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
  , L.Script (ShelleyLedgerEra era) ~ L.AlonzoScript (ShelleyLedgerEra era)
  , L.ScriptsNeeded (ShelleyLedgerEra era) ~ L.AlonzoScriptsNeeded (ShelleyLedgerEra era)
  , L.ShelleyEraTxCert (ShelleyLedgerEra era)
  , L.TxOut (ShelleyLedgerEra era) ~ L.BabbageTxOut (ShelleyLedgerEra era)
  , L.Value (ShelleyLedgerEra era) ~ L.MaryValue L.StandardCrypto
  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , FromCBOR (DebugLedgerState era)
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , ToJSON (DebugLedgerState era)
  , Typeable era
  )

babbageEraOnwardsConstraints
  :: ()
  => BabbageEraOnwards era
  -> (BabbageEraOnwardsConstraints era => a)
  -> a
babbageEraOnwardsConstraints = \case
  BabbageEraOnwardsBabbage -> id
  BabbageEraOnwardsConway -> id

{-# DEPRECATED babbageEraOnwardsToShelleyBasedEra "Use 'convert' instead." #-}
babbageEraOnwardsToShelleyBasedEra :: BabbageEraOnwards era -> ShelleyBasedEra era
babbageEraOnwardsToShelleyBasedEra = convert

class IsAlonzoBasedEra era => IsBabbageBasedEra era where
  babbageBasedEra :: BabbageEraOnwards era

instance IsBabbageBasedEra BabbageEra where
  babbageBasedEra = BabbageEraOnwardsBabbage

instance IsBabbageBasedEra ConwayEra where
  babbageBasedEra = BabbageEraOnwardsConway
