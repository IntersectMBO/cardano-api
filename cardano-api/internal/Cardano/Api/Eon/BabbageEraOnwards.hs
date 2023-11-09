{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Eon.BabbageEraOnwards
  ( BabbageEraOnwards(..)
  , babbageEraOnwardsConstraints
  , babbageEraOnwardsToCardanoEra
  , babbageEraOnwardsToShelleyBasedEra

  , BabbageEraOnwardsConstraints
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
import qualified Cardano.Ledger.Alonzo.TxInfo as L
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
  BabbageEraOnwardsConway  :: BabbageEraOnwards ConwayEra

deriving instance Show (BabbageEraOnwards era)
deriving instance Eq (BabbageEraOnwards era)

instance Eon BabbageEraOnwards where
  inEonForEra no yes = \case
    ByronEra    -> no
    ShelleyEra  -> no
    AllegraEra  -> no
    MaryEra     -> no
    AlonzoEra   -> no
    BabbageEra  -> yes BabbageEraOnwardsBabbage
    ConwayEra   -> yes BabbageEraOnwardsConway

instance ToCardanoEra BabbageEraOnwards where
  toCardanoEra = \case
    BabbageEraOnwardsBabbage -> BabbageEra
    BabbageEraOnwardsConway  -> ConwayEra

type BabbageEraOnwardsConstraints era =
  ( C.HashAlgorithm (L.HASH (L.EraCrypto (LedgerEra era)))
  , C.Signable (L.VRF (L.EraCrypto (LedgerEra era))) L.Seed
  , Consensus.PraosProtocolSupportsNode (ConsensusProtocol era)
  , Consensus.ShelleyBlock (ConsensusProtocol era) (LedgerEra era) ~ ConsensusBlockForEra era
  , Consensus.ShelleyCompatible (ConsensusProtocol era) (LedgerEra era)
  , L.ADDRHASH (Consensus.PraosProtocolSupportsNodeCrypto (ConsensusProtocol era)) ~ Blake2b.Blake2b_224
  , L.AlonzoEraTxOut (LedgerEra era)
  , L.BabbageEraPParams (LedgerEra era)
  , L.BabbageEraTxBody (LedgerEra era)
  , L.BabbageEraTxOut (LedgerEra era)
  , L.Crypto (L.EraCrypto (LedgerEra era))
  , L.Era (LedgerEra era)
  , L.EraCrypto (LedgerEra era) ~ L.StandardCrypto
  , L.EraPlutusContext 'L.PlutusV1 (LedgerEra era)
  , L.EraPParams (LedgerEra era)
  , L.EraTx (LedgerEra era)
  , L.EraTxBody (LedgerEra era)
  , L.EraTxOut (LedgerEra era)
  , L.EraUTxO (LedgerEra era)
  , L.ExtendedUTxO (LedgerEra era)
  , L.HashAnnotated (L.TxBody (LedgerEra era)) L.EraIndependentTxBody L.StandardCrypto
  , L.MaryEraTxBody (LedgerEra era)
  , L.Script (LedgerEra era) ~ L.AlonzoScript (LedgerEra era)
  , L.ScriptsNeeded (LedgerEra era) ~ L.AlonzoScriptsNeeded (LedgerEra era)
  , L.ShelleyEraTxBody (LedgerEra era)
  , L.ShelleyEraTxCert (LedgerEra era)
  , L.TxOut (LedgerEra era) ~ L.BabbageTxOut (LedgerEra era)
  , L.Value (LedgerEra era) ~ L.MaryValue L.StandardCrypto

  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , FromCBOR (DebugLedgerState era)
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , ToJSON (DebugLedgerState era)
  , Typeable era
  )

babbageEraOnwardsConstraints :: ()
  => BabbageEraOnwards era
  -> (BabbageEraOnwardsConstraints era => a)
  -> a
babbageEraOnwardsConstraints = \case
  BabbageEraOnwardsBabbage -> id
  BabbageEraOnwardsConway  -> id

babbageEraOnwardsToCardanoEra :: BabbageEraOnwards era -> CardanoEra era
babbageEraOnwardsToCardanoEra = shelleyBasedToCardanoEra . babbageEraOnwardsToShelleyBasedEra

babbageEraOnwardsToShelleyBasedEra :: BabbageEraOnwards era -> ShelleyBasedEra era
babbageEraOnwardsToShelleyBasedEra = \case
  BabbageEraOnwardsBabbage -> ShelleyBasedEraBabbage
  BabbageEraOnwardsConway  -> ShelleyBasedEraConway
