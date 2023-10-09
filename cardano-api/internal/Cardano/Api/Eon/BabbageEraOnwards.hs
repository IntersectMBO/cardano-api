{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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

import           Cardano.Api.EasyEvidence
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
import qualified Cardano.Ledger.BaseTypes as L
import qualified Cardano.Ledger.Core as L
import qualified Cardano.Ledger.SafeHash as L
import qualified Cardano.Ledger.UTxO as L
import qualified Ouroboros.Consensus.Protocol.Abstract as Consensus
import qualified Ouroboros.Consensus.Protocol.Praos.Common as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus

import           Data.Aeson
import           Data.Typeable (Typeable)

instance EasyEvidence BabbageEraOnwards era where
  easyEvidence sbe =
    case sbe of
      ByronEra -> Nothing
      ShelleyEra -> Nothing
      AllegraEra -> Nothing
      MaryEra -> Nothing
      AlonzoEra -> Nothing
      BabbageEra -> Just BabbageEraOnwardsBabbage
      ConwayEra -> Just BabbageEraOnwardsConway

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
  ( C.HashAlgorithm (L.HASH (L.EraCrypto (ShelleyLedgerEra era)))
  , C.Signable (L.VRF (L.EraCrypto (ShelleyLedgerEra era))) L.Seed
  , Consensus.PraosProtocolSupportsNode (ConsensusProtocol era)
  , Consensus.ShelleyCompatible (ConsensusProtocol era) (ShelleyLedgerEra era)
  , L.ADDRHASH (Consensus.PraosProtocolSupportsNodeCrypto (ConsensusProtocol era)) ~ Blake2b.Blake2b_224
  , L.BabbageEraPParams (ShelleyLedgerEra era)
  , L.BabbageEraTxBody (ShelleyLedgerEra era)
  , L.BabbageEraTxOut (ShelleyLedgerEra era)
  , L.Crypto (L.EraCrypto (ShelleyLedgerEra era))
  , L.Era (ShelleyLedgerEra era)
  , L.EraCrypto (ShelleyLedgerEra era) ~ L.StandardCrypto
  , L.EraPlutusContext 'L.PlutusV1 (ShelleyLedgerEra era)
  , L.EraPParams (ShelleyLedgerEra era)
  , L.EraTx (ShelleyLedgerEra era)
  , L.EraTxBody (ShelleyLedgerEra era)
  , L.EraUTxO (ShelleyLedgerEra era)
  , L.ExtendedUTxO (ShelleyLedgerEra era)
  , L.HashAnnotated (L.TxBody (ShelleyLedgerEra era)) L.EraIndependentTxBody L.StandardCrypto
  , L.Script (ShelleyLedgerEra era) ~ L.AlonzoScript (ShelleyLedgerEra era)
  , L.ScriptsNeeded (ShelleyLedgerEra era) ~ L.AlonzoScriptsNeeded (ShelleyLedgerEra era)
  , L.ShelleyEraTxBody (ShelleyLedgerEra era)
  , L.ShelleyEraTxCert (ShelleyLedgerEra era)

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
