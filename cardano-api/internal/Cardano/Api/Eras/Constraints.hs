{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

module Cardano.Api.Eras.Constraints
  ( cardanoEraConstraints
  , withShelleyBasedEraConstraintsForLedger
  , shelleyBasedEraConstraints
  ) where

import           Cardano.Api.Eras.Core
import           Cardano.Api.Modes
import           Cardano.Api.Orphans ()
import           Cardano.Api.Query.Types

import qualified Cardano.Crypto.Hash.Blake2b as Blake2b
import qualified Cardano.Crypto.Hash.Class as C
import qualified Cardano.Crypto.VRF as C
import qualified Cardano.Ledger.Api as L
import qualified Cardano.Ledger.BaseTypes as L
import           Cardano.Ledger.Binary (FromCBOR)
import qualified Cardano.Ledger.Core as L
import qualified Cardano.Ledger.SafeHash as L
import qualified Ouroboros.Consensus.Protocol.Abstract as Consensus
import qualified Ouroboros.Consensus.Protocol.Praos.Common as Consensus
import qualified Ouroboros.Consensus.Shelley.Ledger as Consensus

import           Data.Aeson (ToJSON)
import           Data.Typeable (Typeable)

type CardanoEraConstraint era =
  ( Typeable era
  , IsCardanoEra era
  )

cardanoEraConstraints :: ()
  => CardanoEra era
  -> (CardanoEraConstraint era => a)
  -> a
cardanoEraConstraints = \case
  ByronEra   -> id
  ShelleyEra -> id
  AllegraEra -> id
  MaryEra    -> id
  AlonzoEra  -> id
  BabbageEra -> id
  ConwayEra  -> id

type ShelleyBasedEraConstraints era =
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
  , L.ShelleyEraTxBody (ShelleyLedgerEra era)
  , FromCBOR (Consensus.ChainDepState (ConsensusProtocol era))
  , FromCBOR (DebugLedgerState era)
  , IsCardanoEra era
  , IsShelleyBasedEra era
  , ToJSON (Consensus.ChainDepState (ConsensusProtocol era))
  , ToJSON (DebugLedgerState era)
  , Typeable era
  )

shelleyBasedEraConstraints :: ()
  => ShelleyBasedEra era
  -> (ShelleyBasedEraConstraints era => a)
  -> a
shelleyBasedEraConstraints = \case
  ShelleyBasedEraShelley -> id
  ShelleyBasedEraAllegra -> id
  ShelleyBasedEraMary    -> id
  ShelleyBasedEraAlonzo  -> id
  ShelleyBasedEraBabbage -> id
  ShelleyBasedEraConway  -> id

-- Deprecated: Use shelleyBasedEraConstraints instead.
withShelleyBasedEraConstraintsForLedger :: ()
  => ShelleyBasedEra era
  -> (ShelleyBasedEraConstraints era => a)
  -> a
withShelleyBasedEraConstraintsForLedger = shelleyBasedEraConstraints
