{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- | Transactions in the context of a consensus mode, and other types used in
-- the transaction submission protocol.
module Cardano.Api.Internal.InMode
  ( -- * Transaction in a consensus mode
    TxInMode (..)
  , fromConsensusGenTx
  , toConsensusGenTx

    -- * Transaction id in a consensus mode
  , TxIdInMode (..)
  , toConsensusTxId

    -- * Transaction validation errors
  , TxValidationError (..)
  , TxValidationErrorInCardanoMode (..)
  , fromConsensusApplyTxErr
  )
where

import Cardano.Api.Internal.Eon.ShelleyBasedEra
import Cardano.Api.Internal.Eras
import Cardano.Api.Internal.Modes
import Cardano.Api.Internal.Orphans ()
import Cardano.Api.Internal.Tx.Body
import Cardano.Api.Internal.Tx.Sign
import Cardano.Api.Internal.Utils (textShow)

import Cardano.Protocol.Crypto (StandardCrypto)
import Ouroboros.Consensus.Byron.Ledger qualified as Consensus
import Ouroboros.Consensus.Cardano.Block qualified as Consensus
import Ouroboros.Consensus.HardFork.Combinator qualified as Consensus
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (EraMismatch)
import Ouroboros.Consensus.Ledger.SupportsMempool qualified as Consensus
import Ouroboros.Consensus.Shelley.HFEras qualified as Consensus
import Ouroboros.Consensus.Shelley.Ledger qualified as Consensus
import Ouroboros.Consensus.TypeFamilyWrappers qualified as Consensus

import Data.Aeson (ToJSON (..), (.=))
import Data.Aeson qualified as Aeson
import Data.SOP.Strict (NS (S, Z))
import Data.Text qualified as Text
import GHC.Generics

-- ----------------------------------------------------------------------------
-- Transactions in the context of a consensus mode
--

-- | A 'Tx' in one of the eras supported by a given protocol mode.
--
-- For multi-era modes such as the 'CardanoMode' this type is a sum of the
-- different transaction types for all the eras. It is used in the
-- LocalTxSubmission protocol.
data TxInMode where
  -- | Shelley based transactions.
  TxInMode
    :: ShelleyBasedEra era
    -> Tx era
    -> TxInMode
  -- | Legacy Byron transactions and things we can
  -- post to the chain which are not actually transactions.
  -- This covers: update proposals, votes and delegation certs.
  TxInByronSpecial
    :: Consensus.GenTx Consensus.ByronBlock
    -> TxInMode

deriving instance Show TxInMode

fromConsensusGenTx
  :: ()
  => Consensus.CardanoBlock StandardCrypto ~ block
  => Consensus.GenTx block
  -> TxInMode
fromConsensusGenTx = \case
  Consensus.HardForkGenTx (Consensus.OneEraGenTx (Z tx')) ->
    TxInByronSpecial tx'
  Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (Z tx'))) ->
    let Consensus.ShelleyTx _txid shelleyEraTx = tx'
     in TxInMode ShelleyBasedEraShelley (ShelleyTx ShelleyBasedEraShelley shelleyEraTx)
  Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (Z tx')))) ->
    let Consensus.ShelleyTx _txid shelleyEraTx = tx'
     in TxInMode ShelleyBasedEraAllegra (ShelleyTx ShelleyBasedEraAllegra shelleyEraTx)
  Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (S (Z tx'))))) ->
    let Consensus.ShelleyTx _txid shelleyEraTx = tx'
     in TxInMode ShelleyBasedEraMary (ShelleyTx ShelleyBasedEraMary shelleyEraTx)
  Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (S (S (Z tx')))))) ->
    let Consensus.ShelleyTx _txid shelleyEraTx = tx'
     in TxInMode ShelleyBasedEraAlonzo (ShelleyTx ShelleyBasedEraAlonzo shelleyEraTx)
  Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (S (S (S (Z tx'))))))) ->
    let Consensus.ShelleyTx _txid shelleyEraTx = tx'
     in TxInMode ShelleyBasedEraBabbage (ShelleyTx ShelleyBasedEraBabbage shelleyEraTx)
  Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (S (S (S (S (Z tx')))))))) ->
    let Consensus.ShelleyTx _txid shelleyEraTx = tx'
     in TxInMode ShelleyBasedEraConway (ShelleyTx ShelleyBasedEraConway shelleyEraTx)

toConsensusGenTx
  :: ()
  => Consensus.CardanoBlock StandardCrypto ~ block
  => TxInMode
  -> Consensus.GenTx block
toConsensusGenTx (TxInByronSpecial gtx) =
  Consensus.HardForkGenTx (Consensus.OneEraGenTx (Z gtx))
toConsensusGenTx (TxInMode ShelleyBasedEraShelley (ShelleyTx _ tx)) =
  Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (Z tx')))
 where
  tx' = Consensus.mkShelleyTx tx
toConsensusGenTx (TxInMode ShelleyBasedEraAllegra (ShelleyTx _ tx)) =
  Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (Z tx'))))
 where
  tx' = Consensus.mkShelleyTx tx
toConsensusGenTx (TxInMode ShelleyBasedEraMary (ShelleyTx _ tx)) =
  Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (S (Z tx')))))
 where
  tx' = Consensus.mkShelleyTx tx
toConsensusGenTx (TxInMode ShelleyBasedEraAlonzo (ShelleyTx _ tx)) =
  Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (S (S (Z tx'))))))
 where
  tx' = Consensus.mkShelleyTx tx
toConsensusGenTx (TxInMode ShelleyBasedEraBabbage (ShelleyTx _ tx)) =
  Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (S (S (S (Z tx')))))))
 where
  tx' = Consensus.mkShelleyTx tx
toConsensusGenTx (TxInMode ShelleyBasedEraConway (ShelleyTx _ tx)) =
  Consensus.HardForkGenTx (Consensus.OneEraGenTx (S (S (S (S (S (S (Z tx'))))))))
 where
  tx' = Consensus.mkShelleyTx tx

-- ----------------------------------------------------------------------------
-- Transaction ids in the context of a consensus mode
--

-- | A 'TxId' in one of the eras supported by a given protocol mode.
--
-- For multi-era modes such as the 'CardanoMode' this type is a sum of the
-- different transaction types for all the eras. It is used in the
-- LocalTxMonitoring protocol.
--
-- TODO Rename to TxIdInEra
data TxIdInMode where
  TxIdInMode
    :: CardanoEra era
    -> TxId
    -> TxIdInMode

toConsensusTxId
  :: ()
  => Consensus.CardanoBlock StandardCrypto ~ block
  => TxIdInMode
  -> Consensus.TxId (Consensus.GenTx block)
toConsensusTxId (TxIdInMode ByronEra txid) =
  Consensus.HardForkGenTxId . Consensus.OneEraGenTxId . Z $ Consensus.WrapGenTxId txid'
 where
  txid' :: Consensus.TxId (Consensus.GenTx Consensus.ByronBlock)
  txid' = Consensus.ByronTxId $ toByronTxId txid
toConsensusTxId (TxIdInMode ShelleyEra txid) =
  Consensus.HardForkGenTxId (Consensus.OneEraGenTxId (S (Z (Consensus.WrapGenTxId txid'))))
 where
  txid' :: Consensus.TxId (Consensus.GenTx Consensus.StandardShelleyBlock)
  txid' = Consensus.ShelleyTxId $ toShelleyTxId txid
toConsensusTxId (TxIdInMode AllegraEra txid) =
  Consensus.HardForkGenTxId (Consensus.OneEraGenTxId (S (S (Z (Consensus.WrapGenTxId txid')))))
 where
  txid' :: Consensus.TxId (Consensus.GenTx Consensus.StandardAllegraBlock)
  txid' = Consensus.ShelleyTxId $ toShelleyTxId txid
toConsensusTxId (TxIdInMode MaryEra txid) =
  Consensus.HardForkGenTxId (Consensus.OneEraGenTxId (S (S (S (Z (Consensus.WrapGenTxId txid'))))))
 where
  txid' :: Consensus.TxId (Consensus.GenTx Consensus.StandardMaryBlock)
  txid' = Consensus.ShelleyTxId $ toShelleyTxId txid
toConsensusTxId (TxIdInMode AlonzoEra txid) =
  Consensus.HardForkGenTxId
    (Consensus.OneEraGenTxId (S (S (S (S (Z (Consensus.WrapGenTxId txid')))))))
 where
  txid' :: Consensus.TxId (Consensus.GenTx Consensus.StandardAlonzoBlock)
  txid' = Consensus.ShelleyTxId $ toShelleyTxId txid
toConsensusTxId (TxIdInMode BabbageEra txid) =
  Consensus.HardForkGenTxId
    (Consensus.OneEraGenTxId (S (S (S (S (S (Z (Consensus.WrapGenTxId txid'))))))))
 where
  txid' :: Consensus.TxId (Consensus.GenTx Consensus.StandardBabbageBlock)
  txid' = Consensus.ShelleyTxId $ toShelleyTxId txid
toConsensusTxId (TxIdInMode ConwayEra txid) =
  Consensus.HardForkGenTxId
    (Consensus.OneEraGenTxId (S (S (S (S (S (S (Z (Consensus.WrapGenTxId txid')))))))))
 where
  txid' :: Consensus.TxId (Consensus.GenTx Consensus.StandardConwayBlock)
  txid' = Consensus.ShelleyTxId $ toShelleyTxId txid

-- ----------------------------------------------------------------------------
-- Transaction validation errors in the context of eras and consensus modes
--

-- | The transaction validations errors that can occur from trying to submit a
-- transaction to a local node. The errors are specific to an era.
data TxValidationError era where
  ByronTxValidationError
    :: Consensus.ApplyTxErr Consensus.ByronBlock
    -> TxValidationError era
  ShelleyTxValidationError
    :: ShelleyBasedEra era
    -> Consensus.ApplyTxErr (Consensus.ShelleyBlock (ConsensusProtocol era) (ShelleyLedgerEra era))
    -> TxValidationError era

deriving instance Generic (TxValidationError era)

instance Show (TxValidationError era) where
  showsPrec p = \case
    ByronTxValidationError err ->
      showParen
        (p >= 11)
        ( showString "ByronTxValidationError "
            . showsPrec 11 err
        )
    ShelleyTxValidationError sbe err ->
      shelleyBasedEraConstraints sbe $
        showParen
          (p >= 11)
          ( showString "ShelleyTxValidationError "
              . showString (show sbe)
              . showString " "
              . showsPrec 11 err
          )

instance ToJSON (TxValidationError era) where
  toJSON = \case
    ByronTxValidationError err ->
      Aeson.object
        [ "kind" .= Aeson.String "ByronTxValidationError"
        , "error" .= toJSON err
        ]
    ShelleyTxValidationError sbe err ->
      shelleyBasedEraConstraints sbe $
        Aeson.object
          [ "kind" .= Aeson.String "ShelleyTxValidationError"
          , "era" .= toJSON (Text.pack (show sbe))
          , "error" .= appTxErrToJson sbe err
          ]

appTxErrToJson
  :: ()
  => ShelleyBasedEra era
  -> Consensus.ApplyTxErr (Consensus.ShelleyBlock (ConsensusProtocol era) (ShelleyLedgerEra era))
  -> Aeson.Value
appTxErrToJson w e = shelleyBasedEraConstraints w $ toJSON e

-- | A 'TxValidationError' in one of the eras supported by a given protocol
-- mode.
--
-- This is used in the LocalStateQuery protocol.
data TxValidationErrorInCardanoMode where
  TxValidationErrorInCardanoMode
    :: ()
    => TxValidationError era
    -> TxValidationErrorInCardanoMode
  TxValidationEraMismatch
    :: ()
    => EraMismatch
    -> TxValidationErrorInCardanoMode

deriving instance Show TxValidationErrorInCardanoMode

instance ToJSON TxValidationErrorInCardanoMode where
  toJSON = \case
    TxValidationErrorInCardanoMode err ->
      Aeson.object
        [ "tag" .= Aeson.String "TxValidationErrorInCardanoMode"
        , "contents" .= toJSON err
        ]
    TxValidationEraMismatch err ->
      Aeson.object
        [ "tag" .= Aeson.String "TxValidationEraMismatch"
        , "contents" .= toJSON (textShow err)
        ]

fromConsensusApplyTxErr
  :: ()
  => Consensus.CardanoBlock StandardCrypto ~ block
  => Consensus.ApplyTxErr block
  -> TxValidationErrorInCardanoMode
fromConsensusApplyTxErr = \case
  Consensus.ApplyTxErrByron err ->
    TxValidationErrorInCardanoMode $ ByronTxValidationError err
  Consensus.ApplyTxErrShelley err ->
    TxValidationErrorInCardanoMode $ ShelleyTxValidationError ShelleyBasedEraShelley err
  Consensus.ApplyTxErrAllegra err ->
    TxValidationErrorInCardanoMode $ ShelleyTxValidationError ShelleyBasedEraAllegra err
  Consensus.ApplyTxErrMary err ->
    TxValidationErrorInCardanoMode $ ShelleyTxValidationError ShelleyBasedEraMary err
  Consensus.ApplyTxErrAlonzo err ->
    TxValidationErrorInCardanoMode $ ShelleyTxValidationError ShelleyBasedEraAlonzo err
  Consensus.ApplyTxErrBabbage err ->
    TxValidationErrorInCardanoMode $ ShelleyTxValidationError ShelleyBasedEraBabbage err
  Consensus.ApplyTxErrConway err ->
    TxValidationErrorInCardanoMode $ ShelleyTxValidationError ShelleyBasedEraConway err
  Consensus.ApplyTxErrWrongEra err ->
    TxValidationEraMismatch err
