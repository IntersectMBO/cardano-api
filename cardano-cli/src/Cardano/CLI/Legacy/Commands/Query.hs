{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Cardano.CLI.Legacy.Commands.Query
  ( LegacyQueryCmds (..)
  , LegacyQueryLeadershipScheduleCmdArgs (..)
  , LegacyQueryProtocolParametersCmdArgs (..)
  , LegacyQueryConstitutionHashCmdArgs (..)
  , LegacyQueryTipCmdArgs (..)
  , LegacyQueryStakePoolsCmdArgs (..)
  , LegacyQueryStakeDistributionCmdArgs (..)
  , LegacyQueryStakeAddressInfoCmdArgs (..)
  , LegacyQueryUTxOCmdArgs (..)
  , LegacyQueryLedgerStateCmdArgs (..)
  , LegacyQueryProtocolStateCmdArgs (..)
  , LegacyQueryStakeSnapshotCmdArgs (..)
  , LegacyQueryKesPeriodInfoCmdArgs (..)
  , LegacyQueryPoolStateCmdArgs (..)
  , LegacyQueryTxMempoolCmdArgs (..)
  , LegacyQuerySlotNumberCmdArgs (..)
  , renderLegacyQueryCmds
  ) where

import           Cardano.Api.Shelley hiding (QueryInShelleyBasedEra (..))

import           Cardano.CLI.Types.Common
import           Cardano.CLI.Types.Key

import           Data.Text (Text)
import           Data.Time.Clock
import           GHC.Generics

data LegacyQueryCmds
  = QueryLeadershipScheduleCmd  !LegacyQueryLeadershipScheduleCmdArgs
  | QueryProtocolParametersCmd  !LegacyQueryProtocolParametersCmdArgs
  | QueryConstitutionHashCmd    !LegacyQueryConstitutionHashCmdArgs
  | QueryTipCmd                 !LegacyQueryTipCmdArgs
  | QueryStakePoolsCmd          !LegacyQueryStakePoolsCmdArgs
  | QueryStakeDistributionCmd   !LegacyQueryStakeDistributionCmdArgs
  | QueryStakeAddressInfoCmd    !LegacyQueryStakeAddressInfoCmdArgs
  | QueryUTxOCmd                !LegacyQueryUTxOCmdArgs
  | QueryLedgerStateCmd         !LegacyQueryLedgerStateCmdArgs
  | QueryProtocolStateCmd       !LegacyQueryProtocolStateCmdArgs
  | QueryStakeSnapshotCmd       !LegacyQueryStakeSnapshotCmdArgs
  | QueryKesPeriodInfoCmd       !LegacyQueryKesPeriodInfoCmdArgs
  | QueryPoolStateCmd           !LegacyQueryPoolStateCmdArgs
  | QueryTxMempoolCmd           !LegacyQueryTxMempoolCmdArgs
  | QuerySlotNumberCmd          !LegacyQuerySlotNumberCmdArgs
  deriving (Generic, Show)

data LegacyQueryLeadershipScheduleCmdArgs = LegacyQueryLeadershipScheduleCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , genesisFp           :: !GenesisFile
  , poolColdVerKeyFile  :: !(VerificationKeyOrHashOrFile StakePoolKey)
  , vrkSkeyFp           :: !(SigningKeyFile In)
  , whichSchedule       :: !EpochLeadershipSchedule
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data LegacyQueryProtocolParametersCmdArgs = LegacyQueryProtocolParametersCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data LegacyQueryConstitutionHashCmdArgs = LegacyQueryConstitutionHashCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data LegacyQueryTipCmdArgs = LegacyQueryTipCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data LegacyQueryStakePoolsCmdArgs = LegacyQueryStakePoolsCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data LegacyQueryStakeDistributionCmdArgs = LegacyQueryStakeDistributionCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data LegacyQueryStakeAddressInfoCmdArgs = LegacyQueryStakeAddressInfoCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , addr                :: !StakeAddress
  , networkId           :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data LegacyQueryUTxOCmdArgs = LegacyQueryUTxOCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , queryFilter         :: !QueryUTxOFilter
  , networkId           :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data LegacyQueryLedgerStateCmdArgs = LegacyQueryLedgerStateCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data LegacyQueryProtocolStateCmdArgs = LegacyQueryProtocolStateCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data LegacyQueryStakeSnapshotCmdArgs = LegacyQueryStakeSnapshotCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , allOrOnlyPoolIds    :: !(AllOrOnly [Hash StakePoolKey])
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data LegacyQueryKesPeriodInfoCmdArgs = LegacyQueryKesPeriodInfoCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , nodeOpCertFp        :: !(File () In) -- ^ Node operational certificate
  , mOutFile            :: !(Maybe (File () Out))
  } deriving (Generic, Show)

data LegacyQueryPoolStateCmdArgs = LegacyQueryPoolStateCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , poolIds             :: ![Hash StakePoolKey]
  } deriving (Generic, Show)

data LegacyQueryTxMempoolCmdArgs = LegacyQueryTxMempoolCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , query               :: !TxMempoolQuery
  , mOutFile            :: !(Maybe (File () Out))
  }
  deriving (Generic, Show)

data LegacyQuerySlotNumberCmdArgs = LegacyQuerySlotNumberCmdArgs
  { nodeSocketPath      :: !SocketPath
  , consensusModeParams :: !AnyConsensusModeParams
  , networkId           :: !NetworkId
  , utcTime             :: !UTCTime
  } deriving (Generic, Show)

renderLegacyQueryCmds :: LegacyQueryCmds -> Text
renderLegacyQueryCmds = \case
  QueryLeadershipScheduleCmd {} -> "query leadership-schedule"
  QueryProtocolParametersCmd {} -> "query protocol-parameters "
  QueryConstitutionHashCmd {} -> "query constitution-hash "
  QueryTipCmd {} -> "query tip"
  QueryStakePoolsCmd {} -> "query stake-pools"
  QueryStakeDistributionCmd {} -> "query stake-distribution"
  QueryStakeAddressInfoCmd {} -> "query stake-address-info"
  QueryUTxOCmd {} -> "query utxo"
  QueryLedgerStateCmd {} -> "query ledger-state"
  QueryProtocolStateCmd {} -> "query protocol-state"
  QueryStakeSnapshotCmd {} -> "query stake-snapshot"
  QueryKesPeriodInfoCmd {} -> "query kes-period-info"
  QueryPoolStateCmd {} -> "query pool-state"
  QueryTxMempoolCmd (LegacyQueryTxMempoolCmdArgs _ _ _ txMempoolQuery _) -> "query tx-mempool" <> renderTxMempoolQuery txMempoolQuery
  QuerySlotNumberCmd {} -> "query slot-number"
  where
    renderTxMempoolQuery = \case
      TxMempoolQueryTxExists tx -> "tx-exists " <> serialiseToRawBytesHexText tx
      TxMempoolQueryNextTx -> "next-tx"
      TxMempoolQueryInfo -> "info"