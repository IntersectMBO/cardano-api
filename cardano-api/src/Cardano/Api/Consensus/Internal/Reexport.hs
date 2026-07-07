module Cardano.Api.Consensus.Internal.Reexport
  ( BlockComponent (..)
  , ByronBlock
  , CardanoBlock
  , ConfigSupportsNode
  , nodeSystemStart
  , ChainDepState
  , GenTx (..)
  , HasHeader
  , HeaderHash
  , EraMismatch (..)
  , NodeKernel (..)
  , OneEraHash (..)
  , HasHardForkHistory (..)
  , PastHorizonException
  , PraosProtocolSupportsNode
  , PraosProtocolSupportsNodeCrypto
  , RealPoint (..)
  , ShelleyGenesisStaking (..)
  , StandardCrypto
  , TopLevelConfig
  , ledgerState
  , blockNo
  , byronBlockRaw
  , byronIdTx
  , configBlock
  , configLedger
  , condense
  , getOpCertCounters
  , interpreterToEpochInfo
  , mkInterpreter
  , unsafeExtendSafeZone
  , txId
  )
where

import Cardano.Protocol.Crypto (StandardCrypto)
import Cardano.Slotting.Time (SystemStart)
import Ouroboros.Consensus.Block (HasHeader, HeaderHash, RealPoint (..), blockNo)
import Ouroboros.Consensus.Byron.Ledger (ByronBlock (byronBlockRaw), GenTx (..), byronIdTx)
import Ouroboros.Consensus.Cardano.Block (CardanoBlock, EraMismatch (..))
import Ouroboros.Consensus.Config (TopLevelConfig, configBlock, configLedger)
import Ouroboros.Consensus.Config.SupportsNode (ConfigSupportsNode (getSystemStart))
import Ouroboros.Consensus.HardFork.Abstract (HasHardForkHistory (..))
import Ouroboros.Consensus.HardFork.Combinator.AcrossEras (OneEraHash (..))
import Ouroboros.Consensus.HardFork.History.EpochInfo (interpreterToEpochInfo)
import Ouroboros.Consensus.HardFork.History.Qry
  ( PastHorizonException
  , mkInterpreter
  , unsafeExtendSafeZone
  )
import Ouroboros.Consensus.Ledger.Extended (ledgerState)
import Ouroboros.Consensus.Ledger.SupportsMempool (txId)
import Ouroboros.Consensus.Node (NodeKernel (..))
import Ouroboros.Consensus.Protocol.Abstract (ChainDepState)
import Ouroboros.Consensus.Protocol.Praos.Common
  ( PraosProtocolSupportsNode
  , PraosProtocolSupportsNodeCrypto
  , getOpCertCounters
  )
import Ouroboros.Consensus.Shelley.Node (ShelleyGenesisStaking (..))
import Ouroboros.Consensus.Storage.Common (BlockComponent (..))
import Ouroboros.Consensus.Util.Condense (condense)

-- | Extract the network system start time from the node's top-level configuration.
nodeSystemStart :: ConfigSupportsNode blk => TopLevelConfig blk -> SystemStart
nodeSystemStart = getSystemStart . configBlock
