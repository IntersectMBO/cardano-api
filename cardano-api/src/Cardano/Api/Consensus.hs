module Cardano.Api.Consensus
  ( -- * Consensus modes

    -- | Consensus modes. The node supports several different modes with different
    -- combinations of consensus protocols and ledger eras.

    -- ** The protocols supported in each era
    ConsensusProtocol
  , ChainDepStateProtocol

    -- ** Connection parameters for each mode
  , ConsensusModeParams (..)
  , EpochSlots (..)

    -- ** Conversions to and from types in the consensus library
  , ConsensusCryptoForBlock
  , ConsensusBlockForEra
  , toConsensusEraIndex
  , fromConsensusEraIndex

    -- * Transactions in the consensus mode

    -- | Transactions in the context of a consensus mode, and other types used in
    -- the transaction submission protocol.

    -- ** Transaction in a consensus mode
  , TxInMode (..)
  , fromConsensusGenTx
  , toConsensusGenTx

    -- ** Transaction id in a consensus mode
  , TxIdInMode (..)
  , toConsensusTxId

    -- ** Transaction validation errors
  , TxValidationError (..)
  , TxValidationErrorInCardanoMode (..)
  , fromConsensusApplyTxErr

    -- * Consensus protocol
  , BlockType (..)
  , SomeBlockType (..)
  , reflBlockType
  , Protocol (..)
  , ProtocolInfoArgs (..)
  , ProtocolClient (..)
  , ProtocolClientInfoArgs (..)

    -- * Reexports from @ouroboros-consensus@
  , ByronBlock
  , ChainDepState
  , GenTx (..)
  , EraMismatch (..)
  , PastHorizonException
  , PraosProtocolSupportsNode
  , PraosProtocolSupportsNodeCrypto
  , ShelleyGenesisStaking (..)
  , byronIdTx
  , condense
  , getOpCertCounters
  , interpreterToEpochInfo
  , unsafeExtendSafeZone
  , txId
  )
where

import Cardano.Api.Consensus.Internal.InMode
import Cardano.Api.Consensus.Internal.Mode
import Cardano.Api.Consensus.Internal.Protocol
import Cardano.Api.Consensus.Internal.Reexport
