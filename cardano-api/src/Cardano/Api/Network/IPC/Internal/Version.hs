module Cardano.Api.Network.IPC.Internal.Version
  ( isQuerySupportedInNtcVersion
  , NodeToClientVersion (..)

    -- *** Error types
  , UnsupportedNtcVersionError (..)
  )
where

import Cardano.Api.Error
import Cardano.Api.Pretty

import Cardano.Protocol.Crypto
import Ouroboros.Consensus.Cardano.Block qualified as Consensus
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.Ledger.Query qualified as Consensus
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.NodeToClient.Version (NodeToClientVersion (..))
import Ouroboros.Network.Protocol.LocalStateQuery.Codec

-- | LocalStateQuery uses versioned queries, which means it requires the Node to support a minimum
-- (and possibly maximum) Node-to-Client version.
--
-- Background: The node to client LocalStateQuery protocol is such that it will disconnect on any
-- unrecognised queries.  This means that for a Node-to-Client connection, if a query is sent
-- that was introduced in a Node-to-Client version that is newer than the Node-To-Client version
-- of the connection, the node will disconnect the client.  The client will not get any
-- information about why the disconnect happened.  This is a bad user experience for tools
-- such as the CLI.
--
-- To improve the user experience the API needs to prevent the sending of queries that are
-- newer than the Node-To-Client version of the connection by checking the version of the
-- query before sending it.  This affords the ability to return @'UnsupportedNtcVersionError'('UnsupportedNtcVersionError')@,
-- informing the caller of a Node-To-Client versioning issue.
--
-- For maintaining typeclass instances, see the 'NodeToClientVersion' type documentation for
-- a list of versions and the queries that were introduced for those versions.
--
-- More information about queries versioning can be found:
--   * https://ouroboros-network.cardano.intersectmbo.org/ouroboros-network/Ouroboros-Network-NodeToClient.html#t:NodeToClientVersion
--   * https://ouroboros-consensus.cardano.intersectmbo.org/docs/for-developers/QueryVersioning/#implementation
isQuerySupportedInNtcVersion
  :: Some (Consensus.Query (Consensus.CardanoBlock StandardCrypto))
  -> NodeToClientVersion
  -> Either UnsupportedNtcVersionError ()
isQuerySupportedInNtcVersion (Some q) ntc =
  if Consensus.queryIsSupportedOnNodeToClientVersion q ntc
    then Right ()
    else Left $ UnsupportedNtcVersionError ntc (Consensus.querySupportedVersions q)

data UnsupportedNtcVersionError
  = UnsupportedNtcVersionError
      !NodeToClientVersion
      -- ^ The version we negotiated
      ![NodeToClientVersion]
      -- ^ The versions in which this query is supported
  deriving (Eq, Show)

instance Error UnsupportedNtcVersionError where
  prettyError (UnsupportedNtcVersionError minNtcVersion ntcVersion) =
    "Unsupported feature for the node-to-client protocol version.\n"
      <> "This query requires at least "
      <> pshow minNtcVersion
      <> " but the node negotiated "
      <> pshow ntcVersion
      <> ".\n"
      <> "Later node versions support later protocol versions (but development protocol versions are not enabled in the node by default)."
