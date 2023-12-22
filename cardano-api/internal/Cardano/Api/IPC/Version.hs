module Cardano.Api.IPC.Version
  ( NodeToClientVersionOf (..)
  , MinNodeToClientVersion

  -- *** Error types
  , UnsupportedNtcVersionError(..)
  ) where

import           Ouroboros.Network.NodeToClient.Version (NodeToClientVersion (..))

-- | The query 'a' is a versioned query, which means it requires the Node to support a minimum
-- Node-to-Client version.
--
-- Background: The node to client protocol is such that it will disconnect on any
-- unrecognised queries.  This means that for a Node-to-Client connection, if a query is sent
-- that was introduced in a Node-to-Client version that is newer than the Node-To-Client version
-- of the connection, the node will disconnect the client.  The client will not get any
-- information about why the disconnect happened.  This is a bad user experience for tools
-- such as the CLI.
--
-- To improve the user experience the API needs to prevent the sending of queries that are
-- newer than the Node-To-Client version of the connection by checking the version of the
-- query before sending it.  This affords the ability to return 'UnsupportedNtcVersionError',
-- informing the caller of a Node-To-Client versioning issue.
--
-- For maintaining typeclass instances, see the 'NodeToClientVersion' type documentation for
-- a list of versions and the queries that were introduced for those versions.
class NodeToClientVersionOf a where
  nodeToClientVersionOf :: a -> NodeToClientVersion

type MinNodeToClientVersion = NodeToClientVersion

-- | Show-ed query
type QueryStr = String

data UnsupportedNtcVersionError
  = UnsupportedNtcVersionError !MinNodeToClientVersion !NodeToClientVersion
  | UnsupportedBlockQuery !QueryStr
  deriving (Eq, Show)
