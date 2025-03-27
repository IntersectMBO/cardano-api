{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Cardano.Api.Internal.IPC.Version
  ( isQuerySupportedInNtcVersion

    -- *** Error types
  , UnsupportedNtcError (..)
  )
where

import Cardano.Protocol.Crypto
import Ouroboros.Consensus.Cardano.Block qualified as Consensus
import Ouroboros.Consensus.Cardano.Node ()
import Ouroboros.Consensus.HardFork.Combinator.Ledger.Query qualified as Consensus
import Ouroboros.Consensus.HardFork.Combinator.Serialisation.Common qualified as Consensus
import Ouroboros.Consensus.Node.NetworkProtocolVersion qualified as Consensus
import Ouroboros.Consensus.Ledger.Query qualified as Consensus
import Ouroboros.Consensus.Shelley.Ledger qualified as Consensus
import Ouroboros.Consensus.Shelley.Ledger.SupportsProtocol ()
import Ouroboros.Network.NodeToClient.Version (NodeToClientVersion (..))
import Ouroboros.Network.Protocol.LocalStateQuery.Codec

import Data.Map.Strict qualified as Map
import Data.Proxy
import Data.SOP.Constraint
import Data.SOP.Strict

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
-- query before sending it.  This affords the ability to return @'UnsupportedNtcError'('UnsupportedNtcVersionError')@,
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
  -> Maybe UnsupportedNtcError
isQuerySupportedInNtcVersion (Some q) ntc = case hfNTC of
  Just
    ( Consensus.HardForkNodeToClientEnabled
        _hfSpecificNTC
        (_ :* npNTC)
      ) ->
      either
        Just
        ( \x ->
            if not x
              then Just (UnsupportedNtcVersionError ntc)
              else Nothing
        )
        $ case q of
          Consensus.GetChainBlockNo -> Right True
          Consensus.GetSystemStart -> Right True
          Consensus.GetChainPoint -> Right True
          Consensus.GetLedgerConfig -> Right True
          Consensus.BlockQuery q' -> case q' of
            Consensus.QueryAnytime _ _ -> Right True
            Consensus.QueryHardFork _ -> Right True
            Consensus.QueryIfCurrent (Consensus.QZ _byronQuery) -> Right True
            Consensus.QueryIfCurrent (Consensus.QS q'') -> getNTC npNTC q''
  Just (Consensus.HardForkNodeToClientDisabled _) -> Just HardForkDisabled
  Nothing -> Just UnknownNtcVersion
 where
  hfNTC =
    Consensus.supportedNodeToClientVersions
      (Proxy @(Consensus.CardanoBlock StandardCrypto))
      Map.!? ntc

  getNTC
    :: forall result
     . NP Consensus.EraNodeToClientVersion (Consensus.CardanoShelleyEras StandardCrypto)
    -> Consensus.QueryIfCurrent (Consensus.CardanoShelleyEras StandardCrypto) result
    -> Either UnsupportedNtcError Bool
  getNTC = go
   where
    go
      :: forall xs
       . All Consensus.IsShelleyBlock xs
      => NP Consensus.EraNodeToClientVersion xs
      -> Consensus.QueryIfCurrent xs result
      -> Either UnsupportedNtcError Bool
    go (Consensus.EraNodeToClientEnabled supportedNtc :* _) (Consensus.QZ q') =
      Right $ Consensus.querySupportedVersion q' supportedNtc
    go (Consensus.EraNodeToClientDisabled :* _) Consensus.QZ{} =
      Left undefined
    go Nil qq = case qq of {}
    go (_ :* n) (Consensus.QS q') = go n q'

data UnsupportedNtcError
  = UnsupportedNtcDisabledEra
  | UnknownNtcVersion
  | HardForkDisabled
  | UnsupportedNtcVersionError !NodeToClientVersion
  deriving (Eq, Show)
