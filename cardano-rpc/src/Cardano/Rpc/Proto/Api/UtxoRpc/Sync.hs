{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Rpc.Proto.Api.UtxoRpc.Sync
  ( module Proto.Utxorpc.V1beta.Sync.Sync
  , module Proto.Utxorpc.V1beta.Sync.Sync_Fields
  , module Proto.Utxorpc.V1beta.Cardano.Cardano
  , module Proto.Utxorpc.V1beta.Cardano.Cardano_Fields
  )
where

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Proto.Utxorpc.V1beta.Cardano.Cardano
import Proto.Utxorpc.V1beta.Cardano.Cardano_Fields hiding
  ( hash
  , height
  , slot
  , timestamp
  )
import Proto.Utxorpc.V1beta.Sync.Sync
import Proto.Utxorpc.V1beta.Sync.Sync_Fields

type instance RequestMetadata (Protobuf SyncService meth) = NoMetadata

type instance ResponseInitialMetadata (Protobuf SyncService meth) = NoMetadata

type instance ResponseTrailingMetadata (Protobuf SyncService meth) = NoMetadata
