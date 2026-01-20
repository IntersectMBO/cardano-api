{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Rpc.Proto.Api.UtxoRpc.Query
  ( module Proto.Utxorpc.V1beta.Query.Query
  , module Proto.Utxorpc.V1beta.Query.Query_Fields
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
  , index
  , items
  , key
  , slot
  , timestamp
  , tx
  , values
  , vec'items
  , vec'values
  )
import Proto.Utxorpc.V1beta.Query.Query
import Proto.Utxorpc.V1beta.Query.Query_Fields

type instance RequestMetadata (Protobuf QueryService meth) = NoMetadata

type instance ResponseInitialMetadata (Protobuf QueryService meth) = NoMetadata

type instance ResponseTrailingMetadata (Protobuf QueryService meth) = NoMetadata
