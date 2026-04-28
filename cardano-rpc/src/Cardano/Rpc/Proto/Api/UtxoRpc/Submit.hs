{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Rpc.Proto.Api.UtxoRpc.Submit
  ( module Proto.Utxorpc.V1beta.Submit.Submit
  , module Proto.Utxorpc.V1beta.Submit.Submit_Fields
  )
where

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Proto.Utxorpc.V1beta.Submit.Submit
import Proto.Utxorpc.V1beta.Submit.Submit_Fields hiding
  ( allOf
  , anyOf
  , fieldMask
  , items
  , match
  , maybe'chain
  , maybe'fieldMask
  , maybe'match
  , maybe'parsedState
  , maybe'predicate
  , nativeBytes
  , not
  , predicate
  , vec'allOf
  , vec'anyOf
  , vec'items
  , vec'not
  )

type instance RequestMetadata (Protobuf SubmitService meth) = NoMetadata

type instance ResponseInitialMetadata (Protobuf SubmitService meth) = NoMetadata

type instance ResponseTrailingMetadata (Protobuf SubmitService meth) = NoMetadata
