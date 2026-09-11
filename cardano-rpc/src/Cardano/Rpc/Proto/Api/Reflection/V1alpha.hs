{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Rpc.Proto.Api.Reflection.V1alpha
  ( module Proto.Grpc.Reflection.V1alpha.Reflection
  , module Proto.Grpc.Reflection.V1alpha.Reflection_Fields
  )
where

import Network.GRPC.Common
import Network.GRPC.Common.Protobuf

import Proto.Grpc.Reflection.V1alpha.Reflection
import Proto.Grpc.Reflection.V1alpha.Reflection_Fields

type instance RequestMetadata (Protobuf ServerReflection meth) = NoMetadata

type instance ResponseInitialMetadata (Protobuf ServerReflection meth) = NoMetadata

type instance ResponseTrailingMetadata (Protobuf ServerReflection meth) = NoMetadata
