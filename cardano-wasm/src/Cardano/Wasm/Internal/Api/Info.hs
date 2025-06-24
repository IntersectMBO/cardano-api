{-# LANGUAGE InstanceSigs #-}

module Cardano.Wasm.Internal.Api.Info (apiInfo) where

import Data.Aeson qualified as Aeson
import Data.Text qualified as Text

-- * API Information Data Types

-- | Describes the return type of a method.
data MethodReturnTypeInfo
  = -- | Returns an instance of the same object type (fluent interface).
    Fluent
  | -- | Returns a new instance of a specified virtual object type.
    NewObject String
  | -- | Returns a non-virtual-object type (e.g., JSString, number).
    OtherType String
  deriving (Show, Eq)

instance Aeson.ToJSON MethodReturnTypeInfo where
  toJSON Fluent = Aeson.object ["type" Aeson..= Text.pack "fluent"]
  toJSON (NewObject objTypeName) = Aeson.object ["type" Aeson..= Text.pack "newObject", "objectType" Aeson..= objTypeName]
  toJSON (OtherType typeName) = Aeson.object ["type" Aeson..= Text.pack "other", "typeName" Aeson..= typeName]

-- | Information about a single method of a virtual object.
data MethodInfo = MethodInfo
  { methodName :: String
  , methodParams :: [String]
  -- ^ Names of parameters, excluding 'this'.
  , methodReturnType :: MethodReturnTypeInfo
  }
  deriving (Show, Eq)

instance Aeson.ToJSON MethodInfo where
  toJSON :: MethodInfo -> Aeson.Value
  toJSON (MethodInfo name params retType) =
    Aeson.object
      [ "name" Aeson..= name
      , "params" Aeson..= params
      , "return" Aeson..= retType
      ]

-- | Information about a virtual object and its methods.
data VirtualObjectInfo = VirtualObjectInfo
  { virtualObjectName :: String
  , virtualObjectMethods :: [MethodInfo]
  }
  deriving (Show, Eq)

instance Aeson.ToJSON VirtualObjectInfo where
  toJSON :: VirtualObjectInfo -> Aeson.Value
  toJSON (VirtualObjectInfo name methods) =
    Aeson.object
      [ "objectName" Aeson..= name
      , "methods" Aeson..= methods
      ]

-- | Aggregate type for all API information.
data ApiInfo = ApiInfo
  { staticMethods :: [MethodInfo]
  , virtualObjects :: [VirtualObjectInfo]
  }
  deriving (Show, Eq)

instance Aeson.ToJSON ApiInfo where
  toJSON :: ApiInfo -> Aeson.Value
  toJSON (ApiInfo staticObjs virtualObjs) =
    Aeson.object
      [ "staticMethods" Aeson..= staticObjs
      , "virtualObjects" Aeson..= virtualObjs
      ]

-- | Provides metadata about the "virtual objects" and their methods.
-- This is intended to help generate JavaScript wrappers.
apiInfo :: ApiInfo
apiInfo =
  let unsignedTxObjectName = "UnsignedTx"
      signedTxObjectName = "SignedTx"

      staticApiMethods =
        [ MethodInfo
            { methodName = "newConwayTx"
            , methodParams = []
            , methodReturnType = NewObject unsignedTxObjectName
            }
        ]

      unsignedTxObj =
        VirtualObjectInfo
          { virtualObjectName = unsignedTxObjectName
          , virtualObjectMethods =
              [ MethodInfo
                  { methodName = "addTxInput"
                  , methodParams = ["txId", "txIx"]
                  , methodReturnType = Fluent
                  }
              , MethodInfo
                  { methodName = "addSimpleTxOut"
                  , methodParams = ["destAddr", "lovelaceAmount"]
                  , methodReturnType = Fluent
                  }
              , MethodInfo
                  { methodName = "setFee"
                  , methodParams = ["lovelaceAmount"]
                  , methodReturnType = Fluent
                  }
              , MethodInfo
                  { methodName = "signWithPaymentKey"
                  , methodParams = ["signingKey"]
                  , methodReturnType = NewObject signedTxObjectName
                  }
              , MethodInfo
                  { methodName = "estimateMinFee"
                  , methodParams =
                      ["protocolParams", "numExtraKeyWitnesses", "numExtraByronKeyWitnesses", "totalRefScriptSize"]
                  , methodReturnType = OtherType "BigInt"
                  }
              ]
          }

      signedTxObj =
        VirtualObjectInfo
          { virtualObjectName = signedTxObjectName
          , virtualObjectMethods =
              [ MethodInfo
                  { methodName = "alsoSignWithPaymentKey"
                  , methodParams = ["signingKey"]
                  , methodReturnType = Fluent
                  }
              , MethodInfo
                  { methodName = "txToCbor"
                  , methodParams = []
                  , methodReturnType = OtherType "string"
                  }
              ]
          }
   in ApiInfo
        { staticMethods = staticApiMethods
        , virtualObjects = [unsignedTxObj, signedTxObj]
        }
