{-# LANGUAGE InstanceSigs #-}

module Cardano.Wasm.Internal.Api.Info
  ( apiInfo
  , ApiInfo (..)
  , VirtualObjectInfo (..)
  , MethodInfo (..)
  , ParamInfo (..)
  , MethodReturnTypeInfo (..)
  )
where

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

-- | Information about a single parameter of a method.
data ParamInfo = ParamInfo
  { paramName :: String
  , paramType :: String
  , paramDoc :: String
  }
  deriving (Show, Eq)

instance Aeson.ToJSON ParamInfo where
  toJSON :: ParamInfo -> Aeson.Value
  toJSON (ParamInfo name pType doc) =
    Aeson.object
      [ "name" Aeson..= name
      , "type" Aeson..= pType
      , "doc" Aeson..= doc
      ]

-- | Information about a single method of a virtual object.
data MethodInfo = MethodInfo
  { methodName :: String
  , methodDoc :: String
  , methodParams :: [ParamInfo]
  -- ^ Names of parameters, excluding 'this'.
  , methodReturnType :: MethodReturnTypeInfo
  , methodReturnDoc :: String
  }
  deriving (Show, Eq)

instance Aeson.ToJSON MethodInfo where
  toJSON :: MethodInfo -> Aeson.Value
  toJSON (MethodInfo name doc params retType retDoc) =
    Aeson.object
      [ "name" Aeson..= name
      , "doc" Aeson..= doc
      , "params" Aeson..= params
      , "return" Aeson..= retType
      , "returnDoc" Aeson..= retDoc
      ]

-- | Information about a virtual object and its methods.
data VirtualObjectInfo = VirtualObjectInfo
  { virtualObjectName :: String
  , virtualObjectDoc :: String
  , virtualObjectMethods :: [MethodInfo]
  }
  deriving (Show, Eq)

instance Aeson.ToJSON VirtualObjectInfo where
  toJSON :: VirtualObjectInfo -> Aeson.Value
  toJSON (VirtualObjectInfo name doc methods) =
    Aeson.object
      [ "objectName" Aeson..= name
      , "doc" Aeson..= doc
      , "methods" Aeson..= methods
      ]

-- | Aggregate type for all API information.
data ApiInfo = ApiInfo
  { mainObject :: VirtualObjectInfo
  , virtualObjects :: [VirtualObjectInfo]
  , initializeFunctionDoc :: String
  , initializeFunctionReturnDoc :: String
  }
  deriving (Show, Eq)

instance Aeson.ToJSON ApiInfo where
  toJSON :: ApiInfo -> Aeson.Value
  toJSON (ApiInfo mainObj virtualObjs initDoc initRetDoc) =
    Aeson.object
      [ "mainObject" Aeson..= mainObj
      , "virtualObjects" Aeson..= virtualObjs
      , "initializeFunctionDoc" Aeson..= initDoc
      , "initializeFunctionReturnDoc" Aeson..= initRetDoc
      ]

-- | Provides metadata about the "virtual objects" and their methods.
-- This is intended to help generate JavaScript wrappers.
apiInfo :: ApiInfo
apiInfo =
  let unsignedTxObjectName = "UnsignedTx"
      signedTxObjectName = "SignedTx"

      unsignedTxObj =
        VirtualObjectInfo
          { virtualObjectName = unsignedTxObjectName
          , virtualObjectDoc = "Represents an unsigned transaction."
          , virtualObjectMethods =
              [ MethodInfo
                  { methodName = "addTxInput"
                  , methodDoc = "Adds a simple transaction input to the transaction."
                  , methodParams =
                      [ ParamInfo "txId" "string" "The transaction ID of the input UTxO."
                      , ParamInfo "txIx" "number" "The index of the input within the UTxO."
                      ]
                  , methodReturnType = Fluent
                  , methodReturnDoc = "The `UnsignedTx` object with the added input."
                  }
              , MethodInfo
                  { methodName = "addSimpleTxOut"
                  , methodDoc = "Adds a simple transaction output to the transaction."
                  , methodParams =
                      [ ParamInfo "destAddr" "string" "The destination address."
                      , ParamInfo "lovelaceAmount" "bigint" "The amount in lovelace to output."
                      ]
                  , methodReturnType = Fluent
                  , methodReturnDoc = "The `UnsignedTx` object with the added output."
                  }
              , MethodInfo
                  { methodName = "setFee"
                  , methodDoc = "Sets the fee for the transaction."
                  , methodParams = [ParamInfo "lovelaceAmount" "bigint" "The fee amount in lovelace."]
                  , methodReturnType = Fluent
                  , methodReturnDoc = "The `UnsignedTx` object with the set fee."
                  }
              , MethodInfo
                  { methodName = "addSigningKey"
                  , methodDoc = "Adds payment key witness to the transaction."
                  , methodParams = [ParamInfo "signingKey" "string" "The signing key of the witness."]
                  , methodReturnType = Fluent
                  , methodReturnDoc = "The `UnsignedTx` object with the added witness."
                  }
              , MethodInfo
                  { methodName = "signTx"
                  , methodDoc = "Signs the transaction."
                  , methodParams = []
                  , methodReturnType = NewObject signedTxObjectName
                  , methodReturnDoc = "A promise that resolves to a `SignedTx` object."
                  }
              , MethodInfo
                  { methodName = "estimateMinFee"
                  , methodDoc = "Estimates the minimum fee for the transaction."
                  , methodParams =
                      [ ParamInfo "protocolParams" "any" "The protocol parameters."
                      , ParamInfo
                          "numExtraKeyWitnesses"
                          "number"
                          "The number of extra key witnesses (in addition to the ones already added)."
                      , ParamInfo "numExtraByronKeyWitnesses" "number" "The number of extra Byron key witnesses."
                      , ParamInfo "totalRefScriptSize" "number" "The total size of reference scripts in bytes."
                      ]
                  , methodReturnType = OtherType "bigint"
                  , methodReturnDoc = "A promise that resolves to the estimated minimum fee in lovelace."
                  }
              ]
          }

      signedTxObj =
        VirtualObjectInfo
          { virtualObjectName = signedTxObjectName
          , virtualObjectDoc = "Represents a signed transaction."
          , virtualObjectMethods =
              [ MethodInfo
                  { methodName = "txToCbor"
                  , methodDoc = "Converts the signed transaction to its CBOR representation."
                  , methodParams = []
                  , methodReturnType = OtherType "string"
                  , methodReturnDoc =
                      "A promise that resolves to the CBOR representation of the transaction as a hex string."
                  }
              ]
          }
   in ApiInfo
        { mainObject =
            VirtualObjectInfo
              { virtualObjectName = "CardanoAPI"
              , virtualObjectDoc = "The main Cardano API object with static methods."
              , virtualObjectMethods =
                  [ MethodInfo
                      { methodName = "newConwayTx"
                      , methodDoc = "Creates a new Conway-era transaction."
                      , methodParams = []
                      , methodReturnType = NewObject unsignedTxObjectName
                      , methodReturnDoc = "A promise that resolves to a new `UnsignedTx` object."
                      }
                  ]
              }
        , virtualObjects = [unsignedTxObj, signedTxObj]
        , initializeFunctionDoc = "Initializes the Cardano API."
        , initializeFunctionReturnDoc = "A promise that resolves to the main `CardanoAPI` object."
        }
