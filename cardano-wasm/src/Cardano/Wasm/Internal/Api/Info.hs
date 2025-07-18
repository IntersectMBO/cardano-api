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
  toJSON :: MethodReturnTypeInfo -> Aeson.Value
  toJSON Fluent = Aeson.object ["type" Aeson..= Text.pack "fluent"]
  toJSON (NewObject objTypeName) = Aeson.object ["type" Aeson..= Text.pack "newObject", "objectType" Aeson..= objTypeName]
  toJSON (OtherType typeName) = Aeson.object ["type" Aeson..= Text.pack "other", "typeName" Aeson..= typeName]

-- | Information about a single parameter of a method.
data ParamInfo = ParamInfo
  { paramName :: String
  -- ^ Name of the parameter.
  , paramType :: String
  -- ^ Type of the parameter (as a TypeScript type).
  , paramDoc :: String
  -- ^ Documentation for the parameter.
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
  -- ^ Name of the method in the virtual object of the JS API (which should match the exported function).
  , methodDoc :: String
  -- ^ General documentation for the method.
  , methodParams :: [ParamInfo]
  -- ^ Info about parameters, excluding 'this'.
  , methodReturnType :: MethodReturnTypeInfo
  -- ^ Return type of the method.
  , methodReturnDoc :: String
  -- ^ Documentation for the return value of the method.
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
  -- ^ Name of the virtual object.
  , virtualObjectDoc :: String
  -- ^ Documentation for the virtual object.
  , virtualObjectMethods :: [MethodInfo]
  -- ^ Information about the methods of the virtual object.
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
  -- ^ Information about the main virtual object of the API, which serves as the entry point.
  , virtualObjects :: [VirtualObjectInfo]
  -- ^ Info about all other (non-main) virtual objects and their methods.
  , initialiseFunctionDoc :: String
  -- ^ Documentation for the initialise function of the API (which creates the main virtual object).
  , initialiseFunctionReturnDoc :: String
  -- ^ Documentation for the return value of the initialise function (which is the main virtual object).
  }
  deriving (Show, Eq)

instance Aeson.ToJSON ApiInfo where
  toJSON :: ApiInfo -> Aeson.Value
  toJSON (ApiInfo mainObj virtualObjs initDoc initRetDoc) =
    Aeson.object
      [ "mainObject" Aeson..= mainObj
      , "virtualObjects" Aeson..= virtualObjs
      , "initialiseFunctionDoc" Aeson..= initDoc
      , "initialiseFunctionReturnDoc" Aeson..= initRetDoc
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
                      , ParamInfo "lovelaceAmount" "bigint" "The amount in lovelaces to output."
                      ]
                  , methodReturnType = Fluent
                  , methodReturnDoc = "The `UnsignedTx` object with the added output."
                  }
              , MethodInfo
                  { methodName = "setFee"
                  , methodDoc = "Sets the fee for the transaction."
                  , methodParams = [ParamInfo "lovelaceAmount" "bigint" "The fee amount in lovelaces."]
                  , methodReturnType = Fluent
                  , methodReturnDoc = "The `UnsignedTx` object with the set fee."
                  }
              , MethodInfo
                  { methodName = "estimateMinFee"
                  , methodDoc = "Estimates the minimum fee for the transaction."
                  , methodParams =
                      [ ParamInfo "protocolParams" "any" "The protocol parameters."
                      , ParamInfo
                          "numKeyWitnesses"
                          "number"
                          "The number of key witnesses."
                      , ParamInfo "numByronKeyWitnesses" "number" "The number of Byron key witnesses."
                      , ParamInfo "totalRefScriptSize" "number" "The total size of reference scripts in bytes."
                      ]
                  , methodReturnType = OtherType "BigInt"
                  , methodReturnDoc = "A promise that resolves to the estimated minimum fee in lovelaces."
                  }
              , MethodInfo
                  { methodName = "signWithPaymentKey"
                  , methodDoc = "Signs the transaction with a payment key."
                  , methodParams = [ParamInfo "signingKey" "string" "The signing key to witness the transaction."]
                  , methodReturnType = NewObject signedTxObjectName
                  , methodReturnDoc = "A promise that resolves to a `SignedTx` object."
                  }
              ]
          }

      signedTxObj =
        VirtualObjectInfo
          { virtualObjectName = signedTxObjectName
          , virtualObjectDoc = "Represents a signed transaction."
          , virtualObjectMethods =
              [ MethodInfo
                  { methodName = "alsoSignWithPaymentKey"
                  , methodDoc = "Adds an extra signature to the transaction with a payment key."
                  , methodParams = [ParamInfo "signingKey" "string" "The signing key to witness the transaction."]
                  , methodReturnType = Fluent
                  , methodReturnDoc = "The `SignedTx` object with the additional signature."
                  }
              , MethodInfo
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
        , initialiseFunctionDoc = "Initialises the Cardano API."
        , initialiseFunctionReturnDoc = "A promise that resolves to the main `CardanoAPI` object."
        }
