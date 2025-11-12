{-# LANGUAGE InstanceSigs #-}

module Cardano.Wasm.Api.Info
  ( apiInfo
  , ApiInfo (..)
  , VirtualObjectInfo (..)
  , MethodHierarchy (..)
  , MethodGroup (..)
  , MethodInfo (..)
  , ParamInfo (..)
  , MethodReturnTypeInfo (..)
  , dashCaseName
  , tsTypeAsString
  )
where

import Cardano.Api (pretty)

import Cardano.Wasm.Api.Tx (UnsignedTxObject (..), newExperimentalEraTxImpl, newTxImpl)

import Data.Aeson qualified as Aeson
import Data.Text qualified as Text
import Text.Casing (fromHumps, toKebab)

-- * API Information Data Types

-- | TypeScript types that are not defined by this code.
data TSType
  = TSString
  | TSNumber
  | TSBigInt
  | TSAny
  | TSUtxoList
  | TSUtxosForAddressList
  deriving (Show, Eq)

tsTypeAsString :: TSType -> String
tsTypeAsString TSString = "string"
tsTypeAsString TSNumber = "number"
tsTypeAsString TSBigInt = "bigint"
tsTypeAsString TSAny = "any"
tsTypeAsString TSUtxoList =
  "{ address: string, txId: string, txIndex: number, lovelace: bigint, assets: any[], datum?: any, script?: any }[]"
tsTypeAsString TSUtxosForAddressList =
  "{ txId: string, txIndex: number, lovelace: bigint, assets: any[], datum?: any, script?: any }[]"

instance Aeson.ToJSON TSType where
  toJSON :: TSType -> Aeson.Value
  toJSON = Aeson.String . Text.pack . tsTypeAsString

-- | Describes the return type of a method.
data MethodReturnTypeInfo
  = -- | Returns an instance of the same object type (fluent interface).
    Fluent
  | -- | Returns a new instance of a specified virtual object type.
    NewObject String
  | -- | Returns a non-virtual-object type (e.g., JSString, number).
    OtherType TSType
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
  , paramType :: TSType
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

-- | Method hierarchy. Allows grouping methods in sub-groups.
data MethodHierarchy
  = MethodGroupEntry MethodGroup
  | MethodInfoEntry MethodInfo
  deriving (Show, Eq)

instance Aeson.ToJSON MethodHierarchy where
  toJSON :: MethodHierarchy -> Aeson.Value
  toJSON (MethodGroupEntry group) =
    Aeson.object
      [ "type" Aeson..= ("group" :: String)
      , "group" Aeson..= group
      ]
  toJSON (MethodInfoEntry info) =
    Aeson.object
      [ "type" Aeson..= ("method" :: String)
      , "method" Aeson..= info
      ]

-- | Method group. groups several methods under a common name.
data MethodGroup = MethodGroup
  { groupName :: String
  -- ^ Name of the method group.
  , groupDoc :: [String]
  -- ^ Documentation for the method group.
  , groupMethods :: [MethodHierarchy]
  -- ^ Methods or sub-groups belonging to this group.
  }
  deriving (Show, Eq)

instance Aeson.ToJSON MethodGroup where
  toJSON :: MethodGroup -> Aeson.Value
  toJSON (MethodGroup name doc methods) =
    Aeson.object
      [ "name" Aeson..= name
      , "doc" Aeson..= doc
      , "methods" Aeson..= methods
      ]

-- | Information about a single method of a virtual object.
data MethodInfo = MethodInfo
  { methodName :: String
  -- ^ Name of the global method name in the API (which should match the exported function and it must be unique globally).
  , methodSimpleName :: String
  -- ^ Name of the method in the virtual object of the API when accessed via JS (used for re-exporting to JS, may be shorter than methodName and does not need to be unique unlike methodName).
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
  toJSON (MethodInfo name simpleName doc params retType retDoc) =
    Aeson.object
      [ "name" Aeson..= name
      , "simpleName" Aeson..= simpleName
      , "doc" Aeson..= doc
      , "params" Aeson..= params
      , "return" Aeson..= retType
      , "returnDoc" Aeson..= retDoc
      ]

-- | Information about a virtual object and its methods.
data VirtualObjectInfo = VirtualObjectInfo
  { virtualObjectName :: String
  -- ^ Name of the virtual object in the JS API (which should match the exported class and be PascalCase).
  , virtualObjectDoc :: String
  -- ^ Documentation for the virtual object.
  , virtualObjectMethods :: [MethodHierarchy]
  -- ^ Information about the methods of the virtual object.
  }
  deriving (Show, Eq)

dashCaseName :: VirtualObjectInfo -> String
dashCaseName = toKebab . fromHumps . virtualObjectName

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

-- | Get a comment about the era for unsigned transaction creation methods.
getEraCommentForUnsignedTx :: Maybe UnsignedTxObject -> String
getEraCommentForUnsignedTx utxMonad =
  case utxMonad of
    Just (UnsignedTxObject era _) -> "(currently " ++ show (pretty era) ++ ")"
    Nothing -> "(currently unavailable)"

-- | Provides metadata about the "virtual objects" and their methods.
-- This is intended to help generate JavaScript wrappers.
apiInfo :: ApiInfo
apiInfo =
  let walletObj =
        VirtualObjectInfo
          { virtualObjectName = "Wallet"
          , virtualObjectDoc = "Represents a wallet."
          , virtualObjectMethods =
              [ MethodInfoEntry $
                  MethodInfo
                    { methodName = "getAddressBech32"
                    , methodSimpleName = "getAddressBech32"
                    , methodDoc = "Get the Bech32 representation of the address. (Can be shared for receiving funds.)"
                    , methodParams = []
                    , methodReturnType = OtherType TSString
                    , methodReturnDoc = "The Bech32 representation of the address."
                    }
              , MethodInfoEntry $
                  MethodInfo
                    { methodName = "getBech32ForPaymentVerificationKey"
                    , methodSimpleName = "getBech32ForPaymentVerificationKey"
                    , methodDoc =
                        "Get the Bech32 representation of the payment verification key of the wallet. (Can be shared for verification.)"
                    , methodParams = []
                    , methodReturnType = OtherType TSString
                    , methodReturnDoc = "The Bech32 representation of the payment verification key."
                    }
              , MethodInfoEntry $
                  MethodInfo
                    { methodName = "getBech32ForPaymentSigningKey"
                    , methodSimpleName = "getBech32ForPaymentSigningKey"
                    , methodDoc =
                        "Get the Bech32 representation of the payment signing key of the wallet. (Must be kept secret.)"
                    , methodParams = []
                    , methodReturnType = OtherType TSString
                    , methodReturnDoc = "The Bech32 representation of the payment signing key."
                    }
              , MethodInfoEntry $
                  MethodInfo
                    { methodName = "getBech32ForStakeVerificationKey"
                    , methodSimpleName = "getBech32ForStakeVerificationKey"
                    , methodDoc =
                        "Get the Bech32 representation of the stake verification key of the wallet. (Can be shared for verification.)"
                    , methodParams = []
                    , methodReturnType = OtherType TSString
                    , methodReturnDoc = "The Bech32 representation of the stake verification key."
                    }
              , MethodInfoEntry $
                  MethodInfo
                    { methodName = "getBech32ForStakeSigningKey"
                    , methodSimpleName = "getBech32ForStakeSigningKey"
                    , methodDoc =
                        "Get the Bech32 representation of the stake signing key of the wallet. (Must be kept secret.)"
                    , methodParams = []
                    , methodReturnType = OtherType TSString
                    , methodReturnDoc = "The Bech32 representation of the stake signing key."
                    }
              , MethodInfoEntry $
                  MethodInfo
                    { methodName = "getBase16ForPaymentVerificationKeyHash"
                    , methodSimpleName = "getBase16ForPaymentVerificationKeyHash"
                    , methodDoc =
                        "Get the base16 representation of the hash of the payment verification key of the wallet."
                    , methodParams = []
                    , methodReturnType = OtherType TSString
                    , methodReturnDoc = "The base16 representation of the payment verification key hash."
                    }
              , MethodInfoEntry $
                  MethodInfo
                    { methodName = "getBase16ForStakeVerificationKeyHash"
                    , methodSimpleName = "getBase16ForStakeVerificationKeyHash"
                    , methodDoc = "Get the base16 representation of the hash of the stake verification key of the wallet."
                    , methodParams = []
                    , methodReturnType = OtherType TSString
                    , methodReturnDoc = "The base16 representation of the stake verification key hash."
                    }
              ]
          }

      unsignedTxObj =
        VirtualObjectInfo
          { virtualObjectName = "UnsignedTx"
          , virtualObjectDoc = "Represents an unsigned transaction."
          , virtualObjectMethods =
              [ MethodInfoEntry $
                  MethodInfo
                    { methodName = "addTxInput"
                    , methodSimpleName = "addTxInput"
                    , methodDoc = "Adds a simple transaction input to the transaction."
                    , methodParams =
                        [ ParamInfo "txId" TSString "The transaction ID of the input UTxO."
                        , ParamInfo "txIx" TSNumber "The index of the input within the UTxO."
                        ]
                    , methodReturnType = Fluent
                    , methodReturnDoc = "The `UnsignedTx` object with the added input."
                    }
              , MethodInfoEntry $
                  MethodInfo
                    { methodName = "addSimpleTxOut"
                    , methodSimpleName = "addSimpleTxOut"
                    , methodDoc = "Adds a simple transaction output to the transaction."
                    , methodParams =
                        [ ParamInfo "destAddr" TSString "The destination address."
                        , ParamInfo "lovelaceAmount" TSBigInt "The amount in lovelaces to output."
                        ]
                    , methodReturnType = Fluent
                    , methodReturnDoc = "The `UnsignedTx` object with the added output."
                    }
              , MethodInfoEntry $
                  MethodInfo
                    { methodName = "setFee"
                    , methodSimpleName = "setFee"
                    , methodDoc = "Sets the fee for the transaction."
                    , methodParams = [ParamInfo "lovelaceAmount" TSBigInt "The fee amount in lovelaces."]
                    , methodReturnType = Fluent
                    , methodReturnDoc = "The `UnsignedTx` object with the set fee."
                    }
              , MethodInfoEntry $
                  MethodInfo
                    { methodName = "estimateMinFee"
                    , methodSimpleName = "estimateMinFee"
                    , methodDoc = "Estimates the minimum fee for the transaction."
                    , methodParams =
                        [ ParamInfo "protocolParams" TSAny "The protocol parameters."
                        , ParamInfo
                            "numKeyWitnesses"
                            TSNumber
                            "The number of key witnesses."
                        , ParamInfo "numByronKeyWitnesses" TSNumber "The number of Byron key witnesses."
                        , ParamInfo "totalRefScriptSize" TSNumber "The total size of reference scripts in bytes."
                        ]
                    , methodReturnType = OtherType TSBigInt
                    , methodReturnDoc = "A promise that resolves to the estimated minimum fee in lovelaces."
                    }
              , MethodInfoEntry $
                  MethodInfo
                    { methodName = "signWithPaymentKey"
                    , methodSimpleName = "signWithPaymentKey"
                    , methodDoc = "Signs the transaction with a payment key."
                    , methodParams = [ParamInfo "signingKey" TSString "The signing key to witness the transaction."]
                    , methodReturnType = NewObject (virtualObjectName signedTxObj)
                    , methodReturnDoc = "A promise that resolves to a `SignedTx` object."
                    }
              ]
          }

      signedTxObj =
        VirtualObjectInfo
          { virtualObjectName = "SignedTx"
          , virtualObjectDoc = "Represents a signed transaction."
          , virtualObjectMethods =
              [ MethodInfoEntry $
                  MethodInfo
                    { methodName = "alsoSignWithPaymentKey"
                    , methodSimpleName = "alsoSignWithPaymentKey"
                    , methodDoc = "Adds an extra signature to the transaction with a payment key."
                    , methodParams = [ParamInfo "signingKey" TSString "The signing key to witness the transaction."]
                    , methodReturnType = Fluent
                    , methodReturnDoc = "The `SignedTx` object with the additional signature."
                    }
              , MethodInfoEntry $
                  MethodInfo
                    { methodName = "txToCbor"
                    , methodSimpleName = "txToCbor"
                    , methodDoc = "Converts the signed transaction to its CBOR representation."
                    , methodParams = []
                    , methodReturnType = OtherType TSString
                    , methodReturnDoc =
                        "A promise that resolves to the CBOR representation of the transaction as a hex string."
                    }
              ]
          }

      grpcConnection =
        VirtualObjectInfo
          { virtualObjectName = "GrpcConnection"
          , virtualObjectDoc = "Represents a gRPC-web client connection to a Cardano node."
          , virtualObjectMethods =
              [ MethodInfoEntry $
                  MethodInfo
                    { methodName = "getEra"
                    , methodSimpleName = "getEra"
                    , methodDoc = "Get the era from the Cardano Node using a GRPC-web client."
                    , methodParams = []
                    , methodReturnType = OtherType TSNumber
                    , methodReturnDoc = "A promise that resolves to the current era number."
                    }
              , MethodInfoEntry $
                  MethodInfo
                    { methodName = "submitTx"
                    , methodSimpleName = "submitTx"
                    , methodDoc = "Submit a signed and CBOR-encoded transaction to the Cardano node."
                    , methodParams = [ParamInfo "txCbor" TSString "The CBOR-encoded transaction as a hex string."]
                    , methodReturnType = OtherType TSString
                    , methodReturnDoc = "A promise that resolves to the transaction ID."
                    }
              , MethodInfoEntry $
                  MethodInfo
                    { methodName = "getProtocolParams"
                    , methodSimpleName = "getProtocolParams"
                    , methodDoc =
                        "Get the protocol parameters in the cardano-ledger format from the Cardano Node using a GRPC-web client."
                    , methodParams = []
                    , methodReturnType = OtherType TSAny
                    , methodReturnDoc = "A promise that resolves to the current protocol parameters."
                    }
              , MethodInfoEntry $
                  MethodInfo
                    { methodName = "getAllUtxos"
                    , methodSimpleName = "getAllUtxos"
                    , methodDoc =
                        "Get all UTXOs from the node using a GRPC-web client."
                    , methodParams = []
                    , methodReturnType =
                        OtherType
                          TSUtxoList
                    , methodReturnDoc = "A promise that resolves to the current UTXO set."
                    }
              , MethodInfoEntry $
                  MethodInfo
                    { methodName = "getUtxosForAddress"
                    , methodSimpleName = "getUtxosForAddress"
                    , methodDoc = "Get UTXOs for a given address using a GRPC-web client."
                    , methodParams = [ParamInfo "address" TSString "The address to get UTXOs for."]
                    , methodReturnType =
                        OtherType
                          TSUtxosForAddressList
                    , methodReturnDoc = "A promise that resolves to the UTXOs for the given address."
                    }
              ]
          }
   in ApiInfo
        { mainObject =
            VirtualObjectInfo
              { virtualObjectName = "CardanoApi"
              , virtualObjectDoc = "The main Cardano API object with static methods."
              , virtualObjectMethods =
                  [ MethodGroupEntry $
                      MethodGroup
                        { groupName = "tx"
                        , groupDoc = ["Methods for creating unsigned transactions."]
                        , groupMethods =
                            [ MethodInfoEntry $
                                MethodInfo
                                  { methodName = "newTx"
                                  , methodSimpleName = "newTx"
                                  , methodDoc =
                                      "Create a new unsigned transaction in the current era "
                                        ++ getEraCommentForUnsignedTx (Just newTxImpl)
                                        ++ "."
                                  , methodParams = []
                                  , methodReturnType = NewObject (virtualObjectName unsignedTxObj)
                                  , methodReturnDoc = "A promise that resolves to a new `UnsignedTx` object."
                                  }
                            , MethodInfoEntry $
                                MethodInfo
                                  { methodName = "newExperimentalEraTx"
                                  , methodSimpleName = "newExperimentalEraTx"
                                  , methodDoc =
                                      "Create a new unsigned transaction in the current experimental era "
                                        ++ getEraCommentForUnsignedTx newExperimentalEraTxImpl
                                        ++ "."
                                  , methodParams = []
                                  , methodReturnType = NewObject (virtualObjectName unsignedTxObj)
                                  , methodReturnDoc = "A promise that resolves to a new `UnsignedTx` object."
                                  }
                            , MethodInfoEntry $
                                MethodInfo
                                  { methodName = "newConwayTx"
                                  , methodSimpleName = "newConwayTx"
                                  , methodDoc = "Create a new unsigned transaction in the Conway era."
                                  , methodParams = []
                                  , methodReturnType = NewObject (virtualObjectName unsignedTxObj)
                                  , methodReturnDoc = "A promise that resolves to a new `UnsignedTx` object."
                                  }
                            ]
                        }
                  , MethodInfoEntry $
                      MethodInfo
                        { methodName = "newGrpcConnection"
                        , methodSimpleName = "newGrpcConnection"
                        , methodDoc = "Create a new client connection for communicating with a Cardano node through gRPC-web."
                        , methodParams = [ParamInfo "webGrpcUrl" TSString "The URL of the gRPC-web server."]
                        , methodReturnType = NewObject (virtualObjectName grpcConnection)
                        , methodReturnDoc = "A promise that resolves to a new `GrpcConnection`."
                        }
                  , MethodGroupEntry $
                      MethodGroup
                        { groupName = "wallet"
                        , groupDoc = ["Methods for generating and restoring wallets."]
                        , groupMethods =
                            [ MethodGroupEntry $
                                MethodGroup
                                  { groupName = "mainnet"
                                  , groupDoc = ["Methods for mainnet wallets."]
                                  , groupMethods =
                                      [ MethodInfoEntry $
                                          MethodInfo
                                            { methodName = "generatePaymentWallet"
                                            , methodSimpleName = "generatePaymentWallet"
                                            , methodDoc = "Generate a simple payment wallet for mainnet."
                                            , methodParams = []
                                            , methodReturnType = NewObject (virtualObjectName walletObj)
                                            , methodReturnDoc = "A promise that resolves to a new `Wallet` object."
                                            }
                                      , MethodInfoEntry $
                                          MethodInfo
                                            { methodName = "generateStakeWallet"
                                            , methodSimpleName = "generateStakeWallet"
                                            , methodDoc = "Generate a stake wallet for mainnet."
                                            , methodParams = []
                                            , methodReturnType = NewObject (virtualObjectName walletObj)
                                            , methodReturnDoc = "A promise that resolves to a new `Wallet` object."
                                            }
                                      , MethodInfoEntry $
                                          MethodInfo
                                            { methodName = "restorePaymentWalletFromSigningKeyBech32"
                                            , methodSimpleName = "restorePaymentWalletFromSigningKeyBech32"
                                            , methodDoc = "Restore a mainnet payment wallet from a Bech32 encoded signing key."
                                            , methodParams = [ParamInfo "signingKeyBech32" TSString "The Bech32 encoded signing key."]
                                            , methodReturnType = NewObject (virtualObjectName walletObj)
                                            , methodReturnDoc = "A promise that resolves to a new `Wallet` object."
                                            }
                                      , MethodInfoEntry $
                                          MethodInfo
                                            { methodName = "restoreStakeWalletFromSigningKeyBech32"
                                            , methodSimpleName = "restoreStakeWalletFromSigningKeyBech32"
                                            , methodDoc = "Restore a mainnet stake wallet from Bech32 encoded signing keys."
                                            , methodParams =
                                                [ ParamInfo "paymentSigningKeyBech32" TSString "The Bech32 encoded payment signing key."
                                                , ParamInfo "stakeSigningKeyBech32" TSString "The Bech32 encoded stake signing key."
                                                ]
                                            , methodReturnType = NewObject (virtualObjectName walletObj)
                                            , methodReturnDoc = "A promise that resolves to a new `Wallet` object."
                                            }
                                      ]
                                  }
                            , MethodGroupEntry $
                                MethodGroup
                                  { groupName = "testnet"
                                  , groupDoc = ["Methods for wallets in other networks."]
                                  , groupMethods =
                                      [ MethodInfoEntry $
                                          MethodInfo
                                            { methodName = "generateTestnetPaymentWallet"
                                            , methodSimpleName = "generateTestnetPaymentWallet"
                                            , methodDoc = "Generate a simple payment wallet for testnet, given the testnet's network magic."
                                            , methodParams = [ParamInfo "networkMagic" TSNumber "The network magic for the testnet."]
                                            , methodReturnType = NewObject (virtualObjectName walletObj)
                                            , methodReturnDoc = "A promise that resolves to a new `Wallet` object."
                                            }
                                      , MethodInfoEntry $
                                          MethodInfo
                                            { methodName = "generateTestnetStakeWallet"
                                            , methodSimpleName = "generateTestnetStakeWallet"
                                            , methodDoc = "Generate a stake wallet for testnet, given the testnet's network magic."
                                            , methodParams = [ParamInfo "networkMagic" TSNumber "The network magic for the testnet."]
                                            , methodReturnType = NewObject (virtualObjectName walletObj)
                                            , methodReturnDoc = "A promise that resolves to a new `Wallet` object."
                                            }
                                      , MethodInfoEntry $
                                          MethodInfo
                                            { methodName = "restoreTestnetPaymentWalletFromSigningKeyBech32"
                                            , methodSimpleName = "restoreTestnetPaymentWalletFromSigningKeyBech32"
                                            , methodDoc = "Restore a testnet payment wallet from a Bech32 encoded signing key."
                                            , methodParams =
                                                [ ParamInfo "networkMagic" TSNumber "The network magic for the testnet."
                                                , ParamInfo "signingKeyBech32" TSString "The Bech32 encoded signing key."
                                                ]
                                            , methodReturnType = NewObject (virtualObjectName walletObj)
                                            , methodReturnDoc = "A promise that resolves to a new `Wallet` object."
                                            }
                                      , MethodInfoEntry $
                                          MethodInfo
                                            { methodName = "restoreTestnetStakeWalletFromSigningKeyBech32"
                                            , methodSimpleName = "restoreTestnetStakeWalletFromSigningKeyBech32"
                                            , methodDoc = "Restore a testnet stake wallet from Bech32 encoded signing keys."
                                            , methodParams =
                                                [ ParamInfo "networkMagic" TSNumber "The network magic for the testnet."
                                                , ParamInfo "paymentSigningKeyBech32" TSString "The Bech32 encoded payment signing key."
                                                , ParamInfo "stakeSigningKeyBech32" TSString "The Bech32 encoded stake signing key."
                                                ]
                                            , methodReturnType = NewObject (virtualObjectName walletObj)
                                            , methodReturnDoc = "A promise that resolves to a new `Wallet` object."
                                            }
                                      ]
                                  }
                            ]
                        }
                  ]
              }
        , virtualObjects = [unsignedTxObj, signedTxObj, grpcConnection, walletObj]
        , initialiseFunctionDoc = "Initialises the Cardano API."
        , initialiseFunctionReturnDoc = "A promise that resolves to the main `CardanoApi` object."
        }
