module Types exposing (..)

{-| Every data type in the application: the Model (all state in one record)
and the Msg (everything that can happen).
-}

import Http


type Network
    = Mainnet
    | Preprod
    | Preview


type alias WalletId =
    Int


type alias Keys =
    { paymentVKey : String
    , paymentSKey : String
    , stakeVKey : String
    , stakeSKey : String
    , paymentKeyHash : String
    , stakeKeyHash : String
    }


type alias Utxo =
    { txId : String
    , txIx : Int
    , lovelace : Int
    , selected : Bool
    , hasAssets : Bool -- carries native tokens; unusable as input in this ADA-only demo
    }


type Loadable a
    = NotAsked
    | Loading
    | Loaded a
    | Failed String


type alias Wallet =
    { id : WalletId
    , alias : String
    , address : String
    , keys : Keys
    , utxos : Loadable (List Utxo)
    , expanded : Bool
    , color : String
    }


type Modal
    = NoModal
    | ForgetDialog WalletId


type alias RestoreForm =
    { open : Bool, paymentSkey : String, stakeSkey : String }


type LogLevel
    = LogInfo
    | LogOk
    | LogWarn
    | LogCmd -- echo of the cardano-wasm call being made


type alias LogLine =
    { level : LogLevel, text : String }


type alias GenPayload =
    { address : String, keys : Keys }


type alias Model =
    { network : Network
    , wallets : List Wallet
    , nextWid : Int
    , modal : Modal
    , bfKeys : BfKeys
    , restore : RestoreForm
    , console : List LogLine
    , toast : Maybe String
    , toastSeq : Int
    }


type alias BfKeys =
    { mainnet : String, preprod : String, preview : String }


type Msg
    = SelectNetwork Network
    | ClickNewWallet
    | GotGeneratedWallet (Result String GenPayload)
    | ClickRestoreToggle
    | UpdateRestorePay String
    | UpdateRestoreStake String
    | SubmitRestore
    | CancelRestore
    | GotRestoredWallet (Result String GenPayload)
    | GotDerivedAddresses (Result String (List ( WalletId, String )))
    | ToggleWalletExpanded WalletId
    | EditAlias WalletId String
    | RequestForget WalletId
    | ConfirmForget WalletId
    | CancelForget
    | UpdateBfKey String
    | ClickLoadUtxos WalletId
    | ClickLoadAll
    | GotUtxos WalletId (Result Http.Error (List Utxo))
    | Copy String
    | ClearConsole
    | ClearToast Int
    | NoOp
