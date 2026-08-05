module Types exposing (..)

{-| Every data type in the application: the Model (all state in one record)
and the Msg (everything that can happen).
-}

import Dict exposing (Dict)
import Http
import Set exposing (Set)


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
    , selected : Bool -- ticked as an input in the payment builder
    , hasAssets : Bool -- carries native tokens; unusable as input in this ADA-only demo
    }


{-| One fetched page of a wallet's UTxOs. `truncated` means the page came back
full, so the on-chain set may be larger and sums are lower bounds.
-}
type alias UtxoPage =
    { utxos : List Utxo
    , truncated : Bool
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
    , utxos : Loadable UtxoPage
    , expanded : Bool
    , color : String
    }


type alias BookEntry =
    { alias : String
    , address : String
    }


type OutputAmount
    = Lovelace String
    | Change


type alias Output =
    { address : String
    , alias : String
    , amount : OutputAmount
    }


{-| Which family of networks an address belongs to. Addresses only encode
mainnet-vs-testnet, so preprod and preview cannot be told apart.
-}
type NetKind
    = MainKind
    | TestKind


{-| Result of cardano-wasm's inspectAddress for one address.
-}
type AddrCheck
    = CheckInvalid
    | CheckValid NetKind
    | CheckFailed -- the checker itself errored; not a verdict about the address


type Modal
    = NoModal
    | ForgetDialog WalletId


type alias BookForm =
    { open : Bool, alias : String, address : String }


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

    -- a network switch re-derives addresses asynchronously; loads wait for it
    , deriving : Bool

    -- wallets whose Loaded UTxO page is being refreshed; the page (and its
    -- ticked inputs) stays visible until the response lands
    , reloading : Set WalletId
    , wallets : List Wallet
    , nextWid : Int
    , book : List BookEntry
    , outputs : List Output
    , modal : Modal
    , bfKeys : BfKeys
    , restore : RestoreForm
    , bookForm : BookForm
    , console : List LogLine
    , toast : Maybe String
    , toastSeq : Int
    , addrChecks : Dict String AddrCheck -- inspectAddress results, keyed by address
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
    | GotUtxos WalletId Network (Result String UtxoPage)
    | ToggleUtxoSelected WalletId String Int
    | ClickAddBookToggle
    | UpdateBookAlias String
    | UpdateBookAddr String
    | SaveBookEntry
    | CancelBookEntry
    | DeleteBookEntry Int
    | UseBookAddress String String
    | UpdateOutputAmount Int String
    | ToggleOutputChange Int
    | DeleteOutput Int
    | ClearInputs
    | ClearOutputs
    | GotAddressInspected (Result String ( String, AddrCheck ))
    | Copy String
    | ClearConsole
    | ClearToast Int
    | NoOp
