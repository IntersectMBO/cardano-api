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
    , selected : Bool
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


type CertAction
    = Register
    | RegisterAndDelegate PoolRef
    | DelegateOnly PoolRef
    | Unregister


{-| What a certificate remembers about its pool, captured at pick time: the
bech32 id and ticker for display, and Blockfrost's authoritative hex, which is
what goes into the certificate. Pinning these here means a certificate never
depends on which picker page happens to be loaded later.
-}
type alias PoolRef =
    { bech32 : String
    , hex : String
    , ticker : Maybe String
    }


type alias Certificate =
    { wallet : WalletId
    , action : CertAction
    }


type alias Pool =
    { idBech32 : String
    , idHex : String
    , ticker : Maybe String -- pools without registered metadata have none
    , liveStake : Int
    , saturation : Float
    }


{-| One fetched page of the pool list. `hasMore` is computed at the fetch
boundary: a full Blockfrost page means a next one probably exists.
-}
type alias PoolPage =
    { pools : List Pool
    , hasMore : Bool
    }


type Era
    = Conway
    | Dijkstra


type FeeState
    = NoFee
    | EstimatingFee
    | FeeSet Int


type alias SignedTx =
    { cbor : String
    , txId : String
    , paymentWits : Int
    , stakeWits : Int
    }


type alias SignedPayload =
    { cbor : String, txId : String }


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


type TxState
    = Draft
    | Signing
    | Signed SignedTx


type SubmitState
    = NotSubmitted
    | Submitting
    | Submitted String
    | SubmitFailed String


type DelegKind
    = RegThenDeleg
    | DelegOnly


type PoolPurpose
    = ForNewCert WalletId DelegKind
    | ForEditCert Int


type Modal
    = NoModal
    | PoolPicker PoolPurpose String
    | ForgetDialog WalletId


type alias RestoreForm =
    { open : Bool, paymentSkey : String, stakeSkey : String }


type alias BookForm =
    { open : Bool, alias : String, address : String }


type LogLevel
    = LogInfo
    | LogOk
    | LogWarn
    | LogCmd -- echo of the cardano-wasm call being made


type alias LogLine =
    { level : LogLevel, text : String }


type alias GenPayload =
    { address : String, keys : Keys }


{-| The two protocol parameters the Elm side needs for its balance arithmetic.
Read from web/pparams.js at startup (see web/ports.js) so the pinned object is
the single source of truth; everything else in it is consumed only by
cardano-wasm's estimateMinFee.
-}
type alias Protocol =
    { keyDeposit : Int
    , coinsPerUtxoByte : Int
    }


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
    , certs : List Certificate
    , era : Era
    , fee : FeeState
    , feeText : String
    , tx : TxState
    , submit : SubmitState
    , pools : Loadable PoolPage -- the page of pools currently shown in the picker
    , poolPage : Int -- its 1-based page number (one server page per view)
    , modal : Modal
    , restore : RestoreForm
    , bookForm : BookForm
    , console : List LogLine
    , toast : Maybe String
    , toastSeq : Int
    , bfKeys : BfKeys
    , addrChecks : Dict String AddrCheck -- inspectAddress results, keyed by address
    , protocol : Protocol
    }


type alias BfKeys =
    { mainnet : String, preprod : String, preview : String }


type Msg
    = SelectNetwork Network
    | UpdateBfKey String
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
    | SetWalletCert WalletId String
    | DeleteCertificate Int
    | ChangeCertPool Int
    | ClearInputs
    | ClearOutputs
    | ClearCerts
    | ClearTx
    | UpdatePoolSearch String
    | PickPool PoolRef
    | ClosePoolModal
    | ClickLoadPools
    | ClickPoolPage Int
    | GotPools Network Int (Result String PoolPage)
    | SelectEra Era
    | ClickEstimateFee
    | GotFeeEstimated (Result String Int)
    | UpdateFeeText String
    | ClickSign
    | GotTxSigned (Result String SignedPayload)
    | GotAddressInspected (Result String ( String, AddrCheck ))
    | ClickDownloadCli
    | ClickSubmit
    | GotSubmitted String (Result String String)
    | Copy String
    | ClearConsole
    | ClearToast Int
    | NoOp


type Balance
    = NoFeeYet
    | Insufficient Int
    | DustChange Int Int
    | Balanced Int
