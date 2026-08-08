module State exposing
    ( adaOnlyMinUtxo
    , addWallet
    , addrFlagged
    , addrIssue
    , addrVerdict
    , aliasOf
    , amountBelowMin
    , balance
    , canSign
    , changeAddress
    , changeRow
    , computeBalance
    , currentKey
    , deselectInputs
    , distinct
    , emptyBookForm
    , emptyRestoreForm
    , explicitOutputsTotal
    , fetching
    , getWallet
    , init
    , inputsTotal
    , invalidate
    , invalidateShape
    , log
    , mapWallet
    , outputAddressesOk
    , outputsComplete
    , ownBook
    , paymentWalletIds
    , removeAt
    , selectedInputs
    , setBookAddr
    , setBookAlias
    , setCurrentKey
    , setRestorePay
    , setRestoreStake
    , startFetch
    , toastNow
    , toggleBook
    , toggleOutputChange
    , toggleRestore
    , toggleUtxo
    , txReady
    , updateAt
    , utxosTruncated
    , walletBalance
    , witnessCount
    )

{-| Everything about the Model: the initial state, derived queries (what the view
and update read), and the small pure updaters. No commands except the toast timer.
-}

import Dict
import Format exposing (adaToLovelace)
import Net exposing (expectedNetKind)
import Process
import Set
import Task
import Types exposing (..)



-- INIT


init : Protocol -> ( Model, Cmd Msg )
init protocol =
    ( { network = Preview
      , deriving = False
      , reloading = Set.empty
      , wallets = []
      , nextWid = 1
      , book = []
      , outputs = []
      , era = Conway
      , fee = NoFee
      , feeText = ""
      , tx = Draft
      , modal = NoModal
      , bfKeys = { mainnet = "", preprod = "", preview = "" }
      , restore = emptyRestoreForm
      , bookForm = emptyBookForm
      , console =
            [ LogLine LogInfo "cardano-wasm loaded · post-link module ready" ]
      , toast = Nothing
      , toastSeq = 0
      , addrChecks = Dict.empty
      , protocol = protocol
      }
    , Cmd.none
    )


emptyRestoreForm : RestoreForm
emptyRestoreForm =
    { open = False, paymentSkey = "", stakeSkey = "" }


emptyBookForm : BookForm
emptyBookForm =
    { open = False, alias = "", address = "" }



-- BLOCKFROST KEY (stored per network, in memory only)


currentKey : Model -> String
currentKey model =
    case model.network of
        Mainnet ->
            model.bfKeys.mainnet

        Preprod ->
            model.bfKeys.preprod

        Preview ->
            model.bfKeys.preview


setCurrentKey : String -> Model -> Model
setCurrentKey v model =
    let
        -- project ids are plain ASCII; pasted whitespace or invisible characters
        -- (a zero-width space would even make the request header throw) must not
        -- reach the header
        cleaned =
            String.filter (\c -> Char.toCode c > 32 && Char.toCode c < 127) v

        k =
            model.bfKeys
    in
    { model
        | bfKeys =
            case model.network of
                Mainnet ->
                    { k | mainnet = cleaned }

                Preprod ->
                    { k | preprod = cleaned }

                Preview ->
                    { k | preview = cleaned }
    }



-- WALLETS


getWallet : WalletId -> Model -> Maybe Wallet
getWallet wid model =
    List.filter (\w -> w.id == wid) model.wallets |> List.head


mapWallet : WalletId -> (Wallet -> Wallet) -> Model -> Model
mapWallet wid f model =
    { model
        | wallets =
            List.map
                (\w ->
                    if w.id == wid then
                        f w

                    else
                        w
                )
                model.wallets
    }


aliasOf : WalletId -> Model -> String
aliasOf wid model =
    getWallet wid model |> Maybe.map .alias |> Maybe.withDefault "?"


avatarColors : List String
avatarColors =
    [ "#3b73ff", "#33d17a", "#f6b73c", "#ff6b6b", "#a06bff", "#19cdd7", "#ff9ed6", "#5ee89c" ]


addWallet : GenPayload -> Model -> Model
addWallet p model =
    let
        color =
            List.drop (modBy (List.length avatarColors) (model.nextWid - 1)) avatarColors
                |> List.head
                |> Maybe.withDefault "#3b73ff"

        w =
            { id = model.nextWid
            , alias = "Wallet " ++ String.fromInt model.nextWid
            , address = p.address
            , keys = p.keys
            , utxos = NotAsked
            , expanded = True
            , color = color
            }
    in
    { model | wallets = model.wallets ++ [ w ], nextWid = model.nextWid + 1 }


{-| A request for this wallet is in flight: a first load (Loading) or a
reload (id in model.reloading, page kept visible meanwhile).
-}
fetching : Wallet -> Model -> Bool
fetching w model =
    w.utxos == Loading || Set.member w.id model.reloading


{-| Mark a wallet's fetch as started. A first load shows Loading; a reload
keeps the current page (and its ticked inputs) visible until the response.
-}
startFetch : Wallet -> Model -> Model
startFetch w model =
    case w.utxos of
        Loaded _ ->
            { model | reloading = Set.insert w.id model.reloading }

        _ ->
            mapWallet w.id (\x -> { x | utxos = Loading }) model


{-| The wallet's on-chain ADA. It includes lovelace sitting on token-carrying
UTxOs the demo cannot spend: this is the real chain balance, not a spendable
amount (a later slice distinguishes the two when building payments).
-}
walletBalance : Wallet -> Loadable Int
walletBalance w =
    case w.utxos of
        NotAsked ->
            NotAsked

        Loading ->
            Loading

        Failed e ->
            Failed e

        Loaded page ->
            Loaded (List.map .lovelace page.utxos |> List.sum)


utxosTruncated : Wallet -> Bool
utxosTruncated w =
    case w.utxos of
        Loaded page ->
            page.truncated

        _ ->
            False



-- CONSOLE & TOAST


log : LogLevel -> String -> Model -> Model
log level text model =
    let
        entries =
            model.console ++ [ LogLine level text ]
    in
    -- keep the last 200 lines only
    { model | console = List.drop (List.length entries - 200) entries }


{-| Show a toast and schedule its dismissal; the sequence number ignores stale timers.
-}
toastNow : String -> Model -> ( Model, Cmd Msg )
toastNow text model =
    let
        seq =
            model.toastSeq + 1
    in
    ( { model | toast = Just text, toastSeq = seq }
    , Process.sleep 1900 |> Task.perform (\_ -> ClearToast seq)
    )



-- SMALL FORM UPDATERS


toggleRestore : RestoreForm -> RestoreForm
toggleRestore r =
    { r | open = not r.open }


setRestorePay : String -> RestoreForm -> RestoreForm
setRestorePay s r =
    { r | paymentSkey = s }


setRestoreStake : String -> RestoreForm -> RestoreForm
setRestoreStake s r =
    { r | stakeSkey = s }


{-| The address book shows own wallets first (derived, always current) plus the
manually added external entries stored in model.book.
-}
ownBook : Model -> List BookEntry
ownBook model =
    List.map (\w -> BookEntry w.alias w.address) model.wallets



-- INPUTS (selected UTxOs across all wallets)


{-| The ticked inputs across all wallets, deduplicated by outpoint: restoring
the same keys twice lists the same UTxOs under two wallets, and one outpoint
must only be spent once.
-}
selectedInputs : Model -> List ( Wallet, Utxo )
selectedInputs model =
    model.wallets
        |> List.concatMap
            (\w ->
                case w.utxos of
                    Loaded page ->
                        page.utxos |> List.filter .selected |> List.map (\u -> ( w, u ))

                    _ ->
                        []
            )
        |> dedupeByOutpoint


dedupeByOutpoint : List ( Wallet, Utxo ) -> List ( Wallet, Utxo )
dedupeByOutpoint pairs =
    List.foldl
        (\( w, u ) ( seen, acc ) ->
            if Set.member ( u.txId, u.txIx ) seen then
                ( seen, acc )

            else
                ( Set.insert ( u.txId, u.txIx ) seen, ( w, u ) :: acc )
        )
        ( Set.empty, [] )
        pairs
        |> Tuple.second
        |> List.reverse


toggleUtxo : String -> Int -> Wallet -> Wallet
toggleUtxo txId txIx w =
    case w.utxos of
        Loaded page ->
            -- token-bearing UTxOs stay unselectable (the checkbox is disabled too)
            { w
                | utxos =
                    Loaded
                        { page
                            | utxos =
                                List.map
                                    (\u ->
                                        if u.txId == txId && u.txIx == txIx && not u.hasAssets then
                                            { u | selected = not u.selected }

                                        else
                                            u
                                    )
                                    page.utxos
                        }
            }

        _ ->
            w


deselectInputs : Model -> Model
deselectInputs model =
    { model
        | wallets =
            List.map
                (\w ->
                    case w.utxos of
                        Loaded page ->
                            { w | utxos = Loaded { page | utxos = List.map (\u -> { u | selected = False }) page.utxos } }

                        _ ->
                            w
                )
                model.wallets
    }


{-| Mark/unmark output i as the change output; at most one: marking one
unmarks any other.
-}
toggleOutputChange : Int -> List Output -> List Output
toggleOutputChange i outputs =
    List.indexedMap
        (\j o ->
            if j == i then
                { o
                    | amount =
                        if o.amount == Change then
                            Lovelace ""

                        else
                            Change
                }

            else if o.amount == Change then
                { o | amount = Lovelace "" }

            else
                o
        )
        outputs



-- TRANSACTION TOTALS (read by the fee/signing UI)


inputsTotal : Model -> Int
inputsTotal model =
    selectedInputs model |> List.map (\( _, u ) -> u.lovelace) |> List.sum


{-| Sum of the typed (non-change) outputs.
-}
explicitOutputsTotal : Model -> Int
explicitOutputsTotal model =
    model.outputs
        |> List.filterMap
            (\o ->
                case o.amount of
                    Lovelace s ->
                        adaToLovelace s

                    Change ->
                        Nothing
            )
        |> List.sum


{-| The output marked "change" (at most one), if any.
-}
changeRow : Model -> Maybe Output
changeRow model =
    model.outputs |> List.filter (\o -> o.amount == Change) |> List.head


{-| Every non-change output parses to at least the ADA-only min-UTxO amount —
the node rejects smaller outputs at submission, so they must not reach signing.
-}
outputsComplete : Model -> Bool
outputsComplete model =
    List.all
        (\o ->
            case o.amount of
                Change ->
                    True

                Lovelace s ->
                    adaToLovelace s
                        |> Maybe.map (\n -> n >= adaOnlyMinUtxo model.protocol)
                        |> Maybe.withDefault False
        )
        model.outputs


{-| A typed amount that parses but is too small for the chain to accept.
-}
amountBelowMin : Model -> String -> Bool
amountBelowMin model s =
    case adaToLovelace s of
        Just n ->
            n > 0 && n < adaOnlyMinUtxo model.protocol

        Nothing ->
            False


{-| Every output address is confirmed valid and on the right network kind.
-}
outputAddressesOk : Model -> Bool
outputAddressesOk model =
    List.all (\o -> addrIssue model o.address == Nothing) model.outputs



-- ADDRESS CHECKS (results of cardano-wasm's inspectAddress, cached by address)


{-| The cached verdict for an address, if it has been inspected.
-}
addrVerdict : Model -> String -> Maybe AddrCheck
addrVerdict model a =
    Dict.get a model.addrChecks


{-| Nothing = address is fine; Just reason otherwise. Unchecked counts as a problem
(inspection is near-instant, so this only blocks momentarily).
-}
addrIssue : Model -> String -> Maybe String
addrIssue model a =
    case addrVerdict model a of
        Nothing ->
            Just "checking address…"

        Just CheckInvalid ->
            Just "invalid address"

        Just CheckFailed ->
            Just "address check failed"

        Just (CheckValid kind) ->
            if kind == expectedNetKind model.network then
                Nothing

            else
                Just
                    ("wrong network ("
                        ++ (if kind == MainKind then
                                "mainnet"

                            else
                                "testnet"
                           )
                        ++ " address)"
                    )


{-| Like addrIssue, but only settled answers (invalid / wrong network / check
failed) — an address still being checked is not flagged. For display; gating
(fee/sign) uses addrIssue.
-}
addrFlagged : Model -> String -> Maybe String
addrFlagged model a =
    addrVerdict model a |> Maybe.andThen (\_ -> addrIssue model a)


toggleBook : BookForm -> BookForm
toggleBook b =
    { b | open = not b.open }


setBookAlias : String -> BookForm -> BookForm
setBookAlias s b =
    { b | alias = s }


setBookAddr : String -> BookForm -> BookForm
setBookAddr s b =
    { b | address = s }


removeAt : Int -> List a -> List a
removeAt i xs =
    List.indexedMap (\j x -> ( j, x )) xs
        |> List.filter (\( j, _ ) -> j /= i)
        |> List.map Tuple.second


updateAt : Int -> (a -> a) -> List a -> List a
updateAt i f xs =
    List.indexedMap
        (\j x ->
            if j == i then
                f x

            else
                x
        )
        xs



-- WITNESSES
-- Payment witnesses come from the wallets whose UTxOs are spent. Distinct per wallet.


paymentWalletIds : Model -> List WalletId
paymentWalletIds model =
    selectedInputs model |> List.map (\( w, _ ) -> w.id) |> distinct


witnessCount : Model -> Int
witnessCount model =
    List.length (paymentWalletIds model)


{-| Where the remainder goes: the change output if marked, else the first input wallet.
-}
changeAddress : Model -> Maybe String
changeAddress model =
    case changeRow model of
        Just o ->
            Just o.address

        Nothing ->
            selectedInputs model |> List.head |> Maybe.map (\( w, _ ) -> w.address)


{-| Balance arithmetic: change = inputs − outputs − deposit − fee, plus the
ADA-only min-UTxO check on the change. Plain integer bookkeeping over lovelace
totals — fee estimation, serialisation and signing all happen in cardano-wasm.
-}
computeBalance : Int -> { inputs : Int, outputs : Int, deposit : Int, fee : Int } -> Balance
computeBalance minUtxo t =
    let
        change =
            t.inputs - t.outputs - t.deposit - t.fee
    in
    if change < 0 then
        Insufficient (negate change)

    else if change == 0 then
        Balanced 0

    else if change < minUtxo then
        DustChange change minUtxo

    else
        Balanced change


{-| Minimum lovelace an ADA-only output must hold:
(≈65 B output + 160 B overhead) × coinsPerUtxoByte ≈ 0.97 ₳.
Only valid for ADA-only outputs — with native assets the size (and thus the
minimum) grows, which is one reason this demo blocks token-bearing UTxOs.
-}
adaOnlyMinUtxo : Protocol -> Int
adaOnlyMinUtxo protocol =
    protocol.coinsPerUtxoByte * 225


balanceWith : Int -> Model -> Balance
balanceWith fee model =
    computeBalance (adaOnlyMinUtxo model.protocol)
        { inputs = inputsTotal model
        , outputs = explicitOutputsTotal model
        , deposit = 0 -- deposits arrive with certificates
        , fee = fee
        }


balance : Model -> Balance
balance model =
    case model.fee of
        FeeSet fee ->
            balanceWith fee model

        _ ->
            NoFeeYet



-- READINESS GATES


{-| Enough of a transaction to estimate a fee: at least one input, something to
send, and no invalid amounts or addresses.
-}
txReady : Model -> Bool
txReady model =
    not (List.isEmpty (selectedInputs model))
        && (explicitOutputsTotal model > 0 || changeRow model /= Nothing)
        && outputsComplete model
        && outputAddressesOk model


{-| The sign button (and handler) gate: ready, fee set, balanced, still a draft.
-}
canSign : Model -> Bool
canSign model =
    case ( model.fee, balance model, model.tx ) of
        ( FeeSet _, Balanced _, Draft ) ->
            txReady model

        _ ->
            False



-- STALENESS
-- A signed tx is a snapshot: any later edit makes it stale.


{-| Drop back to Draft. For edits that do NOT change the transaction body
(e.g. typing a new fee).
-}
invalidate : Model -> Model
invalidate model =
    case model.tx of
        Draft ->
            model

        _ ->
            { model | tx = Draft }


{-| For edits that change the transaction _shape_ (inputs, outputs, certificates,
era). Those also reset the fee: a previously estimated fee would now be wrong
(possibly below the minimum, which the node only rejects at submission).
-}
invalidateShape : Model -> Model
invalidateShape model =
    { model | tx = Draft, fee = NoFee, feeText = "" }


{-| Deduplicate, keeping the first occurrence of each element (stable order).
-}
distinct : List comparable -> List comparable
distinct xs =
    List.foldl
        (\x acc ->
            if List.member x acc then
                acc

            else
                acc ++ [ x ]
        )
        []
        xs
