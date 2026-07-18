module State exposing
    ( adaOnlyMinUtxo
    , addCert
    , addWallet
    , addrFlagged
    , addrIssue
    , addrVerdict
    , aliasOf
    , balance
    , balanceWith
    , canSign
    , certCode
    , certMenu
    , changeAddress
    , changeRow
    , computeBalance
    , currentKey
    , depositTotal
    , deselectInputs
    , distinct
    , emptyBookForm
    , emptyRestoreForm
    , explicitOutputsTotal
    , getWallet
    , init
    , inputsTotal
    , invalidate
    , invalidateShape
    , isFailed
    , loadedPools
    , log
    , mapWallet
    , outputAddressesOk
    , outputsComplete
    , ownBook
    , paymentWalletIds
    , poolByIdIn
    , poolHex
    , removeAt
    , selectedInputs
    , setBookAddr
    , setBookAlias
    , setCertPool
    , setCurrentKey
    , setRestorePay
    , setRestoreStake
    , stakeHashOf
    , stakeWalletIds
    , toastNow
    , toggleBook
    , toggleRestore
    , toggleUtxo
    , txReady
    , updateAt
    , walletBalance
    , walletCertAction
    , witnessCount
    )

{-| Everything about the Model: the initial state, derived queries (what the view
and update read), and the small pure updaters. No commands except the toast timer.
-}

import Bech32
import Dict
import Format exposing (adaToLovelace)
import Net exposing (expectedNetKind)
import Process
import Task
import Types exposing (..)



-- INIT


init : Protocol -> ( Model, Cmd Msg )
init protocol =
    ( { network = Mainnet
      , wallets = []
      , nextWid = 1
      , book = []
      , outputs = []
      , certs = []
      , era = Conway
      , fee = NoFee
      , feeText = ""
      , tx = Draft
      , submit = NotSubmitted
      , pools = NotAsked
      , poolPage = 1
      , modal = NoModal
      , restore = emptyRestoreForm
      , bookForm = emptyBookForm
      , console =
            [ LogLine LogInfo "cardano-wasm loaded · post-link module ready"
            , LogLine LogInfo "data: Blockfrost (enter a free project id) · pinned protocol params"
            ]
      , toast = Nothing
      , toastSeq = 0
      , bfKeys = { mainnet = "", preprod = "", preview = "" }
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
        k =
            model.bfKeys
    in
    { model
        | bfKeys =
            case model.network of
                Mainnet ->
                    { k | mainnet = v }

                Preprod ->
                    { k | preprod = v }

                Preview ->
                    { k | preview = v }
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


walletBalance : Wallet -> Maybe Int
walletBalance w =
    case w.utxos of
        Loaded us ->
            Just (List.map .lovelace us |> List.sum)

        _ ->
            Nothing


{-| The address book shows own wallets first (derived, always current) plus the
manually added external entries stored in model.book.
-}
ownBook : Model -> List BookEntry
ownBook model =
    List.map (\w -> BookEntry w.alias w.address) model.wallets



-- INPUTS (selected UTxOs across all wallets)


selectedInputs : Model -> List ( Wallet, Utxo )
selectedInputs model =
    model.wallets
        |> List.concatMap
            (\w ->
                case w.utxos of
                    Loaded us ->
                        us |> List.filter .selected |> List.map (\u -> ( w, u ))

                    _ ->
                        []
            )


toggleUtxo : String -> Int -> Wallet -> Wallet
toggleUtxo txId txIx w =
    case w.utxos of
        Loaded us ->
            -- token-bearing UTxOs stay unselectable (the checkbox is disabled too)
            { w
                | utxos =
                    Loaded
                        (List.map
                            (\u ->
                                if u.txId == txId && u.txIx == txIx && not u.hasAssets then
                                    { u | selected = not u.selected }

                                else
                                    u
                            )
                            us
                        )
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
                        Loaded us ->
                            { w | utxos = Loaded (List.map (\u -> { u | selected = False }) us) }

                        _ ->
                            w
                )
                model.wallets
    }



-- TRANSACTION TOTALS


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


{-| Net deposit: +keyDeposit per registration, −keyDeposit (refund) per unregistration.
-}
depositTotal : Model -> Int
depositTotal model =
    model.certs
        |> List.map
            (\c ->
                case c.action of
                    Register ->
                        model.protocol.keyDeposit

                    RegisterAndDelegate _ ->
                        model.protocol.keyDeposit

                    Unregister ->
                        negate model.protocol.keyDeposit

                    DelegateOnly _ ->
                        0
            )
        |> List.sum



-- WITNESSES
-- Payment witnesses come from wallets whose UTxOs are spent; stake witnesses from
-- wallets that carry a certificate. Distinct per wallet.


paymentWalletIds : Model -> List WalletId
paymentWalletIds model =
    selectedInputs model |> List.map (\( w, _ ) -> w.id) |> distinct


stakeWalletIds : Model -> List WalletId
stakeWalletIds model =
    model.certs |> List.map .wallet |> distinct


witnessCount : Model -> Int
witnessCount model =
    List.length (paymentWalletIds model) + List.length (stakeWalletIds model)



-- CHANGE & BALANCE


{-| The output marked "change" (at most one), if any.
-}
changeRow : Model -> Maybe Output
changeRow model =
    model.outputs |> List.filter (\o -> o.amount == Change) |> List.head


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
        , deposit = depositTotal model
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


{-| Enough of a transaction to estimate a fee: at least one input, something to do
(outputs or certificates), and no invalid amounts or addresses.
-}
txReady : Model -> Bool
txReady model =
    not (List.isEmpty (selectedInputs model))
        && (explicitOutputsTotal model > 0 || changeRow model /= Nothing || not (List.isEmpty model.certs))
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


{-| Every non-change output parses to a positive lovelace amount.
-}
outputsComplete : Model -> Bool
outputsComplete model =
    List.all
        (\o ->
            case o.amount of
                Change ->
                    True

                Lovelace s ->
                    adaToLovelace s |> Maybe.map (\n -> n > 0) |> Maybe.withDefault False
        )
        model.outputs


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


{-| Like addrIssue, but only _definitive_ problems (invalid / wrong network) — an
address still being checked is not flagged. For display; gating uses addrIssue.
-}
addrFlagged : Model -> String -> Maybe String
addrFlagged model a =
    addrVerdict model a |> Maybe.andThen (\_ -> addrIssue model a)



-- CERTIFICATES
-- One certificate per wallet, chosen from a small menu. The menu codes below are the
-- single source of truth shared by the view (options) and the update (parsing).


certMenu : List ( String, String )
certMenu =
    [ ( "", "no certificate" )
    , ( "reg", "Register stake key" )
    , ( "deleg", "Register + delegate" )
    , ( "delegonly", "Delegate only" )
    , ( "unreg", "Unregister stake key" )
    ]


certCode : CertAction -> String
certCode action =
    case action of
        Register ->
            "reg"

        RegisterAndDelegate _ ->
            "deleg"

        DelegateOnly _ ->
            "delegonly"

        Unregister ->
            "unreg"


{-| The menu code of the wallet's current certificate ("" = none) — keeps the
per-wallet select in sync with the certificate list.
-}
walletCertAction : WalletId -> Model -> String
walletCertAction wid model =
    List.filter (\c -> c.wallet == wid) model.certs
        |> List.head
        |> Maybe.map (.action >> certCode)
        |> Maybe.withDefault ""


addCert : Certificate -> Model -> Model
addCert c model =
    { model | certs = model.certs ++ [ c ] } |> invalidateShape


setCertPool : String -> Certificate -> Certificate
setCertPool pid c =
    { c
        | action =
            case c.action of
                RegisterAndDelegate _ ->
                    RegisterAndDelegate pid

                DelegateOnly _ ->
                    DelegateOnly pid

                other ->
                    other
    }


stakeHashOf : WalletId -> Model -> String
stakeHashOf wid model =
    getWallet wid model |> Maybe.map (\w -> w.keys.stakeKeyHash) |> Maybe.withDefault ""



-- POOLS


loadedPools : Model -> List Pool
loadedPools model =
    case model.pools of
        Loaded ps ->
            ps

        _ ->
            []


poolByIdIn : List Pool -> String -> Maybe Pool
poolByIdIn ps pid =
    List.filter (\p -> p.idBech32 == pid) ps |> List.head


{-| Pool base16 id for the delegation certificate. Prefer Blockfrost's `hex`; fall
back to the provisional Elm bech32 decoder if the pool isn't in the loaded set.
-}
poolHex : Model -> String -> String
poolHex model pid =
    case poolByIdIn (loadedPools model) pid of
        Just p ->
            p.idHex

        Nothing ->
            Bech32.bech32ToHex pid |> Maybe.withDefault pid



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
            { model | tx = Draft, submit = NotSubmitted }


{-| For edits that change the transaction _shape_ (inputs, outputs, certificates,
era). Those also reset the fee: a previously estimated fee would now be wrong
(possibly below the minimum, which the node only rejects at submission).
-}
invalidateShape : Model -> Model
invalidateShape model =
    { model | tx = Draft, submit = NotSubmitted, fee = NoFee, feeText = "" }



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


toggleBook : BookForm -> BookForm
toggleBook b =
    { b | open = not b.open }


setBookAlias : String -> BookForm -> BookForm
setBookAlias s b =
    { b | alias = s }


setBookAddr : String -> BookForm -> BookForm
setBookAddr s b =
    { b | address = s }



-- GENERIC LIST HELPERS


isFailed : Loadable a -> Bool
isFailed l =
    case l of
        Failed _ ->
            True

        _ ->
            False


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
