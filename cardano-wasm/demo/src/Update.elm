module Update exposing (update)

{-| The controller: every Msg in one `update`. Pure state changes call State
helpers; effects go through Wasm (ports).
-}

import Blockfrost
import Dict
import Format
import Net exposing (netName)
import Ports exposing (clipboardWrite)
import State exposing (..)
import Types exposing (..)
import Wasm


{-| Inspect an address via cardano-wasm unless we already have a verdict for it.
-}
inspectIfNew : Model -> String -> Cmd Msg
inspectIfNew model a =
    if Dict.member a model.addrChecks then
        Cmd.none

    else
        Wasm.inspectAddress a


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        -- ── network ────────────────────────────────────────────────────────────
        SelectNetwork n ->
            -- Balances are network-specific and swept. Wallet keys survive a network
            -- switch; addresses are re-derived below (the bech32 encoding is
            -- network-specific, the keys are not).
            ( { model
                | network = n
                , wallets = List.map (\w -> { w | utxos = NotAsked }) model.wallets
                , outputs = []
              }
                |> log LogInfo ("switched to " ++ netName n)
            , if List.isEmpty model.wallets then
                Cmd.none

              else
                Wasm.deriveAddresses n model.wallets
            )

        GotDerivedAddresses (Ok pairs) ->
            ( { model
                | wallets =
                    List.map
                        (\w ->
                            case List.filter (\( i, _ ) -> i == w.id) pairs |> List.head of
                                Just ( _, addr ) ->
                                    { w | address = addr }

                                Nothing ->
                                    w
                        )
                        model.wallets
              }
            , Cmd.batch (List.map (\( _, addr ) -> inspectIfNew model addr) pairs)
            )

        GotDerivedAddresses (Err e) ->
            ( log LogWarn ("derive addresses failed: " ++ e) model, Cmd.none )

        -- ── wallets: generate / restore / edit / forget ────────────────────────
        ClickNewWallet ->
            ( log LogCmd "CardanoApi.wallet.generateStakeWallet()" model
            , Wasm.generateWallet model.network
            )

        GotGeneratedWallet (Ok p) ->
            ( addWallet p model |> log LogOk "generated wallet", inspectIfNew model p.address )

        GotGeneratedWallet (Err e) ->
            ( log LogWarn ("generate failed: " ++ e) model, Cmd.none )

        ClickRestoreToggle ->
            ( { model | restore = toggleRestore model.restore }, Cmd.none )

        UpdateRestorePay s ->
            ( { model | restore = setRestorePay s model.restore }, Cmd.none )

        UpdateRestoreStake s ->
            ( { model | restore = setRestoreStake s model.restore }, Cmd.none )

        CancelRestore ->
            ( { model | restore = emptyRestoreForm }, Cmd.none )

        SubmitRestore ->
            ( log LogCmd "CardanoApi.wallet.restoreStakeWalletFromSigningKeyBech32(...)" model
            , Wasm.restoreWallet model.network model.restore
            )

        GotRestoredWallet (Ok p) ->
            ( addWallet p { model | restore = emptyRestoreForm } |> log LogOk "restored wallet"
            , inspectIfNew model p.address
            )

        GotRestoredWallet (Err e) ->
            ( log LogWarn ("restore failed: " ++ e) model, Cmd.none )

        ToggleWalletExpanded wid ->
            ( mapWallet wid (\w -> { w | expanded = not w.expanded }) model, Cmd.none )

        EditAlias wid s ->
            ( mapWallet wid (\w -> { w | alias = s }) model, Cmd.none )

        RequestForget wid ->
            ( { model | modal = ForgetDialog wid }, Cmd.none )

        CancelForget ->
            ( { model | modal = NoModal }, Cmd.none )

        ConfirmForget wid ->
            ( { model
                | wallets = List.filter (\w -> w.id /= wid) model.wallets
                , modal = NoModal
              }
                |> log LogWarn "forgot wallet"
            , Cmd.none
            )

        -- ── UTxOs (Blockfrost reads) ───────────────────────────────────────────
        UpdateBfKey v ->
            ( setCurrentKey v model, Cmd.none )

        ClickLoadUtxos wid ->
            case getWallet wid model of
                Just w ->
                    if currentKey model == "" then
                        toastNow "Enter a Blockfrost project id first" model

                    else if w.utxos == Loading then
                        -- a request is already in flight
                        ( model, Cmd.none )

                    else
                        ( mapWallet wid (\x -> { x | utxos = Loading }) model
                            |> log LogInfo ("GET blockfrost /addresses/…/utxos · " ++ w.alias)
                        , Blockfrost.fetchUtxos model wid w.address
                        )

                Nothing ->
                    ( model, Cmd.none )

        ClickLoadAll ->
            if currentKey model == "" then
                toastNow "Enter a Blockfrost project id first" model

            else
                -- reload ALL wallets (not just never-loaded ones), skipping in-flight requests
                let
                    pending =
                        List.filter (\w -> w.utxos /= Loading) model.wallets
                in
                ( { model
                    | wallets =
                        List.map
                            (\w ->
                                if w.utxos == Loading then
                                    w

                                else
                                    { w | utxos = Loading }
                            )
                            model.wallets
                  }
                , Cmd.batch (List.map (\w -> Blockfrost.fetchUtxos model w.id w.address) pending)
                )

        GotUtxos wid (Ok us) ->
            -- On reload, keep the selection for UTxOs that still exist.
            let
                prevSelected =
                    case getWallet wid model |> Maybe.map .utxos of
                        Just (Loaded old) ->
                            old |> List.filter .selected |> List.map (\u -> ( u.txId, u.txIx ))

                        _ ->
                            []

                restored =
                    List.map (\u -> { u | selected = List.member ( u.txId, u.txIx ) prevSelected }) us

                warnTruncated m =
                    if List.length us >= 100 then
                        log LogWarn "wallet has 100+ UTxOs — only the first page is shown" m

                    else
                        m
            in
            ( mapWallet wid (\w -> { w | utxos = Loaded restored }) model
                |> log LogOk ("loaded " ++ String.fromInt (List.length us) ++ " UTxOs")
                |> warnTruncated
            , Cmd.none
            )

        GotUtxos wid (Err err) ->
            ( mapWallet wid (\w -> { w | utxos = Failed (Blockfrost.httpErrStr err) }) model
                |> log LogWarn ("UTxO load failed: " ++ Blockfrost.httpErrStr err)
            , Cmd.none
            )

        ToggleUtxoSelected wid txId txIx ->
            ( mapWallet wid (toggleUtxo txId txIx) model, Cmd.none )

        -- ── address book ───────────────────────────────────────────────────────
        ClickAddBookToggle ->
            ( { model | bookForm = toggleBook model.bookForm }, Cmd.none )

        UpdateBookAlias s ->
            ( { model | bookForm = setBookAlias s model.bookForm }, Cmd.none )

        UpdateBookAddr s ->
            ( { model | bookForm = setBookAddr s model.bookForm }, Cmd.none )

        CancelBookEntry ->
            ( { model | bookForm = emptyBookForm }, Cmd.none )

        SaveBookEntry ->
            if model.bookForm.address == "" then
                toastNow "Enter an address" model

            else
                ( { model
                    | book = model.book ++ [ BookEntry (Format.orDefault "Saved address" model.bookForm.alias) model.bookForm.address ]
                    , bookForm = emptyBookForm
                  }
                    |> log LogInfo "added address to book"
                , inspectIfNew model model.bookForm.address
                )

        DeleteBookEntry i ->
            ( { model | book = removeAt i model.book }, Cmd.none )

        GotAddressInspected (Ok ( a, verdict )) ->
            ( { model | addrChecks = Dict.insert a verdict model.addrChecks }
                |> (if verdict == CheckInvalid then
                        log LogWarn ("invalid address: " ++ Format.shorten a)

                    else
                        identity
                   )
            , Cmd.none
            )

        GotAddressInspected (Err e) ->
            ( log LogWarn ("address inspection failed: " ++ e) model, Cmd.none )

        -- ── outputs ────────────────────────────────────────────────────────────
        UseBookAddress alias addr ->
            ( { model | outputs = model.outputs ++ [ Output addr alias (Lovelace "") ] }
            , inspectIfNew model addr
            )

        UpdateOutputAmount i s ->
            ( { model | outputs = updateAt i (\o -> { o | amount = Lovelace s }) model.outputs }, Cmd.none )

        ToggleOutputChange i ->
            -- at most one change output: marking one unmarks any other
            ( { model
                | outputs =
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
                        model.outputs
              }
            , Cmd.none
            )

        DeleteOutput i ->
            ( { model | outputs = removeAt i model.outputs }, Cmd.none )

        -- ── clearing ───────────────────────────────────────────────────────────
        ClearInputs ->
            ( deselectInputs model, Cmd.none )

        ClearOutputs ->
            ( { model | outputs = [] }, Cmd.none )

        -- ── misc ───────────────────────────────────────────────────────────────
        Copy t ->
            let
                ( m, c ) =
                    toastNow "Copied" model
            in
            ( m, Cmd.batch [ c, clipboardWrite t ] )

        ClearConsole ->
            ( { model | console = [] }, Cmd.none )

        ClearToast n ->
            ( if n == model.toastSeq then
                { model | toast = Nothing }

              else
                model
            , Cmd.none
            )
