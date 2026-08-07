module Update exposing (update)

{-| The controller: every Msg in one `update`. Pure state changes call State
helpers; effects go through Wasm (ports).
-}

import Blockfrost
import Dict
import Format
import Net exposing (netName)
import Ports exposing (clipboardWrite)
import Set
import State exposing (..)
import Types exposing (..)
import Wasm


{-| Inspect an address via cardano-wasm unless we already have a verdict for it.
A failed check is not a verdict, so it does not block a retry: re-adding or
re-using the address inspects it again, as the "check failed" badge promises.
-}
inspectIfNew : Model -> String -> Cmd Msg
inspectIfNew model a =
    case Dict.get a model.addrChecks of
        Just CheckFailed ->
            Wasm.inspectAddress a

        Just _ ->
            Cmd.none

        Nothing ->
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
            -- network-specific, the keys are not). Loads pause while the
            -- re-derivation is pending so no request carries a stale address.
            ( { model
                | network = n
                , wallets = List.map (\w -> { w | utxos = NotAsked }) model.wallets
                , deriving = not (List.isEmpty model.wallets)
                , reloading = Set.empty
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
                | deriving = False
                , wallets =
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
            ( log LogWarn ("derive addresses failed: " ++ e) { model | deriving = False }, Cmd.none )

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

                    else if model.deriving then
                        toastNow "Re-deriving addresses — try again in a moment" model

                    else if fetching w model then
                        -- a request is already in flight
                        ( model, Cmd.none )

                    else
                        ( startFetch w model
                            |> log LogInfo ("GET blockfrost /addresses/…/utxos · " ++ w.alias)
                        , Blockfrost.fetchUtxos (currentKey model) model.network wid w.address
                        )

                Nothing ->
                    ( model, Cmd.none )

        ClickLoadAll ->
            if currentKey model == "" then
                toastNow "Enter a Blockfrost project id first" model

            else if model.deriving then
                toastNow "Re-deriving addresses — try again in a moment" model

            else
                -- reload ALL wallets (not just never-loaded ones), skipping in-flight
                -- requests; one predicate decides both the sweep and the commands
                let
                    toLoad =
                        List.filter (\w -> not (fetching w model)) model.wallets
                in
                ( List.foldl startFetch model toLoad
                    |> log LogInfo ("GET blockfrost /addresses/…/utxos × " ++ String.fromInt (List.length toLoad))
                , Cmd.batch (List.map (\w -> Blockfrost.fetchUtxos (currentKey model) model.network w.id w.address) toLoad)
                )

        GotUtxos wid net result ->
            let
                settled =
                    { model | reloading = Set.remove wid model.reloading }
            in
            if net /= model.network then
                -- fired before a network switch; the wallet was already swept
                ( log LogWarn (aliasOf wid settled ++ ": dropped a UTxO response from the previous network") settled
                , Cmd.none
                )

            else
                case result of
                    Ok page ->
                        -- On a reload the previous page is still in place, so ticks are
                        -- kept for UTxOs that still exist.
                        let
                            prevSelected =
                                case getWallet wid settled |> Maybe.map .utxos of
                                    Just (Loaded old) ->
                                        old.utxos |> List.filter .selected |> List.map (\u -> ( u.txId, u.txIx ))

                                    _ ->
                                        []

                            restored =
                                List.map (\u -> { u | selected = List.member ( u.txId, u.txIx ) prevSelected }) page.utxos

                            warnTruncated m =
                                if page.truncated then
                                    log LogWarn (aliasOf wid m ++ " has more UTxOs than one page — the balance is a lower bound") m

                                else
                                    m
                        in
                        ( mapWallet wid (\w -> { w | utxos = Loaded { page | utxos = restored } }) settled
                            |> log LogOk (aliasOf wid settled ++ ": loaded " ++ String.fromInt (List.length restored) ++ " UTxOs")
                            |> warnTruncated
                        , Cmd.none
                        )

                    Err err ->
                        case getWallet wid settled |> Maybe.map .utxos of
                            Just (Loaded _) ->
                                -- a failed reload keeps showing the last known page
                                ( log LogWarn (aliasOf wid settled ++ ": UTxO reload failed (showing last known data): " ++ err) settled
                                , Cmd.none
                                )

                            _ ->
                                ( mapWallet wid (\w -> { w | utxos = Failed err }) settled
                                    |> log LogWarn (aliasOf wid settled ++ ": UTxO load failed: " ++ err)
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
            -- pasted addresses often carry stray whitespace; store them clean so the
            -- verdict cache and the inspection see the address the chain would
            let
                addr =
                    String.trim model.bookForm.address
            in
            if addr == "" then
                toastNow "Enter an address" model

            else
                ( { model
                    | book = model.book ++ [ BookEntry (Format.orDefault "Saved address" model.bookForm.alias) addr ]
                    , bookForm = emptyBookForm
                  }
                    |> log LogInfo "added address to book"
                , inspectIfNew model addr
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
            ( { model | outputs = toggleOutputChange i model.outputs }, Cmd.none )

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
