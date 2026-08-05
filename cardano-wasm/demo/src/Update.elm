module Update exposing (update)

{-| The controller: every Msg in one `update`. Pure state changes call State
helpers; effects go through Wasm (ports).
-}

import Blockfrost
import Net exposing (netName)
import Ports exposing (clipboardWrite)
import State exposing (..)
import Types exposing (..)
import Wasm


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
            , Cmd.none
            )

        GotDerivedAddresses (Err e) ->
            ( log LogWarn ("derive addresses failed: " ++ e) { model | deriving = False }, Cmd.none )

        -- ── wallets: generate / restore / edit / forget ────────────────────────
        ClickNewWallet ->
            ( log LogCmd "CardanoApi.wallet.generateStakeWallet()" model
            , Wasm.generateWallet model.network
            )

        GotGeneratedWallet (Ok p) ->
            ( addWallet p model |> log LogOk "generated wallet", Cmd.none )

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
            , Cmd.none
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

                    else if w.utxos == Loading then
                        -- a request is already in flight
                        ( model, Cmd.none )

                    else
                        ( mapWallet wid (\x -> { x | utxos = Loading }) model
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
                        List.filter (\w -> w.utxos /= Loading) model.wallets

                    loadingIds =
                        List.map .id toLoad
                in
                ( { model
                    | wallets =
                        List.map
                            (\w ->
                                if List.member w.id loadingIds then
                                    { w | utxos = Loading }

                                else
                                    w
                            )
                            model.wallets
                  }
                    |> log LogInfo ("GET blockfrost /addresses/…/utxos × " ++ String.fromInt (List.length toLoad))
                , Cmd.batch (List.map (\w -> Blockfrost.fetchUtxos (currentKey model) model.network w.id w.address) toLoad)
                )

        GotUtxos wid net result ->
            if net /= model.network then
                -- fired before a network switch; the wallet was already swept
                ( log LogWarn (aliasOf wid model ++ ": dropped a UTxO response from the previous network") model
                , Cmd.none
                )

            else
                case result of
                    Ok page ->
                        let
                            warnTruncated m =
                                if page.truncated then
                                    log LogWarn (aliasOf wid m ++ " has more UTxOs than one page — the balance is a lower bound") m

                                else
                                    m
                        in
                        ( mapWallet wid (\w -> { w | utxos = Loaded page }) model
                            |> log LogOk (aliasOf wid model ++ ": loaded " ++ String.fromInt (List.length page.utxos) ++ " UTxOs")
                            |> warnTruncated
                        , Cmd.none
                        )

                    Err err ->
                        ( mapWallet wid (\w -> { w | utxos = Failed err }) model
                            |> log LogWarn (aliasOf wid model ++ ": UTxO load failed: " ++ err)
                        , Cmd.none
                        )

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
