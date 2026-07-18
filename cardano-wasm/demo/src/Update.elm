module Update exposing (update)

{-| The controller: every Msg in one `update`. Pure state changes call State
helpers; effects go through Wasm (ports).
-}

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
            -- Wallet keys survive a network switch; addresses are re-derived below
            -- (the bech32 encoding is network-specific, the keys are not).
            ( { model | network = n }
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
            , Cmd.none
            )

        GotDerivedAddresses (Err e) ->
            ( log LogWarn ("derive addresses failed: " ++ e) model, Cmd.none )

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
