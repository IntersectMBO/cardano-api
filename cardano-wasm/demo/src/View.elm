module View exposing (view)

{-| The UI: wallets column · (builder placeholder) · console column, the forget
dialog and the toast. Pure Model → Html.
-}

import Format exposing (ada, adaToLovelace, amountError, lovelaceToAda, shorten)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, stopPropagationOn)
import Json.Decode as D
import Net exposing (expectedNetKind, faucetUrl, netMagic, netName, netTag)
import State exposing (..)
import Types exposing (..)


view : Model -> Html Msg
view model =
    div []
        [ div [ class "mock" ] [ text "◆ DEMO — keys, balances, fees and transactions are real cardano-wasm calls on the selected network. Use TESTNETS only." ]
        , viewTopbar model
        , div [ class "grid" ]
            [ div [ class "col" ] [ viewDataSource model, viewWalletsCard model, viewBookCard model ]
            , div [ class "col" ] [ viewBuilder model ]
            , div [ class "col" ] [ viewConsole model ]
            ]
        , viewModal model
        , viewToast model
        ]


viewTopbar : Model -> Html Msg
viewTopbar model =
    div [ class "topbar" ]
        [ div [ class "brand" ] [ span [ class "b" ] [ text "◆" ], text " cardano-wasm ", span [ class "muted" ] [ text "demo · multi-wallet" ] ]
        , div [ class "nettabs" ]
            (List.map
                (\n ->
                    button [ classList [ ( "on", model.network == n ) ], onClick (SelectNetwork n) ] [ text (netName n) ]
                )
                [ Mainnet, Preprod, Preview ]
            )
        , span [ class "magic" ] [ text ("magic " ++ netMagic model.network) ]
        ]


viewDataSource : Model -> Html Msg
viewDataSource model =
    div [ class "card" ]
        [ h3 [] [ text "Data source · Blockfrost" ]
        , label [] [ text ("project id (" ++ netName model.network ++ ")") ]
        , input [ type_ "password", placeholder (netTag model.network ++ "…"), value (currentKey model), onInput UpdateBfKey ] []
        , div [ class "muted small", style "margin-top" "6px" ]
            [ text "Free key per network at "
            , a [ href "https://blockfrost.io", target "_blank" ] [ text "blockfrost.io" ]
            , text " · kept in memory only, never bundled."
            ]
        ]


viewWalletsCard : Model -> Html Msg
viewWalletsCard model =
    div [ class "card" ]
        [ h3 []
            [ text "Wallets"
            , span [ class "hrow" ]
                [ button [ class "btn xs", onClick ClickNewWallet ] [ text "+ New" ]
                , button [ class "btn ghost xs", onClick ClickRestoreToggle ] [ text "Restore" ]
                ]
            ]
        , if model.restore.open then
            div [ class "restore" ]
                [ input [ placeholder "payment signing key (bech32)", value model.restore.paymentSkey, onInput UpdateRestorePay ] []
                , input [ placeholder "stake signing key (bech32)", value model.restore.stakeSkey, onInput UpdateRestoreStake, style "margin-top" "6px" ] []
                , div [ class "hrow", style "margin-top" "6px" ]
                    [ button [ class "btn sm", style "flex" "1", onClick SubmitRestore ] [ text "Restore wallet" ]
                    , button [ class "btn ghost sm", onClick CancelRestore ] [ text "Cancel" ]
                    ]
                ]

          else
            text ""
        , if List.isEmpty model.wallets then
            div [ class "empty" ] [ text "No wallets yet — click + New" ]

          else
            div [] (List.map (viewWallet model) model.wallets)
        , if List.isEmpty model.wallets then
            text ""

          else
            button [ class "btn ghost sm block", onClick ClickLoadAll ] [ text "↻ Load all balances (Blockfrost)" ]
        , case faucetUrl model.network of
            Just url ->
                if List.isEmpty model.wallets then
                    text ""

                else
                    a [ href url, target "_blank", class "faucet" ] [ button [ class "btn ghost sm block" ] [ text "🚰 Faucet" ] ]

            Nothing ->
                text ""
        ]


viewWallet : Model -> Wallet -> Html Msg
viewWallet model w =
    div [ classList [ ( "wallet", True ), ( "open", w.expanded ) ] ]
        [ div [ class "whead", onClick (ToggleWalletExpanded w.id) ]
            [ span [ class "wchev" ] [ text "▶" ]
            , span [ class "wav", style "background" w.color ] [ text (String.left 1 w.alias |> String.toUpper) ]
            , span [ class "winfo" ]
                [ input [ class "walias", value w.alias, stopClick, onInput (EditAlias w.id) ] []
                , span [ class "waddr mono" ] [ text (shorten w.address) ]
                ]
            , span [ class "wbal" ] [ text (walletBalance w |> Maybe.map ada |> Maybe.withDefault "—") ]
            ]
        , if w.expanded then
            div [ class "wbody" ]
                [ div [ class "kv" ]
                    [ span [ class "k" ] [ text "Address" ]
                    , span [ class "v mono" ]
                        [ text (shorten w.address)
                        , text " "
                        , button [ class "btn ghost xs", onClick (Copy w.address) ] [ text "copy" ]
                        ]
                    ]
                , details []
                    [ summary [] [ text "keys & hashes (signing keys are your backup)" ]
                    , kv "pay vkey" (shorten w.keys.paymentVKey)
                    , kvSecret "pay skey" w.keys.paymentSKey
                    , kv "stake vkey" (shorten w.keys.stakeVKey)
                    , kvSecret "stake skey" w.keys.stakeSKey
                    , kv "pay keyhash" (shorten w.keys.paymentKeyHash)
                    , kv "stake keyhash" (shorten w.keys.stakeKeyHash)
                    ]
                , div [ class "hrow", style "margin-top" "8px" ]
                    [ button [ class "btn ghost xs", onClick (ClickLoadUtxos w.id) ]
                        [ text
                            (if w.utxos == NotAsked then
                                "↻ load UTxOs"

                             else
                                "↻ reload UTxOs"
                            )
                        ]
                    , button [ class "btn xs danger", onClick (RequestForget w.id) ] [ text "🗑 forget" ]
                    ]
                , viewWalletUtxos w
                ]

          else
            text ""
        ]


viewWalletUtxos : Wallet -> Html Msg
viewWalletUtxos w =
    case w.utxos of
        NotAsked ->
            div [ class "empty" ] [ text "UTxOs not loaded" ]

        Loading ->
            div [ class "empty" ] [ text "loading…" ]

        Failed e ->
            div [ class "empty" ] [ text ("failed: " ++ e) ]

        Loaded [] ->
            div [ class "empty" ] [ text "no UTxOs at this address" ]

        Loaded us ->
            table []
                (tr [] [ th [] [], th [] [ text "txid#ix" ], th [ class "right" ] [ text "₳" ] ]
                    :: List.map
                        (\u ->
                            tr [ classList [ ( "sel", u.selected ) ] ]
                                [ td [] [ input [ type_ "checkbox", checked u.selected, disabled u.hasAssets, onClick (ToggleUtxoSelected w.id u.txId u.txIx) ] [] ]
                                , td [ class "mono" ]
                                    [ text (String.left 8 u.txId ++ "…#" ++ String.fromInt u.txIx)
                                    , if u.hasAssets then
                                        span [ class "pill warn", title "carries native tokens — can't be spent in this ADA-only demo" ] [ text "tokens" ]

                                      else
                                        text ""
                                    ]
                                , td [ class "right" ] [ text (lovelaceToAda u.lovelace) ]
                                ]
                        )
                        us
                )


viewBookCard : Model -> Html Msg
viewBookCard model =
    let
        ownEntries =
            ownBook model |> List.map (\b -> ( b, True ))

        extEntries =
            List.indexedMap (\i b -> ( b, i )) model.book
    in
    div [ class "card" ]
        [ h3 [] [ text "Address book", span [ class "btn xs", onClick ClickAddBookToggle ] [ text "+ Add" ] ]
        , if model.bookForm.open then
            div [ class "restore" ]
                [ input [ placeholder "label (e.g. Exchange)", value model.bookForm.alias, onInput UpdateBookAlias ] []
                , input [ placeholder "addr…", value model.bookForm.address, onInput UpdateBookAddr, style "margin-top" "6px" ] []
                , div [ class "hrow", style "margin-top" "6px" ]
                    [ button [ class "btn sm", style "flex" "1", onClick SaveBookEntry ] [ text "Save to book" ]
                    , button [ class "btn ghost sm", onClick CancelBookEntry ] [ text "Cancel" ]
                    ]
                ]

          else
            text ""
        , if List.isEmpty ownEntries && List.isEmpty extEntries then
            div [ class "empty" ] [ text "Own wallets appear here automatically" ]

          else
            div []
                (List.map (\( b, _ ) -> viewBookRow model b Nothing) ownEntries
                    ++ List.map (\( b, i ) -> viewBookRow model b (Just i)) extEntries
                )
        ]


{-| One address-book row. `deletableAt` is Nothing for own wallets (derived rows,
not deletable) and Just the index into model.book for manual entries.
-}
viewBookRow : Model -> BookEntry -> Maybe Int -> Html Msg
viewBookRow model b deletableAt =
    let
        isInvalid =
            addrVerdict model b.address == Just CheckInvalid
    in
    div [ class "bookrow" ]
        [ span [ class "nm" ] [ text b.alias ]
        , case deletableAt of
            Nothing ->
                span [ class "pill own" ] [ text "own" ]

            Just _ ->
                viewAddrBadge model b.address
        , span [ class "ad mono" ] [ text (shorten b.address) ]
        , button [ class "btn ghost xs", disabled isInvalid, onClick (UseBookAddress b.alias b.address) ] [ text "use" ]
        , case deletableAt of
            Nothing ->
                text ""

            Just i ->
                button [ class "btn xs danger", onClick (DeleteBookEntry i) ] [ text "×" ]
        ]


{-| Verdict badge for a non-own address: invalid, or its network when it differs from the
selected one. Valid + matching needs no badge. (Preprod/preview can't be told apart.)
-}
viewAddrBadge : Model -> String -> Html Msg
viewAddrBadge model a =
    case addrVerdict model a of
        Nothing ->
            text ""

        Just CheckInvalid ->
            span [ class "pill bad" ] [ text "invalid ✗" ]

        Just (CheckValid kind) ->
            if kind == expectedNetKind model.network then
                text ""

            else
                span [ class "pill warn" ] [ text "wrong net" ]


viewBuilder : Model -> Html Msg
viewBuilder model =
    div [ class "card" ]
        [ h3 [] [ text "Transaction builder" ]
        , sectionLabel "Inputs — tick UTxOs inside a wallet panel" (not (List.isEmpty (selectedInputs model))) ClearInputs
        , viewInputs model
        , sectionLabel "Outputs — add recipients from the address book" (not (List.isEmpty model.outputs)) ClearOutputs
        , viewOutputs model
        ]


viewInputs : Model -> Html Msg
viewInputs model =
    let
        ins =
            selectedInputs model
    in
    if List.isEmpty ins then
        div [ class "empty" ] [ text "No inputs yet — tick UTxOs in a wallet panel" ]

    else
        div []
            (List.map
                (\( w, u ) ->
                    div [ class "inrow" ]
                        [ span [ class "mono grow" ] [ text (u.txId ++ "#" ++ String.fromInt u.txIx) ]
                        , span [ class "muted nowrap" ] [ text w.alias ]
                        , span [ class "nowrap" ] [ text (ada u.lovelace) ]
                        ]
                )
                ins
            )


viewOutputs : Model -> Html Msg
viewOutputs model =
    if List.isEmpty model.outputs then
        div [ class "empty" ] [ text "No outputs yet — pick an address with “use” in the address book" ]

    else
        let
            hasInvalid =
                List.any
                    (\o ->
                        case o.amount of
                            Lovelace s ->
                                amountError s

                            Change ->
                                False
                    )
                    model.outputs

            addrProblems =
                model.outputs |> List.filterMap (\o -> addrFlagged model o.address)

            hint =
                (if hasInvalid then
                    [ div [ class "small", style "color" "#ff6b6b", style "margin-top" "4px" ]
                        [ text "⚠ invalid amount — use “.” as the decimal separator" ]
                    ]

                 else
                    []
                )
                    ++ (case addrProblems of
                            [] ->
                                []

                            p :: _ ->
                                [ div [ class "small", style "color" "#ff6b6b", style "margin-top" "4px" ]
                                    [ text ("⚠ " ++ p ++ " — fix or remove the flagged recipient") ]
                                ]
                       )

            row i o =
                div [ class "outrow" ]
                    [ div [ classList [ ( "orec", True ), ( "chg", o.amount == Change ), ( "badaddr", addrFlagged model o.address /= Nothing ) ] ]
                        [ b [] [ text o.alias ], span [ class "mono" ] [ text o.address ] ]
                    , case o.amount of
                        Change ->
                            div [ class "orec chg center" ] [ b [] [ text "change" ], span [] [ text "remaining" ] ]

                        Lovelace s ->
                            input [ classList [ ( "amt", True ), ( "bad", amountError s ) ], placeholder "₳", value s, onInput (UpdateOutputAmount i) ] []
                    , button [ classList [ ( "chgbtn", True ), ( "on", o.amount == Change ) ], title "use as change", onClick (ToggleOutputChange i) ]
                        [ text
                            (if o.amount == Change then
                                "✓ change"

                             else
                                "as change"
                            )
                        ]
                    , button [ class "x", onClick (DeleteOutput i) ] [ text "×" ]
                    ]
        in
        div [] (List.indexedMap row model.outputs ++ hint)


sectionLabel : String -> Bool -> Msg -> Html Msg
sectionLabel txt canClear clearMsg =
    div [ class "seclabel" ]
        [ label [] [ text txt ]
        , if canClear then
            button [ class "btn ghost xs", onClick clearMsg ] [ text "clear" ]

          else
            text ""
        ]


viewConsole : Model -> Html Msg
viewConsole model =
    div [ class "card" ]
        [ h3 [] [ text "Console", button [ class "btn ghost xs", onClick ClearConsole ] [ text "clear" ] ]

        -- newest rendered first + CSS column-reverse = chronological order with the
        -- scroll position pinned to the latest line (no scroll commands needed)
        , div [ class "console" ]
            (List.map
                (\l ->
                    div [ class ("ln " ++ logClass l.level) ] [ text (logPrefix l.level ++ " " ++ l.text) ]
                )
                (List.reverse model.console)
            )
        ]


logClass : LogLevel -> String
logClass l =
    case l of
        LogInfo ->
            "info"

        LogOk ->
            "ok"

        LogWarn ->
            "warn"

        LogCmd ->
            "cmd"


logPrefix : LogLevel -> String
logPrefix l =
    case l of
        LogInfo ->
            "→"

        LogOk ->
            "✓"

        LogWarn ->
            "!"

        LogCmd ->
            "$"


viewModal : Model -> Html Msg
viewModal model =
    case model.modal of
        NoModal ->
            text ""

        ForgetDialog wid ->
            div [ class "modal-bg" ]
                [ div [ class "modal sm" ]
                    [ div [ class "mh" ] [ h3 [] [ text ("Forget " ++ aliasOf wid model ++ "?") ] ]
                    , div [ class "mb" ]
                        [ p [] [ text "Keys are not stored — be sure the signing keys are saved." ]
                        , div [ class "hrow" ]
                            [ button [ class "btn danger", onClick (ConfirmForget wid) ] [ text "Forget" ]
                            , button [ class "btn ghost", onClick CancelForget ] [ text "Cancel" ]
                            ]
                        ]
                    ]
                ]


viewToast : Model -> Html Msg
viewToast model =
    case model.toast of
        Just t ->
            div [ class "toast on" ] [ text t ]

        Nothing ->
            div [ class "toast" ] []



-- view helpers


kv : String -> String -> Html Msg
kv k v =
    div [ class "kv" ] [ span [ class "k" ] [ text k ], span [ class "v" ] [ text v ] ]


kvSecret : String -> String -> Html Msg
kvSecret k v =
    div [ class "kv" ] [ span [ class "k" ] [ text k ], span [ class "v mono secret" ] [ text v ] ]


stopClick : Attribute Msg
stopClick =
    stopPropagationOn "click" (D.succeed ( NoOp, True ))
