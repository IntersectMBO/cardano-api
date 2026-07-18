module View exposing (view)

{-| The entire UI: three columns (wallets + address book · transaction builder ·
inspector + console), the pool/forget modals and the toast. Pure Model → Html.
-}

import Format exposing (ada, adaToLovelace, amountError, lovelaceToAda, shorten)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onInput, stopPropagationOn, targetValue)
import Json.Decode as D
import Net exposing (cliFlag, eraTag, expectedNetKind, explorerTx, faucetUrl, netMagic, netName, netTag)
import State exposing (..)
import Types exposing (..)


onChange : (String -> msg) -> Attribute msg
onChange tagger =
    on "change" (D.map tagger targetValue)


view : Model -> Html Msg
view model =
    div []
        [ div [ class "mock" ] [ text "◆ DEMO — keys, balances, fees and transactions are real cardano-wasm calls on the selected network. Use TESTNETS only." ]
        , viewTopbar model
        , div [ class "grid" ]
            [ div [ class "col" ] [ viewDataSource model, viewWalletsCard model, viewBookCard model ]
            , div [ class "col" ] [ viewBuilder model ]
            , div [ class "col" ] [ viewInspector model, viewConsole model ]
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

                -- bound to the wallet's current certificate; options come from
                -- State.certMenu (the same codes the update parses)
                , let
                    cur =
                        walletCertAction w.id model
                  in
                  select [ class "certsel", onChange (SetWalletCert w.id) ]
                    (List.map
                        (\( code, lbl ) -> option [ value code, selected (code == cur) ] [ text lbl ])
                        certMenu
                    )
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
        [ h3 []
            [ text "Transaction builder"
            , span [ class "hrow" ]
                [ button [ class "btn ghost xs", onClick ClearTx ] [ text "clear tx" ]
                , span [ class "seg" ]
                    [ button [ classList [ ( "on", model.era == Conway ) ], onClick (SelectEra Conway) ] [ text "Conway" ]
                    , button [ classList [ ( "on", model.era == Dijkstra ) ], onClick (SelectEra Dijkstra) ] [ text "Dijkstra" ]
                    ]
                ]
            ]
        , sectionLabel "Inputs — tick UTxOs inside a wallet panel" (not (List.isEmpty (selectedInputs model))) ClearInputs
        , viewInputs model
        , sectionLabel "Outputs — add recipients from the address book" (not (List.isEmpty model.outputs)) ClearOutputs
        , viewOutputs model
        , sectionLabel "Certificates — pick one in a wallet’s certificate menu" (not (List.isEmpty model.certs)) ClearCerts
        , viewCerts model
        , viewSummary model
        , div [ class "hrow", style "margin-top" "12px" ]
            [ button [ class "btn ghost", disabled (not (txReady model)), onClick ClickEstimateFee ] [ text "⚙ estimateMinFee()" ]
            , input [ class "feeinput", placeholder "or set fee (lovelace)", value model.feeText, onInput UpdateFeeText ] []
            ]
        , div [ class "hrow", style "margin-top" "12px" ]
            [ button [ class "btn", disabled (not (canSign model)), onClick ClickSign ]
                [ text
                    (if model.tx == Signing then
                        "signing…"

                     else
                        "✍ sign"
                    )
                ]
            ]
        , label [ style "margin-top" "16px" ] [ text "Signed transaction" ]
        , viewExport model
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


viewCerts : Model -> Html Msg
viewCerts model =
    if List.isEmpty model.certs then
        div [ class "empty" ] [ text "No certificates yet — pick one in a wallet’s certificate menu" ]

    else
        div []
            (List.indexedMap
                (\i c ->
                    let
                        wAlias =
                            aliasOf c.wallet model

                        ( label_, poolMaybe ) =
                            certLabel (loadedPools model) c.action
                    in
                    div [ class "certrow" ]
                        [ span [ class "wav small", style "background" (getWallet c.wallet model |> Maybe.map .color |> Maybe.withDefault "#555") ]
                            [ text (String.left 1 wAlias |> String.toUpper) ]
                        , div [ class "grow" ]
                            [ b [] [ text wAlias ]
                            , div [ class "muted small" ] [ text label_ ]
                            ]
                        , case poolMaybe of
                            Just _ ->
                                button [ class "btn ghost xs", onClick (ChangeCertPool i) ] [ text "pool" ]

                            Nothing ->
                                text ""
                        , button [ class "x", onClick (DeleteCertificate i) ] [ text "×" ]
                        ]
                )
                model.certs
            )


certLabel : List Pool -> CertAction -> ( String, Maybe String )
certLabel ps action =
    let
        poolName pid =
            case poolByIdIn ps pid of
                Just p ->
                    shorten p.idBech32

                Nothing ->
                    shorten pid
    in
    case action of
        Register ->
            ( "Register", Nothing )

        Unregister ->
            ( "Unregister", Nothing )

        DelegateOnly pid ->
            ( "Delegate only → " ++ poolName pid, Just pid )

        RegisterAndDelegate pid ->
            ( "Register + delegate → " ++ poolName pid, Just pid )


viewSummary : Model -> Html Msg
viewSummary model =
    let
        dep =
            depositTotal model

        ( changeLabel, changeText ) =
            case ( model.fee, balance model ) of
                ( FeeSet _, Balanced ch ) ->
                    ( changeLabelText model, ada ch )

                ( FeeSet _, Insufficient short ) ->
                    ( "⚠ insufficient funds", "short " ++ ada short )

                ( FeeSet _, DustChange ch _ ) ->
                    ( "⚠ change below min-UTxO", ada ch )

                _ ->
                    ( changeLabelText model, "—" )
    in
    div [ class "summary" ]
        [ kv "Input total" (ada (inputsTotal model))
        , kv "Output total" (ada (explicitOutputsTotal model))
        , kv "Deposit" (ada dep)
        , kv "Fee" (feeDisplay model)
        , kv changeLabel changeText
        , kv "Witnesses" (witnessSummary model)
        ]


changeLabelText : Model -> String
changeLabelText model =
    case changeRow model of
        Just o ->
            "Change → "
                ++ (if o.alias == "" then
                        o.address

                    else
                        o.alias
                   )

        Nothing ->
            "Change → first input wallet"


feeDisplay : Model -> String
feeDisplay model =
    case model.fee of
        NoFee ->
            "— not set —"

        EstimatingFee ->
            "estimating…"

        FeeSet n ->
            ada n


witnessSummary : Model -> String
witnessSummary model =
    let
        pays =
            paymentWalletIds model |> List.map (\wid -> aliasOf wid model ++ " (payment)")

        stakes =
            stakeWalletIds model |> List.map (\wid -> aliasOf wid model ++ " (stake)")

        parts =
            pays ++ stakes
    in
    if List.isEmpty parts then
        "—"

    else
        String.join ", " parts ++ " · " ++ String.fromInt (witnessCount model)


viewExport : Model -> Html Msg
viewExport model =
    case model.tx of
        Signed s ->
            div []
                [ div [ class "small", style "margin-bottom" "8px" ]
                    [ span [ class "muted" ] [ text "txid " ]
                    , span [ class "mono", style "word-break" "break-all" ] [ text s.txId ]
                    , text " "
                    , button [ class "btn ghost xs", onClick (Copy s.txId) ] [ text "copy" ]
                    , text " "
                    , a [ href (explorerTx model.network ++ "transaction/" ++ s.txId), target "_blank" ] [ text "↗ explorer" ]
                    ]
                , div [ class "hrow" ]
                    [ button [ class "btn", onClick ClickSubmit, disabled (model.submit == Submitting) ]
                        [ text
                            (if model.submit == Submitting then
                                "submitting…"

                             else
                                "⇪ submit via Blockfrost"
                            )
                        ]
                    , button [ class "btn good", onClick ClickDownloadCli ] [ text "⤓ download cardano-cli tx file" ]
                    , button [ class "btn ghost sm", onClick (Copy s.cbor) ] [ text "copy CBOR" ]
                    ]
                , viewSubmitStatus model
                , div [ class "clihint mono" ] [ text ("or broadcast yourself: cardano-cli conway transaction submit --tx-file tx.signed " ++ cliFlag model.network) ]
                , div [ class "muted small", style "margin-top" "6px" ]
                    [ text "the explorer link resolves once the transaction is on-chain" ]
                ]

        _ ->
            div [ class "empty" ] [ text "sign the transaction to enable submit / export" ]


viewSubmitStatus : Model -> Html Msg
viewSubmitStatus model =
    case model.submit of
        NotSubmitted ->
            text ""

        Submitting ->
            div [ class "muted small", style "margin-top" "6px" ] [ text "submitting to Blockfrost…" ]

        Submitted txid ->
            div [ class "small", style "margin-top" "6px" ]
                [ span [ class "pill signed" ] [ text "on-chain" ]
                , text " txid "
                , span [ class "mono" ] [ text txid ]
                , text " "
                , a [ href (explorerTx model.network ++ "transaction/" ++ txid), target "_blank" ] [ text "↗ explorer" ]
                ]

        SubmitFailed e ->
            div [ class "small", style "margin-top" "6px", style "color" "#ff6b6b" ] [ text ("submit failed: " ++ e) ]


viewInspector : Model -> Html Msg
viewInspector model =
    div [ class "card" ]
        [ h3 [] [ text "Live tx inspector", span [ classList [ ( "pill", True ), ( "signed", isSigned model ) ] ] [ text (txStateLabel model) ] ]
        , pre [ class "inspector" ] [ text (inspectorText model) ]
        ]


isSigned : Model -> Bool
isSigned model =
    case model.tx of
        Signed _ ->
            True

        _ ->
            False


txStateLabel : Model -> String
txStateLabel model =
    case model.tx of
        Draft ->
            "draft"

        Signing ->
            "signing"

        Signed _ ->
            "signed"


inspectorText : Model -> String
inspectorText model =
    let
        ins =
            selectedInputs model
                |> List.map (\( w, u ) -> "    \"" ++ u.txId ++ "#" ++ String.fromInt u.txIx ++ "\"  // " ++ w.alias)
                |> String.join ",\n"

        outs =
            model.outputs
                |> List.filterMap
                    (\o ->
                        case o.amount of
                            Change ->
                                Just ("    { addr: \"" ++ o.address ++ "\", lovelace: \"<remaining>\" }")

                            Lovelace s ->
                                if s == "" then
                                    Nothing

                                else
                                    Just
                                        ("    { addr: \""
                                            ++ o.address
                                            ++ "\", lovelace: "
                                            ++ (adaToLovelace s |> Maybe.map String.fromInt |> Maybe.withDefault "\"<invalid>\"")
                                            ++ " }"
                                        )
                    )
                |> String.join ",\n"

        certs =
            model.certs
                |> List.map
                    (\c ->
                        let
                            ( lbl, _ ) =
                                certLabel (loadedPools model) c.action
                        in
                        "    { stakeKey: \"" ++ aliasOf c.wallet model ++ "\", action: \"" ++ lbl ++ "\" }"
                    )
                |> String.join ",\n"
    in
    "{\n  era: \""
        ++ eraTag model.era
        ++ "\",\n  inputs: [\n"
        ++ ins
        ++ "\n  ],\n  outputs: [\n"
        ++ outs
        ++ "\n  ],\n  certs: [\n"
        ++ certs
        ++ "\n  ],\n  fee: "
        ++ feeDisplay model
        ++ ",\n  requiredWitnesses: "
        ++ String.fromInt (witnessCount model)
        ++ "\n}"


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

        PoolPicker _ query ->
            div [ class "modal-bg" ]
                [ div [ class "modal" ]
                    [ div [ class "mh" ]
                        [ h3 [] [ text "Choose a stake pool" ]
                        , input [ class "poolsearch", placeholder "search ticker or id…", value query, onInput UpdatePoolSearch ] []
                        , button [ class "btn ghost x", onClick ClosePoolModal ] [ text "✕" ]
                        ]
                    , div [ class "mb" ] (viewPoolList model query)
                    ]
                ]


viewPoolList : Model -> String -> List (Html Msg)
viewPoolList model query =
    case model.pools of
        NotAsked ->
            [ div [ class "empty" ]
                [ text "pools not loaded — enter a Blockfrost project id, then "
                , button [ class "btn ghost xs", onClick ClickLoadPools ] [ text "load pools" ]
                ]
            ]

        Loading ->
            [ div [ class "empty" ] [ text "loading pools from Blockfrost…" ] ]

        Failed e ->
            [ div [ class "empty" ]
                [ text ("failed: " ++ e ++ " ")
                , button [ class "btn ghost xs", onClick (ClickPoolPage model.poolPage) ] [ text "retry" ]
                ]
            ]

        Loaded ps ->
            let
                ql =
                    String.toLower query

                matches =
                    List.filter (\p -> String.contains ql (String.toLower p.idBech32)) ps

                cards =
                    if List.isEmpty matches then
                        [ div [ class "empty" ] [ text "no pools match (search covers this page only)" ] ]

                    else
                        List.map viewPoolCard matches
            in
            cards ++ [ viewPoolPager model (List.length ps) ]


{-| Prev/next pager. A full page (100) means there is probably a next one; a short
page is the end of the list. Pages are only ever fetched on these clicks.
-}
viewPoolPager : Model -> Int -> Html Msg
viewPoolPager model pageSize =
    div [ class "empty" ]
        [ button
            [ class "btn ghost xs", disabled (model.poolPage <= 1), onClick (ClickPoolPage (model.poolPage - 1)) ]
            [ text "◂ prev" ]
        , text (" page " ++ String.fromInt model.poolPage ++ " · " ++ String.fromInt pageSize ++ " pools ")
        , button
            [ class "btn ghost xs", disabled (pageSize < 100), onClick (ClickPoolPage (model.poolPage + 1)) ]
            [ text "next ▸" ]
        ]


viewPoolCard : Pool -> Html Msg
viewPoolCard p =
    div [ class "poolcard", onClick (PickPool p.idBech32) ]
        [ span [ class "tk" ] [ text "◆" ]
        , div [ class "pm grow" ]
            [ b [ class "mono" ] [ text (shorten p.idBech32) ]
            , div [ class "d mono" ] [ text ("hex " ++ String.left 16 p.idHex ++ "…") ]
            ]
        , div [ class "stats" ]
            [ statBox "live ₳" (lovelaceToAda p.liveStake)
            , statBox "sat" (String.fromInt (round (p.saturation * 100)) ++ "%")
            ]
        ]


statBox : String -> String -> Html Msg
statBox lbl v =
    div [] [ span [] [ text lbl ], text v ]


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
    Html.Events.stopPropagationOn "click" (D.succeed ( NoOp, True ))
