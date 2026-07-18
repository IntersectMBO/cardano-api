module Update exposing (update)

{-| The controller: every Msg in one `update`. Pure state changes call State
helpers; effects go through Wasm (ports), Blockfrost (HTTP), or File.Download.
-}

import Blockfrost
import Dict
import File.Download as Download
import Format
import Json.Encode as E
import Net exposing (cliType, netName)
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


{-| Open the pool picker; fetch the pool list (once) if we have a key and none yet.
-}
openPool : PoolPurpose -> Model -> ( Model, Cmd Msg )
openPool purpose model =
    let
        shouldFetch =
            currentKey model /= "" && (model.pools == NotAsked || isFailed model.pools)
    in
    ( { model
        | modal = PoolPicker purpose ""
        , pools =
            if shouldFetch then
                Loading

            else
                model.pools
        , poolPage =
            if shouldFetch then
                1

            else
                model.poolPage
      }
    , if shouldFetch then
        Blockfrost.fetchPools (currentKey model) model.network 1

      else
        Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        -- ── network ────────────────────────────────────────────────────────────
        SelectNetwork n ->
            -- Everything network-specific is swept: balances, tx draft, fee, and the
            -- pool list (pools differ per network!). Wallet keys survive; addresses
            -- are re-derived below. addrChecks survive too (an address's network kind
            -- is intrinsic to the address, not to the selected network).
            let
                swept =
                    { model
                        | network = n
                        , wallets = List.map (\w -> { w | utxos = NotAsked }) model.wallets
                        , outputs = []
                        , certs = []
                        , modal = NoModal
                        , pools = NotAsked
                        , poolPage = 1

                        -- loads pause until the new network's addresses arrive
                        , deriving = not (List.isEmpty model.wallets)
                        , reloading = Set.empty
                    }
                        |> invalidateShape
                        |> log LogInfo ("switched to " ++ netName n ++ " — cleared inputs, outputs, certs")
            in
            ( swept
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

        UpdateBfKey v ->
            ( setCurrentKey v model, Cmd.none )

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
                , certs = List.filter (\c -> c.wallet /= wid) model.certs
                , modal = NoModal
              }
                |> invalidateShape
                |> log LogWarn "forgot wallet"
            , Cmd.none
            )

        -- ── UTxOs (Blockfrost reads) ───────────────────────────────────────────
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
                        -- kept for UTxOs that still exist. If a ticked UTxO disappeared
                        -- (it was spent), the tx shape changed under us — invalidate.
                        let
                            prevSelected =
                                case getWallet wid settled |> Maybe.map .utxos of
                                    Just (Loaded old) ->
                                        old.utxos |> List.filter .selected |> List.map (\u -> ( u.txId, u.txIx ))

                                    _ ->
                                        []

                            restored =
                                List.map (\u -> { u | selected = List.member ( u.txId, u.txIx ) prevSelected }) page.utxos

                            lostSelection =
                                List.length (List.filter .selected restored) /= List.length prevSelected

                            warnTruncated m =
                                if page.truncated then
                                    log LogWarn (aliasOf wid m ++ " has more UTxOs than one page — the balance is a lower bound") m

                                else
                                    m
                        in
                        ( mapWallet wid (\w -> { w | utxos = Loaded { page | utxos = restored } }) settled
                            |> (if lostSelection then
                                    invalidateShape

                                else
                                    identity
                               )
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
            ( mapWallet wid (toggleUtxo txId txIx) model |> invalidateShape, Cmd.none )

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
            ( { model | outputs = model.outputs ++ [ Output addr alias (Lovelace "") ] } |> invalidateShape
            , inspectIfNew model addr
            )

        UpdateOutputAmount i s ->
            ( { model | outputs = updateAt i (\o -> { o | amount = Lovelace s }) model.outputs } |> invalidateShape, Cmd.none )

        ToggleOutputChange i ->
            ( { model | outputs = toggleOutputChange i model.outputs }
                |> invalidateShape
            , Cmd.none
            )

        DeleteOutput i ->
            ( { model | outputs = removeAt i model.outputs } |> invalidateShape, Cmd.none )

        -- ── certificates ───────────────────────────────────────────────────────
        SetWalletCert wid raw ->
            -- The select is bound to the wallet's current certificate: changing it
            -- replaces (or clears) that wallet's cert; "" = no certificate.
            let
                cleared =
                    { model | certs = List.filter (\c -> c.wallet /= wid) model.certs } |> invalidateShape
            in
            case raw of
                "reg" ->
                    ( addCert (Certificate wid Register) cleared |> log LogCmd (aliasOf wid model ++ ": register cert"), Cmd.none )

                "unreg" ->
                    ( addCert (Certificate wid Unregister) cleared |> log LogCmd (aliasOf wid model ++ ": unregister cert"), Cmd.none )

                "deleg" ->
                    openPool (ForNewCert wid RegThenDeleg) cleared

                "delegonly" ->
                    openPool (ForNewCert wid DelegOnly) cleared

                _ ->
                    ( cleared, Cmd.none )

        DeleteCertificate i ->
            ( { model | certs = removeAt i model.certs } |> invalidateShape, Cmd.none )

        ChangeCertPool i ->
            openPool (ForEditCert i) model

        -- ── pool picker ────────────────────────────────────────────────────────
        UpdatePoolSearch s ->
            ( { model
                | modal =
                    case model.modal of
                        PoolPicker p _ ->
                            PoolPicker p s

                        other ->
                            other
              }
            , Cmd.none
            )

        PickPool pid ->
            case model.modal of
                PoolPicker (ForNewCert wid kind) _ ->
                    let
                        action =
                            case kind of
                                RegThenDeleg ->
                                    RegisterAndDelegate pid

                                DelegOnly ->
                                    DelegateOnly pid
                    in
                    ( addCert (Certificate wid action) { model | modal = NoModal }
                        |> log LogCmd (aliasOf wid model ++ ": delegate to " ++ pid)
                    , Cmd.none
                    )

                PoolPicker (ForEditCert i) _ ->
                    ( { model | certs = updateAt i (setCertPool pid) model.certs, modal = NoModal } |> invalidateShape, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ClosePoolModal ->
            ( { model | modal = NoModal }, Cmd.none )

        ClickLoadPools ->
            -- (re)load the first page, from inside the picker (e.g. the key was
            -- entered after opening it, or the fetch failed)
            if currentKey model == "" then
                toastNow "Enter a Blockfrost project id first" model

            else
                ( { model | pools = Loading, poolPage = 1 }
                , Blockfrost.fetchPools (currentKey model) model.network 1
                )

        ClickPoolPage page ->
            -- prev/next navigation: each view is exactly one server page, so shifting
            -- offsets can never show duplicates. Only fetched on click.
            if page < 1 || currentKey model == "" then
                ( model, Cmd.none )

            else
                ( { model | pools = Loading, poolPage = page }
                , Blockfrost.fetchPools (currentKey model) model.network page
                )

        GotPools (Ok ps) ->
            ( { model | pools = Loaded ps }
                |> log LogOk ("loaded " ++ String.fromInt (List.length ps) ++ " pools (page " ++ String.fromInt model.poolPage ++ ")")
            , Cmd.none
            )

        GotPools (Err e) ->
            ( { model | pools = Failed e }
                |> log LogWarn ("pool list failed: " ++ e)
            , Cmd.none
            )

        -- ── era & fee ──────────────────────────────────────────────────────────
        SelectEra e ->
            ( { model | era = e }
                |> invalidateShape
                |> log LogCmd
                    ("tx = "
                        ++ (if e == Conway then
                                "newTx()"

                            else
                                "newUpcomingEraTx()"
                           )
                    )
            , Cmd.none
            )

        ClickEstimateFee ->
            if not (txReady model) then
                toastNow "Add inputs and a recipient or certificate, and fix any flagged issues" model

            else
                -- a fresh estimate makes any existing signature's fee basis stale
                ( { model | fee = EstimatingFee }
                    |> invalidate
                    |> log LogCmd ("tx.estimateMinFee(pparams, " ++ String.fromInt (witnessCount model) ++ ", 0, 0)")
                , Wasm.estimateFee model
                )

        GotFeeEstimated result ->
            -- accepted only if the request that produced it is still the live one;
            -- an invalidating edit while it was in flight makes it stale (the same
            -- rule GotUtxos applies with its network stamp)
            if model.fee /= EstimatingFee then
                ( log LogWarn "dropped a stale fee estimate" model, Cmd.none )

            else
                case result of
                    Ok n ->
                        ( { model | fee = FeeSet n, feeText = String.fromInt n } |> log LogOk ("minFee = " ++ String.fromInt n ++ " lovelace"), Cmd.none )

                    Err e ->
                        ( { model | fee = NoFee, feeText = "" } |> log LogWarn ("fee estimate failed: " ++ e), Cmd.none )

        UpdateFeeText s ->
            -- Manual fee entry: only a positive integer counts. Plain `invalidate` here —
            -- `invalidateShape` would wipe the very fee being typed.
            ( { model
                | feeText = s
                , fee =
                    case String.toInt s of
                        Just n ->
                            if n > 0 then
                                FeeSet n

                            else
                                NoFee

                        Nothing ->
                            NoFee
              }
                |> invalidate
            , Cmd.none
            )

        -- ── sign / submit / export ─────────────────────────────────────────────
        ClickSign ->
            -- same predicate that enables the button (fee set + balanced + draft + ready)
            case ( canSign model, model.fee ) of
                ( True, FeeSet fee ) ->
                    ( { model | tx = Signing } |> log LogCmd "signWithPaymentKey(…) + alsoSign…"
                    , Wasm.signTx fee model
                    )

                _ ->
                    ( model, Cmd.none )

        GotTxSigned result ->
            -- accepted only while still Signing: an invalidating edit mid-flight
            -- must not let a stale signature resurrect itself
            if model.tx /= Signing then
                ( log LogWarn "dropped a stale signing result" model, Cmd.none )

            else
                case result of
                    Ok p ->
                        ( { model | tx = Signed (SignedTx p.cbor p.txId (List.length (paymentWalletIds model)) (List.length (stakeWalletIds model))), submit = NotSubmitted }
                            |> log LogOk ("transaction signed · txid " ++ p.txId)
                        , Cmd.none
                        )

                    Err e ->
                        ( { model | tx = Draft } |> log LogWarn ("sign failed: " ++ e), Cmd.none )

        ClickSubmit ->
            case model.tx of
                Signed s ->
                    if submitLocked model.submit then
                        -- a request is already in flight, or this tx is already accepted
                        ( model, Cmd.none )

                    else if currentKey model == "" then
                        toastNow "Enter a Blockfrost project id first" model

                    else
                        ( { model | submit = Submitting } |> log LogInfo "POST blockfrost /tx/submit"
                        , Blockfrost.submitTx (currentKey model) model.network s.txId s.cbor
                        )

                _ ->
                    ( model, Cmd.none )

        GotSubmitted expected result ->
            let
                -- accepted only while still Submitting AND only for the submission of
                -- the currently signed transaction: a superseded submit's late reply
                -- must not masquerade as the live one (the GotUtxos stamp idiom)
                live =
                    case ( model.submit, model.tx ) of
                        ( Submitting, Signed s ) ->
                            s.txId == expected

                        _ ->
                            False
            in
            if not live then
                -- the broadcast may still have happened — keep its outcome on record
                ( log LogWarn
                    ("dropped a stale submit result for "
                        ++ Format.shorten expected
                        ++ " — "
                        ++ (case result of
                                Ok txid ->
                                    "accepted · txid " ++ txid

                                Err e ->
                                    "failed: " ++ e
                           )
                    )
                    model
                , Cmd.none
                )

            else
                case result of
                    Ok txid ->
                        -- Blockfrost returns the tx hash; it must equal the id cardano-wasm
                        -- computed. A difference means a serialization/hash bug — the view
                        -- warns next to the accepted id, which is the authoritative one.
                        let
                            consistency =
                                if txid == expected then
                                    identity

                                else
                                    log LogWarn ("txid mismatch! wasm said " ++ expected ++ " but Blockfrost returned " ++ txid)
                        in
                        ( { model | submit = Submitted txid } |> log LogOk ("submitted · txid " ++ txid) |> consistency, Cmd.none )

                    Err e ->
                        let
                            shown =
                                if e == "timeout" then
                                    "no answer in 30 s — the transaction may still have been submitted"

                                else
                                    e
                        in
                        ( { model | submit = SubmitFailed shown } |> log LogWarn ("submit failed: " ++ shown), Cmd.none )

        ClickDownloadCli ->
            case model.tx of
                Signed s ->
                    -- cardano-cli TextEnvelope; broadcast with:
                    --   cardano-cli <era> transaction submit --tx-file tx.signed <network-flag>
                    let
                        envelope =
                            E.encode 4
                                (E.object
                                    [ ( "type", E.string (cliType model.era) )
                                    , ( "description", E.string "Ledger Cddl Format" )
                                    , ( "cborHex", E.string s.cbor )
                                    ]
                                )
                    in
                    ( log LogOk ("wrote tx.signed (" ++ cliType model.era ++ ")") model
                    , Download.string "tx.signed" "application/json" envelope
                    )

                _ ->
                    ( model, Cmd.none )

        -- ── clearing ───────────────────────────────────────────────────────────
        ClearInputs ->
            ( deselectInputs model |> invalidateShape, Cmd.none )

        ClearOutputs ->
            ( { model | outputs = [] } |> invalidateShape, Cmd.none )

        ClearCerts ->
            ( { model | certs = [] } |> invalidateShape, Cmd.none )

        ClearTx ->
            ( deselectInputs { model | outputs = [], certs = [] } |> invalidateShape, Cmd.none )

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
