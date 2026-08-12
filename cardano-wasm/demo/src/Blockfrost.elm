module Blockfrost exposing (fetchPools, fetchUtxos, isBlockfrostNotFound, pageSize, poolsDecoder, submitTx, utxosDecoder)

{-| The Blockfrost boundary (plain HTTP, CORS-friendly from a static page).
Supplies UTxOs and the pool list, submits transactions — authenticated with
the per-network project id the user types into the UI. Deliberately kept
independent of `State`, so state helpers can build on this module without an
import cycle.
-}

import Format
import Hex
import Http
import Json.Decode as D
import Net exposing (blockfrostBase)
import Types exposing (..)



-- REQUESTS


{-| One page of UTxOs, newest first. The result is stamped with the network it
was requested on, so a response that survives a network switch can be dropped.
-}
fetchUtxos : String -> Network -> WalletId -> String -> Cmd Msg
fetchUtxos key network wid addr =
    request key
        network
        "GET"
        ("/addresses/" ++ addr ++ "/utxos?order=desc&count=" ++ String.fromInt pageSize)
        Http.emptyBody
        (expectUtxos (GotUtxos wid network))


{-| One page of registered pools (pages are 1-based; each reply replaces the
shown page). The result is stamped with the network and page it was requested
for, so a late reply that lands after a network switch or another page click
can be dropped.
-}
fetchPools : String -> Network -> Int -> Cmd Msg
fetchPools key network page =
    request key
        network
        "GET"
        ("/pools/extended?count=" ++ String.fromInt pageSize ++ "&page=" ++ String.fromInt page)
        Http.emptyBody
        (expectPools (GotPools network page))


{-| POST the signed CBOR. The reply is stamped with the id of the transaction
it answers, so a superseded submission's late reply can be told apart.
-}
submitTx : String -> Network -> String -> String -> Cmd Msg
submitTx key network txId cborHex =
    request key network "POST" "/tx/submit" (Http.bytesBody "application/cbor" (Hex.hexToBytes cborHex)) (expectSubmit (GotSubmitted txId))


{-| Blockfrost's maximum page size; a full page means more entries may exist.
-}
pageSize : Int
pageSize =
    100


request : String -> Network -> String -> String -> Http.Body -> Http.Expect Msg -> Cmd Msg
request key network method path body expect =
    Http.request
        { method = method
        , headers = [ Http.header "project_id" key ]
        , url = blockfrostBase network ++ path
        , body = body
        , expect = expect

        -- a stalled request must not wedge the wallet in Loading forever
        , timeout = Just 30000
        , tracker = Nothing
        }



-- RESPONSES


{-| Route a string response: statuses go to `onStatus` (good AND bad — the code
is in the metadata), transport failures become ready-made error strings.
-}
expectResponse : (Http.Metadata -> String -> Result String a) -> (Result String a -> Msg) -> Http.Expect Msg
expectResponse onStatus toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.GoodStatus_ meta body ->
                    onStatus meta body

                Http.BadStatus_ meta body ->
                    onStatus meta body

                Http.NetworkError_ ->
                    Err "network error"

                Http.Timeout_ ->
                    Err "timeout"

                Http.BadUrl_ u ->
                    Err ("bad url " ++ u)


{-| Blockfrost answers 404 with its own error JSON for an address it has never
seen — treat exactly that as "no UTxOs". Any other 404 (proxy error page,
wrong path) stays an error instead of becoming a confident empty wallet.
-}
expectUtxos : (Result String UtxoPage -> Msg) -> Http.Expect Msg
expectUtxos =
    expectResponse
        (\meta body ->
            if meta.statusCode == 404 && isBlockfrostNotFound body then
                Ok { utxos = [], truncated = False }

            else if meta.statusCode >= 200 && meta.statusCode < 300 then
                D.decodeString utxosDecoder body
                    |> Result.map (\us -> { utxos = us, truncated = List.length us == pageSize })
                    |> Result.mapError D.errorToString

            else
                Err (statusErrStr meta body)
        )


expectPools : (Result String PoolPage -> Msg) -> Http.Expect Msg
expectPools =
    expectResponse
        (\meta body ->
            if meta.statusCode >= 200 && meta.statusCode < 300 then
                D.decodeString poolsDecoder body
                    |> Result.map (\ps -> { pools = ps, hasMore = List.length ps == pageSize })
                    |> Result.mapError D.errorToString

            else
                Err (statusErrStr meta body)
        )


{-| /tx/submit returns the tx hash as a JSON string on success, a JSON error
otherwise. A 2xx only counts as success when the body actually is a tx hash —
a proxy's page or an empty body must not become a confident "submitted".
-}
expectSubmit : (Result String String -> Msg) -> Http.Expect Msg
expectSubmit =
    expectResponse
        (\meta body ->
            let
                parsed =
                    D.decodeString D.string body |> Result.withDefault (String.trim body)
            in
            if meta.statusCode >= 200 && meta.statusCode < 300 then
                if isTxHash parsed then
                    Ok parsed

                else
                    Err ("unexpected response body: " ++ String.left 120 parsed)

            else
                Err (submitErrStr meta body)
        )


{-| A transaction id: 64 lowercase hex characters.
-}
isTxHash : String -> Bool
isTxHash s =
    String.length s == 64 && String.all (\c -> Char.isDigit c || (c >= 'a' && c <= 'f')) s


{-| Like statusErrStr, but falls back to the raw body when Blockfrost's message
field is absent (a gateway may answer plain text), and keeps more of it: the
ledger's rejection reason — the part the user actually needs — can be long.
Truncation is marked, so a cut message can't pass for a complete one.
-}
submitErrStr : Http.Metadata -> String -> String
submitErrStr meta body =
    let
        full =
            "HTTP "
                ++ String.fromInt meta.statusCode
                ++ " — "
                ++ (case D.decodeString (D.field "message" D.string) body of
                        Ok message ->
                            message

                        Err _ ->
                            String.trim body
                   )
    in
    if String.length full > 500 then
        String.left 500 full ++ "…"

    else
        full


{-| Exposed for the test suite.
-}
isBlockfrostNotFound : String -> Bool
isBlockfrostNotFound body =
    D.decodeString (D.field "status_code" D.int) body == Ok 404


{-| "HTTP 403 — Network token mismatch" beats a bare "HTTP 403": Blockfrost's
error bodies distinguish an invalid project id from a right-id-wrong-network
one, the two most likely first-run mistakes.
-}
statusErrStr : Http.Metadata -> String -> String
statusErrStr meta body =
    "HTTP "
        ++ String.fromInt meta.statusCode
        ++ (case D.decodeString (D.field "message" D.string) body of
                Ok message ->
                    " — " ++ message

                Err _ ->
                    ""
           )



-- DECODERS


{-| Exposed for the test suite.
-}
utxosDecoder : D.Decoder (List Utxo)
utxosDecoder =
    D.list
        (D.map3
            (\h i units ->
                { txId = h
                , txIx = i
                , lovelace = lovelaceIn units
                , selected = False

                -- any non-lovelace unit = native tokens (unusable in this ADA-only demo)
                , hasAssets = List.any (\( u, _ ) -> u /= "lovelace") units
                }
            )
            (D.field "tx_hash" D.string)
            (D.field "output_index" D.int)
            (D.field "amount" unitsDecoder)
        )


{-| Blockfrost's "amount" is a list of { unit, quantity } entries, quantities
as strings. Only the lovelace quantities are parsed — and loudly: a malformed
one fails the decode instead of counting as 0. Token quantities can exceed Int
precision and are never used, so they are not parsed at all.
-}
unitsDecoder : D.Decoder (List ( String, Int ))
unitsDecoder =
    D.list
        (D.field "unit" D.string
            |> D.andThen
                (\unit ->
                    if unit == "lovelace" then
                        D.field "quantity" lovelaceQuantityDecoder |> D.map (Tuple.pair unit)

                    else
                        D.succeed ( unit, 0 )
                )
        )


{-| A lovelace quantity is a plain digit run; anything else is a decode failure.
-}
lovelaceQuantityDecoder : D.Decoder Int
lovelaceQuantityDecoder =
    D.string
        |> D.andThen
            (\s ->
                case Format.digits s of
                    Just n ->
                        D.succeed n

                    Nothing ->
                        D.fail ("bad lovelace quantity: " ++ s)
            )


lovelaceIn : List ( String, Int ) -> Int
lovelaceIn units =
    units |> List.filter (\( u, _ ) -> u == "lovelace") |> List.map Tuple.second |> List.sum


poolsDecoder : D.Decoder (List Pool)
poolsDecoder =
    D.list
        (D.map5 Pool
            (D.field "pool_id" D.string)
            (D.field "hex" D.string)
            -- `metadata` is null for pools that never registered any, and `ticker`
            -- is nullable within it — D.maybe absorbs every shape
            (D.maybe (D.at [ "metadata", "ticker" ] D.string))
            (D.field "live_stake" (D.nullable lovelaceStringDecoder) |> D.map (Maybe.withDefault 0))
            (D.field "live_saturation" (D.nullable D.float) |> D.map (Maybe.withDefault 0))
        )


{-| Lovelace amounts arrive as strings (or occasionally numbers).
-}
lovelaceStringDecoder : D.Decoder Int
lovelaceStringDecoder =
    D.oneOf
        [ D.int
        , D.string
            |> D.andThen
                (\s ->
                    case String.toInt s of
                        Just n ->
                            D.succeed n

                        Nothing ->
                            D.fail "bad lovelace"
                )
        ]
