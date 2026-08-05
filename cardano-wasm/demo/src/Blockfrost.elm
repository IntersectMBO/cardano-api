module Blockfrost exposing (fetchUtxos, isBlockfrostNotFound, pageSize, utxosDecoder)

{-| The Blockfrost boundary (plain HTTP, CORS-friendly from a static page).
Supplies UTxOs, authenticated with the per-network project id the user types
into the UI. Deliberately independent of `State`, so state helpers can build
on this module without an import cycle.
-}

import Format
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


{-| Blockfrost's maximum page size; a full page means more UTxOs may exist.
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
