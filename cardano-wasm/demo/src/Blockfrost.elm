module Blockfrost exposing (fetchUtxos, httpErrStr, submitTx)

{-| The Blockfrost boundary (plain HTTP, CORS-friendly from a static page).
Supplies UTxOs and broadcasts signed transactions, authenticated with the
per-network project id the user types into the UI.
-}

import Hex
import Http
import Json.Decode as D
import Net exposing (blockfrostBase)
import State exposing (currentKey)
import Types exposing (..)



-- REQUESTS


fetchUtxos : Model -> WalletId -> String -> Cmd Msg
fetchUtxos model wid addr =
    request model "GET" ("/addresses/" ++ addr ++ "/utxos?count=100") Http.emptyBody (expectUtxos (GotUtxos wid))


submitTx : Model -> String -> Cmd Msg
submitTx model cborHex =
    request model "POST" "/tx/submit" (Http.bytesBody "application/cbor" (Hex.hexToBytes cborHex)) (expectSubmit GotSubmitted)


request : Model -> String -> String -> Http.Body -> Http.Expect Msg -> Cmd Msg
request model method path body expect =
    Http.request
        { method = method
        , headers = [ Http.header "project_id" (currentKey model) ]
        , url = blockfrostBase model.network ++ path
        , body = body
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }



-- RESPONSES


{-| Route a string response: statuses go to `onStatus` (good AND bad — the code is
in the metadata), transport failures are wrapped by `fromErr`.
-}
expectResponse : (Http.Error -> e) -> (Http.Metadata -> String -> Result e a) -> (Result e a -> Msg) -> Http.Expect Msg
expectResponse fromErr onStatus toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.GoodStatus_ meta body ->
                    onStatus meta body

                Http.BadStatus_ meta body ->
                    onStatus meta body

                Http.NetworkError_ ->
                    Err (fromErr Http.NetworkError)

                Http.Timeout_ ->
                    Err (fromErr Http.Timeout)

                Http.BadUrl_ u ->
                    Err (fromErr (Http.BadUrl u))


{-| Blockfrost returns 404 for an address it has never seen — treat that as "no UTxOs".
-}
expectUtxos : (Result Http.Error (List Utxo) -> Msg) -> Http.Expect Msg
expectUtxos =
    expectResponse identity
        (\meta body ->
            if meta.statusCode == 404 then
                Ok []

            else if meta.statusCode >= 200 && meta.statusCode < 300 then
                D.decodeString utxosDecoder body |> Result.mapError (D.errorToString >> Http.BadBody)

            else
                Err (Http.BadStatus meta.statusCode)
        )


{-| /tx/submit returns the tx hash as a JSON string on success, a JSON error otherwise.
-}
expectSubmit : (Result String String -> Msg) -> Http.Expect Msg
expectSubmit =
    expectResponse httpErrStr
        (\meta body ->
            if meta.statusCode >= 200 && meta.statusCode < 300 then
                Ok (D.decodeString D.string body |> Result.withDefault (String.trim body))

            else
                Err ("HTTP " ++ String.fromInt meta.statusCode ++ " · " ++ String.left 300 body)
        )


httpErrStr : Http.Error -> String
httpErrStr err =
    case err of
        Http.BadUrl u ->
            "bad url " ++ u

        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "network error"

        Http.BadStatus code ->
            "HTTP " ++ String.fromInt code

        Http.BadBody b ->
            "bad body: " ++ b



-- DECODERS


utxosDecoder : D.Decoder (List Utxo)
utxosDecoder =
    D.list
        (D.map3
            (\h i units ->
                Utxo h
                    i
                    (lovelaceIn units)
                    False
                    -- any non-lovelace unit = native tokens (unusable in this ADA-only demo)
                    (List.any (\( u, _ ) -> u /= "lovelace") units)
            )
            (D.field "tx_hash" D.string)
            (D.field "output_index" D.int)
            (D.field "amount" unitsDecoder)
        )


{-| Blockfrost's "amount" is a list of { unit, quantity } entries, quantities as strings.
-}
unitsDecoder : D.Decoder (List ( String, Int ))
unitsDecoder =
    D.list
        (D.map2 Tuple.pair
            (D.field "unit" D.string)
            (D.field "quantity" (D.string |> D.map (String.toInt >> Maybe.withDefault 0)))
        )


lovelaceIn : List ( String, Int ) -> Int
lovelaceIn units =
    units |> List.filter (\( u, _ ) -> u == "lovelace") |> List.map Tuple.second |> List.sum
