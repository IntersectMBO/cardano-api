module Hex exposing (bytesToHex, hexToBytes)

{-| Base16 encoding/decoding of raw bytes. Used by Bech32 (pool ids) and by
Blockfrost submission (the signed tx CBOR travels as hex, the HTTP body as bytes).
-}

import Bytes
import Bytes.Encode as BE


bytesToHex : List Int -> String
bytesToHex =
    List.map byteToHex >> String.concat


byteToHex : Int -> String
byteToHex b =
    String.fromList [ hexDigit (b // 16), hexDigit (modBy 16 b) ]


hexDigit : Int -> Char
hexDigit n =
    String.toList "0123456789abcdef" |> List.drop n |> List.head |> Maybe.withDefault '0'


{-| Input is always wasm-produced CBOR hex, so invalid characters (mapped to 0)
cannot occur in practice.
-}
hexToBytes : String -> Bytes.Bytes
hexToBytes hex =
    String.toList hex
        |> hexToInts []
        |> List.reverse
        |> List.map BE.unsignedInt8
        |> BE.sequence
        |> BE.encode


hexToInts : List Int -> List Char -> List Int
hexToInts acc cs =
    case cs of
        a :: b :: rest ->
            hexToInts (hexNibble a * 16 + hexNibble b :: acc) rest

        _ ->
            acc


hexNibble : Char -> Int
hexNibble c =
    let
        n =
            Char.toCode c
    in
    if n >= 48 && n <= 57 then
        n - 48

    else if n >= 97 && n <= 102 then
        n - 87

    else if n >= 65 && n <= 70 then
        n - 55

    else
        0
