module CoreTest exposing (suite)

{-| Pure-Elm unit tests for the demo's helper modules: Bech32, Format, and the
pure parts of State (computeBalance and the generic list helpers).

Golden bech32 vectors are real preprod pools, verified against Koios.

-}

import Bech32
import Expect
import Format
import State
import Test exposing (Test, describe, test)
import Types exposing (Balance(..))


suite : Test
suite =
    describe "core helpers"
        [ bech32Tests
        , formatTests
        , computeBalanceTests
        , listHelperTests
        ]



-- BECH32


{-| ( bech32 pool id, expected hex ) — verified against Koios.
-}
poolVectors : List ( String, String )
poolVectors =
    [ ( "pool1547tew8vmuj0g6vj3k5jfddudextcw6hsk2hwgg6pkhk7lwphe6"
      , "a57cbcb8ecdf24f469928da924b5bc6e4cbc3b57859577211a0daf6f"
      )
    , ( "pool174mw7e20768e8vj4fn8y6p536n8rkzswsapwtwn354dckpjqzr8"
      , "f576ef654ff68f93b2554cce4d0691d4ce3b0a0e8742e5ba71a55b8b"
      )
    , ( "pool1z22x50lqsrwent6en0llzzs9e577rx7n3mv9kfw7udwa2rf42fa"
      , "12946a3fe080dd99af599bfff10a05cd3de19bd38ed85b25dee35dd5"
      )
    , ( "pool1wn6a6f23ctq06udwhw27ravdpd6zcr7jlut3yez0wzdackz3222"
      , "74f5dd2551c2c0fd71aebb95e1f58d0b742c0fd2ff1712644f709bdc"
      )
    ]


bech32Tests : Test
bech32Tests =
    describe "Bech32.bech32ToHex"
        [ describe "golden vectors (preprod pools)"
            (List.map
                (\( bech, hex ) ->
                    test bech <|
                        \_ -> Bech32.bech32ToHex bech |> Expect.equal (Just hex)
                )
                poolVectors
            )
        , test "uppercase input decodes the same (lowercased internally)" <|
            \_ ->
                Bech32.bech32ToHex
                    (String.toUpper "pool1547tew8vmuj0g6vj3k5jfddudextcw6hsk2hwgg6pkhk7lwphe6")
                    |> Expect.equal
                        (Just "a57cbcb8ecdf24f469928da924b5bc6e4cbc3b57859577211a0daf6f")
        , test "empty string is rejected" <|
            \_ -> Bech32.bech32ToHex "" |> Expect.equal Nothing
        , test "\"hello world\" is rejected (no separator, chars outside charset)" <|
            \_ -> Bech32.bech32ToHex "hello world" |> Expect.equal Nothing
        , test "\"pool1\" is rejected (fewer than 6 data symbols)" <|
            \_ -> Bech32.bech32ToHex "pool1" |> Expect.equal Nothing
        ]



-- FORMAT


formatTests : Test
formatTests =
    describe "Format"
        [ describe "adaToLovelace"
            [ test "\"1.5\" parses to 1500000" <|
                \_ -> Format.adaToLovelace "1.5" |> Expect.equal (Just 1500000)
            , test "\"0\" parses to 0" <|
                \_ -> Format.adaToLovelace "0" |> Expect.equal (Just 0)
            , test "\" 2 \" is trimmed and parses to 2000000" <|
                \_ -> Format.adaToLovelace " 2 " |> Expect.equal (Just 2000000)
            , test "\"1,5\" (comma) is rejected" <|
                \_ -> Format.adaToLovelace "1,5" |> Expect.equal Nothing
            , test "\"abc\" is rejected" <|
                \_ -> Format.adaToLovelace "abc" |> Expect.equal Nothing
            , test "\"\" is rejected" <|
                \_ -> Format.adaToLovelace "" |> Expect.equal Nothing
            ]
        , describe "lovelaceToAda"
            [ test "1500000 → \"1.5\"" <|
                \_ -> Format.lovelaceToAda 1500000 |> Expect.equal "1.5"
            , test "969750 → \"0.96975\"" <|
                \_ -> Format.lovelaceToAda 969750 |> Expect.equal "0.96975"
            , test "0 → \"0\"" <|
                \_ -> Format.lovelaceToAda 0 |> Expect.equal "0"
            , test "-1500000 → \"-1.5\"" <|
                \_ -> Format.lovelaceToAda -1500000 |> Expect.equal "-1.5"
            , test "1000000 → \"1\" (no trailing dot or zeros)" <|
                \_ -> Format.lovelaceToAda 1000000 |> Expect.equal "1"
            ]
        , describe "amountError"
            [ test "\"\" is not an error (empty is allowed)" <|
                \_ -> Format.amountError "" |> Expect.equal False
            , test "\"1.5\" is not an error" <|
                \_ -> Format.amountError "1.5" |> Expect.equal False
            , test "\"1,5\" is an error (unparseable)" <|
                \_ -> Format.amountError "1,5" |> Expect.equal True
            , test "\"0\" is an error (not positive)" <|
                \_ -> Format.amountError "0" |> Expect.equal True
            , test "\"-2\" is an error (negative)" <|
                \_ -> Format.amountError "-2" |> Expect.equal True
            ]
        , describe "round-trip adaToLovelace ∘ lovelaceToAda"
            (List.map
                (\v ->
                    test (String.fromInt v ++ " lovelace survives the round-trip") <|
                        \_ ->
                            Format.adaToLovelace (Format.lovelaceToAda v)
                                |> Expect.equal (Just v)
                )
                [ 1, 999999, 1000000, 969750, 123456789 ]
            )
        ]



-- COMPUTE BALANCE


{-| Shorthand: a transaction with no deposit and the given fee.
-}
bal : Int -> Int -> Int -> Balance
bal inputs outputs fee =
    State.computeBalance minUtxo { inputs = inputs, outputs = outputs, deposit = 0, fee = fee }


minUtxo : Int
minUtxo =
    State.adaOnlyMinUtxo { keyDeposit = 2000000, coinsPerUtxoByte = 4310 }


computeBalanceTests : Test
computeBalanceTests =
    describe "State.computeBalance"
        [ test "adaOnlyMinUtxo is 969750 (assumed by the cases below)" <|
            \_ -> minUtxo |> Expect.equal 969750
        , test "inputs exactly cover outputs+fee → Balanced 0" <|
            \_ -> bal 5000000 4800000 200000 |> Expect.equal (Balanced 0)
        , test "short by 1 → Insufficient 1" <|
            \_ -> bal 5000000 4800001 200000 |> Expect.equal (Insufficient 1)
        , test "change of exactly minUtxo → Balanced minUtxo" <|
            \_ -> bal (5000000 + minUtxo) 4800000 200000 |> Expect.equal (Balanced minUtxo)
        , test "change of minUtxo - 1 → DustChange" <|
            \_ ->
                bal (5000000 + minUtxo - 1) 4800000 200000
                    |> Expect.equal (DustChange (minUtxo - 1) minUtxo)
        , test "change of 1 → DustChange 1" <|
            \_ -> bal 5000001 4800000 200000 |> Expect.equal (DustChange 1 minUtxo)
        , test "big change → Balanced" <|
            \_ -> bal 100000000 4800000 200000 |> Expect.equal (Balanced 95000000)
        , test "deposit counts against the change" <|
            \_ ->
                State.computeBalance minUtxo
                    { inputs = 5000000, outputs = 2000000, deposit = 2800000, fee = 200000 }
                    |> Expect.equal (Balanced 0)
        ]



-- LIST HELPERS


listHelperTests : Test
listHelperTests =
    describe "State list helpers"
        [ describe "removeAt"
            [ test "index 0 removes the head" <|
                \_ -> State.removeAt 0 [ 1, 2, 3 ] |> Expect.equal [ 2, 3 ]
            , test "middle index" <|
                \_ -> State.removeAt 1 [ 1, 2, 3 ] |> Expect.equal [ 1, 3 ]
            , test "last index" <|
                \_ -> State.removeAt 2 [ 1, 2, 3 ] |> Expect.equal [ 1, 2 ]
            , test "out-of-range index leaves the list unchanged" <|
                \_ -> State.removeAt 5 [ 1, 2, 3 ] |> Expect.equal [ 1, 2, 3 ]
            , test "negative index leaves the list unchanged" <|
                \_ -> State.removeAt -1 [ 1, 2, 3 ] |> Expect.equal [ 1, 2, 3 ]
            ]
        , describe "updateAt"
            [ test "in-range index applies the function to that element only" <|
                \_ -> State.updateAt 1 ((*) 10) [ 1, 2, 3 ] |> Expect.equal [ 1, 20, 3 ]
            , test "out-of-range index leaves the list unchanged" <|
                \_ -> State.updateAt 7 ((*) 10) [ 1, 2, 3 ] |> Expect.equal [ 1, 2, 3 ]
            ]
        , describe "distinct"
            [ test "no duplicates: order preserved" <|
                \_ -> State.distinct [ 3, 1, 2 ] |> Expect.equal [ 3, 1, 2 ]
            , test "duplicates: keeps the first occurrence of each ([3,1,3,2,1] → [3,1,2])" <|
                \_ -> State.distinct [ 3, 1, 3, 2, 1 ] |> Expect.equal [ 3, 1, 2 ]
            , test "empty list" <|
                \_ ->
                    let
                        empty : List Int
                        empty =
                            []
                    in
                    State.distinct empty |> Expect.equal empty
            ]
        ]
