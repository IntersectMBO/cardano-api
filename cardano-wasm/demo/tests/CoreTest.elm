module CoreTest exposing (suite)

{-| Pure-Elm unit tests for the demo's helper modules: Bech32, Format, the
pure parts of State (computeBalance, certificates, the generic list helpers),
the Blockfrost decoders, and the certificate JSON sent to cardano-wasm.

Golden bech32 vectors are real preprod pools, verified against Koios.

-}

import Bech32
import Blockfrost
import Bytes
import Bytes.Decode
import Expect
import Format
import Hex
import Json.Decode as D
import Json.Encode as E
import State
import Test exposing (Test, describe, test)
import Types exposing (Balance(..), CertAction(..), Loadable(..), Msg(..), Network(..), OutputAmount(..))
import Update
import Wasm


suite : Test
suite =
    describe "core helpers"
        [ bech32Tests
        , formatTests
        , stateSelectionTests
        , hexTests
        , blockfrostTests
        , computeBalanceTests
        , certTests
        , poolListTests
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
        , test "a one-character typo is rejected (the checksum is verified)" <|
            \_ ->
                Bech32.bech32ToHex "pool1447tew8vmuj0g6vj3k5jfddudextcw6hsk2hwgg6pkhk7lwphe6"
                    |> Expect.equal Nothing
        , test "a valid checksum under another prefix is rejected (BIP-173 test vector)" <|
            \_ ->
                Bech32.bech32ToHex "abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw"
                    |> Expect.equal Nothing
        , test "a valid 28-byte payload under another prefix is rejected" <|
            \_ ->
                Bech32.bech32ToHex "stake1547tew8vmuj0g6vj3k5jfddudextcw6hsk2hwgg6pkhk7sft67d"
                    |> Expect.equal Nothing
        , test "a pool id whose payload is not 28 bytes is rejected" <|
            \_ ->
                Bech32.bech32ToHex "pool1547tew8vmuj0g6vj3k5jfddudextcw6hj5z3p9"
                    |> Expect.equal Nothing
        , test "a non-zero padding bit is rejected (payload otherwise valid)" <|
            \_ ->
                Bech32.bech32ToHex "pool1547tew8vmuj0g6vj3k5jfddudextcw6hsk2hwgg6pkhklzc4zyg"
                    |> Expect.equal Nothing
        , test "mixed case is rejected" <|
            \_ ->
                Bech32.bech32ToHex "Pool1547tew8vmuj0g6vj3k5jfddudextcw6hsk2hwgg6pkhk7lwphe6"
                    |> Expect.equal Nothing
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
            , test "one lovelace as decimals" <|
                \_ -> Format.adaToLovelace "0.000001" |> Expect.equal (Just 1)
            , test "more than six decimals is rejected" <|
                \_ -> Format.adaToLovelace "1.2345678" |> Expect.equal Nothing
            , test "scientific notation is rejected" <|
                \_ -> Format.adaToLovelace "1e3" |> Expect.equal Nothing
            , test "negative amounts are rejected" <|
                \_ -> Format.adaToLovelace "-1.5" |> Expect.equal Nothing
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
            , test "whole part beyond 32 bits (Elm's // would wrap)" <|
                \_ -> Format.lovelaceToAda 3000000000000000 |> Expect.equal "3000000000"
            , test "first whole-ADA value past the 32-bit boundary" <|
                \_ -> Format.lovelaceToAda 2147483648000000 |> Expect.equal "2147483648"
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
                [ 1, 999999, 1000000, 969750, 123456789, 3000000000000000 ]
            )
        ]



-- STATE SELECTION & OUTPUTS


stateSelectionTests : Test
stateSelectionTests =
    describe "State selection & outputs"
        [ describe "selectedInputs"
            [ test "an outpoint listed by duplicate wallets is counted once" <|
                \_ ->
                    let
                        utxo =
                            { txId = "aa", txIx = 0, lovelace = 5000000, selected = True, hasAssets = False }

                        mkWallet i =
                            { id = i
                            , alias = "w" ++ String.fromInt i
                            , address = "addr_test1dup"
                            , keys = { paymentVKey = "", paymentSKey = "", stakeVKey = "", stakeSKey = "", paymentKeyHash = "", stakeKeyHash = "" }
                            , utxos = Loaded { utxos = [ utxo ], truncated = False }
                            , expanded = False
                            , color = "#123456"
                            }

                        model =
                            State.init { keyDeposit = 2000000, coinsPerUtxoByte = 4310 } |> Tuple.first
                    in
                    State.selectedInputs { model | wallets = [ mkWallet 1, mkWallet 2 ] }
                        |> List.map (\( w, u ) -> ( w.id, ( u.txId, u.txIx ) ))
                        |> Expect.equal [ ( 1, ( "aa", 0 ) ) ]
            ]
        , describe "outputsComplete: explicit outputs respect min-UTxO"
            (let
                baseModel =
                    State.init { keyDeposit = 2000000, coinsPerUtxoByte = 4310 } |> Tuple.first

                withAmount s =
                    { baseModel | outputs = [ { address = "addr_test1x", alias = "a", amount = Lovelace s } ] }
             in
             [ test "below min-UTxO is incomplete" <|
                \_ -> State.outputsComplete (withAmount "0.5") |> Expect.equal False
             , test "exactly min-UTxO (0.96975) is complete" <|
                \_ -> State.outputsComplete (withAmount "0.96975") |> Expect.equal True
             , test "a change-only draft stays complete" <|
                \_ ->
                    State.outputsComplete
                        { baseModel | outputs = [ { address = "addr_test1x", alias = "a", amount = Change } ] }
                        |> Expect.equal True
             , test "amountBelowMin flags parseable-but-small, not invalid" <|
                \_ ->
                    ( State.amountBelowMin baseModel "0.5"
                    , State.amountBelowMin baseModel "abc"
                    )
                        |> Expect.equal ( True, False )
             ]
            )
        ]



-- HEX


hexTests : Test
hexTests =
    describe "Hex round-trip"
        [ test "wasm-shaped hex survives hexToBytes ∘ bytesToHex" <|
            \_ -> Hex.bytesToHex (bytesToList (Hex.hexToBytes "84a400818258")) |> Expect.equal "84a400818258"
        , test "uppercase input decodes; re-encoding is lowercase" <|
            \_ -> Hex.bytesToHex (bytesToList (Hex.hexToBytes "DEADBEEF")) |> Expect.equal "deadbeef"
        , test "a trailing odd nibble is dropped (documented: wasm hex is always even)" <|
            \_ -> Hex.bytesToHex (bytesToList (Hex.hexToBytes "abc")) |> Expect.equal "ab"
        ]



-- BLOCKFROST DECODERS


blockfrostTests : Test
blockfrostTests =
    describe "Blockfrost decoders"
        [ test "decodes lovelace and flags native tokens" <|
            \_ ->
                D.decodeString Blockfrost.utxosDecoder utxoBody
                    |> Expect.equal
                        (Ok
                            [ { txId = "abcd"
                              , txIx = 1
                              , lovelace = 5000000
                              , selected = False
                              , hasAssets = True
                              }
                            ]
                        )
        , test "a malformed lovelace quantity fails the decode (never a silent 0)" <|
            \_ ->
                D.decodeString Blockfrost.utxosDecoder
                    """[{"tx_hash":"ab","output_index":0,"amount":[{"unit":"lovelace","quantity":"12x"}]}]"""
                    |> okOrErr
                    |> Expect.equal "Err"
        , test "a negative lovelace quantity fails the decode" <|
            \_ ->
                D.decodeString Blockfrost.utxosDecoder
                    """[{"tx_hash":"ab","output_index":0,"amount":[{"unit":"lovelace","quantity":"-5"}]}]"""
                    |> okOrErr
                    |> Expect.equal "Err"
        , test "Blockfrost's own not-found body is recognised" <|
            \_ ->
                Blockfrost.isBlockfrostNotFound
                    """{"status_code":404,"error":"Not Found","message":"The requested component has not been found."}"""
                    |> Expect.equal True
        , test "an arbitrary 404 page is not recognised as Blockfrost's not-found" <|
            \_ ->
                Blockfrost.isBlockfrostNotFound "<html>404 not found</html>"
                    |> Expect.equal False
        , test "pools: ticker, hex and stats decode from a canonical row" <|
            \_ ->
                D.decodeString Blockfrost.poolsDecoder poolBody
                    |> Expect.equal
                        (Ok
                            [ { idBech32 = "pool1547tew8vmuj0g6vj3k5jfddudextcw6hsk2hwgg6pkhk7lwphe6"
                              , idHex = "a57cbcb8ecdf24f469928da924b5bc6e4cbc3b57859577211a0daf6f"
                              , ticker = Just "IOG1"
                              , liveStake = 123456789
                              , saturation = 0.5
                              }
                            ]
                        )
        , test "pools: null metadata means no ticker, not a failed page" <|
            \_ ->
                D.decodeString Blockfrost.poolsDecoder
                    """[{"pool_id":"pool1x","hex":"aa","metadata":null,"live_stake":"1","live_saturation":0.1}]"""
                    |> Result.map (List.map .ticker)
                    |> Expect.equal (Ok [ Nothing ])
        , test "pools: a missing hex fails the decode (it is load-bearing)" <|
            \_ ->
                D.decodeString Blockfrost.poolsDecoder
                    """[{"pool_id":"pool1x","metadata":null,"live_stake":"1","live_saturation":0.1}]"""
                    |> okOrErr
                    |> Expect.equal "Err"
        ]



-- TEST HELPERS & FIXTURES


bytesToList : Bytes.Bytes -> List Int
bytesToList bs =
    Bytes.Decode.decode (byteListDecoder (Bytes.width bs)) bs |> Maybe.withDefault []


byteListDecoder : Int -> Bytes.Decode.Decoder (List Int)
byteListDecoder n =
    Bytes.Decode.loop ( n, [] )
        (\( k, acc ) ->
            if k <= 0 then
                Bytes.Decode.succeed (Bytes.Decode.Done (List.reverse acc))

            else
                Bytes.Decode.map (\b -> Bytes.Decode.Loop ( k - 1, b :: acc )) Bytes.Decode.unsignedInt8
        )


okOrErr : Result e a -> String
okOrErr r =
    case r of
        Ok _ ->
            "Ok"

        Err _ ->
            "Err"


utxoBody : String
utxoBody =
    """[{"tx_hash":"abcd","output_index":1,"amount":[{"unit":"lovelace","quantity":"5000000"},{"unit":"deadbeef646561646265656664656164","quantity":"999999999999999999999999"}]}]"""


{-| One canonical /pools/extended row (the fields the decoder does not read
included on purpose, as Blockfrost sends them).
-}
poolBody : String
poolBody =
    """[{"pool_id":"pool1547tew8vmuj0g6vj3k5jfddudextcw6hsk2hwgg6pkhk7lwphe6","hex":"a57cbcb8ecdf24f469928da924b5bc6e4cbc3b57859577211a0daf6f","active_stake":"0","live_stake":"123456789","live_saturation":0.5,"blocks_minted":12,"margin_cost":0.05,"fixed_cost":"340000000","declared_pledge":"1000000","metadata":{"ticker":"IOG1","name":"Example","description":null,"homepage":null}}]"""



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



-- CERTIFICATES


certTests : Test
certTests =
    let
        base =
            State.init { keyDeposit = 2000000, coinsPerUtxoByte = 4310 } |> Tuple.first

        ref =
            { bech32 = "pool1547tew8vmuj0g6vj3k5jfddudextcw6hsk2hwgg6pkhk7lwphe6"
            , hex = "a57cbcb8ecdf24f469928da924b5bc6e4cbc3b57859577211a0daf6f"
            , ticker = Nothing
            }

        wallet =
            { id = 1
            , alias = "w1"
            , address = "addr_test1x"
            , keys = { paymentVKey = "", paymentSKey = "", stakeVKey = "", stakeSKey = "", paymentKeyHash = "", stakeKeyHash = "5ca1ab1e" }
            , utxos = NotAsked
            , expanded = False
            , color = "#123456"
            }

        withCert action =
            { base | wallets = [ wallet ], certs = [ { wallet = 1, action = action } ] }
    in
    describe "certificates"
        [ describe "State.depositTotal signs"
            [ test "Register adds the deposit" <|
                \_ -> State.depositTotal (withCert Register) |> Expect.equal 2000000
            , test "Register + delegate adds the deposit once" <|
                \_ -> State.depositTotal (withCert (RegisterAndDelegate ref)) |> Expect.equal 2000000
            , test "Delegate only is deposit-neutral" <|
                \_ -> State.depositTotal (withCert (DelegateOnly ref)) |> Expect.equal 0
            , test "Unregister refunds the deposit" <|
                \_ -> State.depositTotal (withCert Unregister) |> Expect.equal -2000000
            ]
        , describe "State.walletCertAction menu codes"
            (List.map
                (\( action, code ) ->
                    test ("code \"" ++ code ++ "\"") <|
                        \_ -> State.walletCertAction 1 (withCert action) |> Expect.equal code
                )
                [ ( Register, "reg" )
                , ( RegisterAndDelegate ref, "deleg" )
                , ( DelegateOnly ref, "delegonly" )
                , ( Unregister, "unreg" )
                ]
                ++ [ test "reads the queried wallet's certificate, not the first" <|
                        \_ ->
                            let
                                model =
                                    { base | certs = [ { wallet = 1, action = Register }, { wallet = 2, action = Unregister } ] }
                            in
                            ( State.walletCertAction 2 model, State.walletCertAction 3 model )
                                |> Expect.equal ( "unreg", "" )
                   ]
            )
        , describe "Wasm.certJson"
            [ test "register + delegate expands to two certificates, registration first" <|
                \_ ->
                    Wasm.certJson (withCert (RegisterAndDelegate ref)) { wallet = 1, action = RegisterAndDelegate ref }
                        |> List.map (E.encode 0)
                        |> Expect.equal
                            [ """{"action":"register","stakeKeyHash":"5ca1ab1e","deposit":2000000}"""
                            , """{"action":"delegate","stakeKeyHash":"5ca1ab1e","poolId":"a57cbcb8ecdf24f469928da924b5bc6e4cbc3b57859577211a0daf6f"}"""
                            ]
            , test "the delegation carries the hex pinned at pick time" <|
                \_ ->
                    Wasm.certJson (withCert (DelegateOnly ref)) { wallet = 1, action = DelegateOnly ref }
                        |> List.map (E.encode 0)
                        |> List.map (D.decodeString (D.field "poolId" D.string))
                        |> Expect.equal [ Ok ref.hex ]
            , test "a bare registration emits exactly the registration certificate" <|
                \_ ->
                    Wasm.certJson (withCert Register) { wallet = 1, action = Register }
                        |> List.map (E.encode 0)
                        |> Expect.equal
                            [ """{"action":"register","stakeKeyHash":"5ca1ab1e","deposit":2000000}""" ]
            , test "an unregistration carries the refund equal to the deposit" <|
                \_ ->
                    Wasm.certJson (withCert Unregister) { wallet = 1, action = Unregister }
                        |> List.map (E.encode 0)
                        |> Expect.equal
                            [ """{"action":"unregister","stakeKeyHash":"5ca1ab1e","deposit":2000000}""" ]
            , test "a plain delegation sends no deposit field" <|
                \_ ->
                    Wasm.certJson (withCert (DelegateOnly ref)) { wallet = 1, action = DelegateOnly ref }
                        |> List.map (E.encode 0)
                        |> List.map (D.decodeString (D.field "deposit" D.int) >> okOrErr)
                        |> Expect.equal [ "Err" ]
            ]
        ]



-- POOL LIST


poolListTests : Test
poolListTests =
    let
        base =
            State.init { keyDeposit = 2000000, coinsPerUtxoByte = 4310 } |> Tuple.first
    in
    describe "GotPools landing guard"
        (let
            page =
                Ok { pools = [], hasMore = False }

            landedPools msg =
                Update.update msg { base | pools = Loading } |> Tuple.first |> .pools
         in
         [ test "a reply for another network is dropped (init network is Preview)" <|
            \_ -> landedPools (GotPools Preprod 1 page) |> Expect.equal Loading
         , test "a reply for another page is dropped" <|
            \_ -> landedPools (GotPools Preview 2 page) |> Expect.equal Loading
         , test "the reply for the asked network and page lands" <|
            \_ -> landedPools (GotPools Preview 1 page) |> Expect.equal (Loaded { pools = [], hasMore = False })
         , test "an error reply for the asked network and page lands as Failed" <|
            \_ -> landedPools (GotPools Preview 1 (Err "boom")) |> Expect.equal (Failed "boom")
         , test "clicking a page sets the stamp the reply is checked against" <|
            \_ ->
                let
                    after =
                        Update.update (ClickPoolPage 2) (State.setCurrentKey "k" base) |> Tuple.first
                in
                ( after.poolPage, after.pools ) |> Expect.equal ( 2, Loading )
         ]
        )



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
