module Wasm exposing
    ( addrsDecoder
    , decodeResult
    , deriveAddresses
    , estimateFee
    , feeDecoder
    , genDecoder
    , generateWallet
    , inspectAddress
    , inspectedDecoder
    , restoreWallet
    , signTx
    , signedDecoder
    )

{-| The cardano-wasm boundary. Commands encode a request and send it out a port;
results come back on the matching incoming port and are decoded here (see the
subscriptions in Main). The Cardano processing itself (key handling, address
encoding) happens in web/ports.js through the cardano-wasm wrapper.
-}

import Format
import Json.Decode as D
import Json.Encode as E
import Net exposing (eraTag, netTag)
import Ports
import State exposing (..)
import Types exposing (..)



-- COMMANDS (out → cardano-wasm)


{-| Generate a fresh stake-enabled wallet on the given network.
-}
generateWallet : Network -> Cmd msg
generateWallet net =
    Ports.wasmGenerateWallet (E.object [ ( "network", E.string (netTag net) ) ])


{-| Restore a wallet from its two bech32 signing keys (the backup artifact).
-}
restoreWallet : Network -> RestoreForm -> Cmd msg
restoreWallet net form =
    Ports.wasmRestoreWallet
        (E.object
            [ ( "network", E.string (netTag net) )
            , ( "paymentSkey", E.string form.paymentSkey )
            , ( "stakeSkey", E.string form.stakeSkey )
            ]
        )


{-| Re-encode every wallet's address for a new network (keys are network-agnostic,
the bech32 address is not).
-}
deriveAddresses : Network -> List Wallet -> Cmd msg
deriveAddresses net wallets =
    Ports.wasmDeriveAddresses
        (E.object
            [ ( "network", E.string (netTag net) )
            , ( "wallets"
              , E.list
                    (\w ->
                        E.object
                            [ ( "id", E.int w.id )
                            , ( "paymentSkey", E.string w.keys.paymentSKey )
                            , ( "stakeSkey", E.string w.keys.stakeSKey )
                            ]
                    )
                    wallets
              )
            ]
        )



-- DECODERS (in ← cardano-wasm)


{-| Every incoming port payload is either the expected object or { error }.
-}
decodeResult : D.Decoder a -> D.Value -> Result String a
decodeResult dec v =
    case D.decodeValue (D.field "error" D.string) v of
        Ok e ->
            Err e

        Err _ ->
            D.decodeValue dec v |> Result.mapError D.errorToString


genDecoder : D.Decoder GenPayload
genDecoder =
    D.map2 GenPayload
        (D.field "address" D.string)
        (D.field "keys" keysDecoder)


keysDecoder : D.Decoder Keys
keysDecoder =
    D.map6 Keys
        (D.field "paymentVKey" D.string)
        (D.field "paymentSKey" D.string)
        (D.field "stakeVKey" D.string)
        (D.field "stakeSKey" D.string)
        (D.field "paymentKeyHash" D.string)
        (D.field "stakeKeyHash" D.string)


addrsDecoder : D.Decoder (List ( WalletId, String ))
addrsDecoder =
    D.list (D.map2 Tuple.pair (D.field "id" D.int) (D.field "address" D.string))


{-| Validate an address and detect its network kind.
-}
inspectAddress : String -> Cmd msg
inspectAddress addr =
    Ports.wasmInspectAddress (E.object [ ( "address", E.string addr ) ])


inspectedDecoder : D.Decoder ( String, AddrCheck )
inspectedDecoder =
    D.map2 Tuple.pair
        (D.field "address" D.string)
        (D.field "network" D.string
            |> D.map
                (\n ->
                    case n of
                        "mainnet" ->
                            CheckValid MainKind

                        "testnet" ->
                            CheckValid TestKind

                        _ ->
                            CheckInvalid
                )
        )


{-| Ask for the minimum fee of the currently described transaction.
-}
estimateFee : Model -> Cmd msg
estimateFee model =
    Ports.wasmEstimateFee
        (E.object
            [ ( "spec", encodeSpec Nothing model )
            , ( "paymentWits", E.int (List.length (paymentWalletIds model)) )
            , ( "stakeWits", E.int 0 ) -- stake witnesses arrive with certificates
            ]
        )


{-| Build, balance (fee + change already decided in Elm) and sign the transaction.
-}
signTx : Int -> Model -> Cmd msg
signTx fee model =
    Ports.wasmSignTx
        (E.object
            [ ( "spec", encodeSpec (Just fee) model )
            , ( "paymentKeys", E.list E.string (paymentSigningKeys model) )
            , ( "stakeKeys", E.list E.string [] ) -- stake keys arrive with certificates
            ]
        )



-- TX SPEC
-- The JSON description of the transaction that web/ports.js replays against the
-- cardano-wasm builder (newTx → addTxInput → addSimpleTxOut).


encodeSpec : Maybe Int -> Model -> E.Value
encodeSpec maybeFee model =
    E.object
        [ ( "era", E.string (eraTag model.era) )
        , ( "inputs"
          , E.list
                (\( _, u ) -> E.object [ ( "txId", E.string u.txId ), ( "txIx", E.int u.txIx ) ])
                (selectedInputs model)
          )
        , ( "outputs", E.list identity (finalOutputs (Maybe.withDefault 0 maybeFee) model) )
        , ( "certs", E.list identity [] ) -- certificates arrive in a later change
        , ( "fee", maybeFee |> Maybe.map E.int |> Maybe.withDefault E.null )
        ]


{-| The explicit outputs plus the computed change output (if there is a remainder).
For fee estimation the fee is still unknown (0) — the change is then slightly too
large, but the transaction _size_ is the same, which is all the estimate needs.
-}
finalOutputs : Int -> Model -> List E.Value
finalOutputs feeForChange model =
    let
        explicit =
            model.outputs
                |> List.filterMap
                    (\o ->
                        case o.amount of
                            Lovelace s ->
                                Format.adaToLovelace s
                                    |> Maybe.map (\l -> outputJson o.address l)

                            Change ->
                                Nothing
                    )

        chg =
            inputsTotal model - explicitOutputsTotal model - feeForChange

        changeOut =
            case ( changeAddress model, chg > 0 ) of
                ( Just addr, True ) ->
                    [ outputJson addr chg ]

                _ ->
                    []
    in
    explicit ++ changeOut


outputJson : String -> Int -> E.Value
outputJson addr lovelace =
    E.object [ ( "address", E.string addr ), ( "lovelace", E.int lovelace ) ]



-- SIGNING KEYS
-- Payment witnesses for the wallets whose UTxOs are spent.


paymentSigningKeys : Model -> List String
paymentSigningKeys model =
    paymentWalletIds model
        |> List.filterMap (\wid -> getWallet wid model)
        |> List.map (\w -> w.keys.paymentSKey)


feeDecoder : D.Decoder Int
feeDecoder =
    D.field "fee" D.int


signedDecoder : D.Decoder SignedPayload
signedDecoder =
    D.map2 SignedPayload
        (D.field "cbor" D.string)
        (D.field "txId" D.string)
