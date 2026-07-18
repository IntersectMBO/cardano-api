module Wasm exposing
    ( addrsDecoder
    , decodeResult
    , deriveAddresses
    , genDecoder
    , generateWallet
    , restoreWallet
    )

{-| The cardano-wasm boundary. Commands encode a request and send it out a port;
results come back on the matching incoming port and are decoded here (see the
subscriptions in Main). The Cardano processing itself (key handling, address
encoding) happens in web/ports.js through the cardano-wasm wrapper.
-}

import Json.Decode as D
import Json.Encode as E
import Net exposing (netTag)
import Ports
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
