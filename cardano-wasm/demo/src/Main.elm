module Main exposing (main)

{-| Entry point — wires The Elm Architecture together. The interesting code lives in:

  - Types — every data type, the Model and the Msg
  - State — initial state, derived queries, small updaters
  - Update — the controller (one branch per Msg)
  - View — the whole UI
  - Wasm — the cardano-wasm boundary (port commands + decoders)
  - Net / Format — static tables and pure utilities
  - Ports — the raw port declarations (JS side: web/ports.js)

-}

import Browser
import Ports
import State exposing (init)
import Types exposing (..)
import Update exposing (update)
import View exposing (view)
import Wasm


{-| Each incoming port is decoded by Wasm and dispatched as a Msg.
-}
subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Ports.wasmWalletGenerated (Wasm.decodeResult Wasm.genDecoder >> GotGeneratedWallet)
        , Ports.wasmWalletRestored (Wasm.decodeResult Wasm.genDecoder >> GotRestoredWallet)
        , Ports.wasmAddressesDerived (Wasm.decodeResult Wasm.addrsDecoder >> GotDerivedAddresses)
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
