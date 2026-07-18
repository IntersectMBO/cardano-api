port module Ports exposing (..)

{-| The raw port declarations — the only holes in the wall between Elm and
JavaScript. The JS side lives in web/ports.js. Payloads are untyped JSON;
Wasm.elm encodes the requests and decodes the replies.
-}

import Json.Decode as D
import Json.Encode as E



-- PORTS (out → cardano-wasm / clipboard)


port wasmGenerateWallet : E.Value -> Cmd msg


port wasmRestoreWallet : E.Value -> Cmd msg


port wasmDeriveAddresses : E.Value -> Cmd msg


port wasmInspectAddress : E.Value -> Cmd msg


port clipboardWrite : String -> Cmd msg



-- PORTS (in ← cardano-wasm)


port wasmWalletGenerated : (D.Value -> msg) -> Sub msg


port wasmWalletRestored : (D.Value -> msg) -> Sub msg


port wasmAddressesDerived : (D.Value -> msg) -> Sub msg


port wasmAddressInspected : (D.Value -> msg) -> Sub msg
