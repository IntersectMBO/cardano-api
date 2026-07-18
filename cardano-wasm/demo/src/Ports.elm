port module Ports exposing (..)

import Json.Decode as D
import Json.Encode as E



-- PORTS (out → cardano-wasm / clipboard)


port wasmGenerateWallet : E.Value -> Cmd msg


port wasmRestoreWallet : E.Value -> Cmd msg


port wasmDeriveAddresses : E.Value -> Cmd msg


port wasmEstimateFee : E.Value -> Cmd msg


port wasmSignTx : E.Value -> Cmd msg


port wasmInspectAddress : E.Value -> Cmd msg


port clipboardWrite : String -> Cmd msg



-- PORTS (in ← cardano-wasm)


port wasmWalletGenerated : (D.Value -> msg) -> Sub msg


port wasmWalletRestored : (D.Value -> msg) -> Sub msg


port wasmAddressesDerived : (D.Value -> msg) -> Sub msg


port wasmFeeEstimated : (D.Value -> msg) -> Sub msg


port wasmTxSigned : (D.Value -> msg) -> Sub msg


port wasmAddressInspected : (D.Value -> msg) -> Sub msg
