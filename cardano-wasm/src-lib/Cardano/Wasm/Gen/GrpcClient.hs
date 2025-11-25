module Cardano.Wasm.Gen.GrpcClient where

-- | The bridge function signature.
-- Arguments: ServiceName -> MethodName -> JSONPayload -> IO JSONResponse
type GrpcExecutor = String -> String -> String -> IO String
