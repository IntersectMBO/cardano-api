# Plan: Add `+rpc` cabal flag to gate gRPC dependencies

## Context

GitHub issue: IntersectMBO/cardano-api#1167

The `cardano-rpc` package pulls in heavy dependencies (`grapesy`, `grpc-spec`, `proto-lens`) that are always linked into `cardano-node` builds, even for block producers that never enable RPC.
A `+rpc` flag (off by default, manual) should gate these dependencies so builds without it exclude them entirely.

**Critical constraint**: CPP breaks fourmolu.
We must limit CPP to the absolute minimum number of modules -- ideally **one new file**.

## Architecture

The work spans two repositories:

1. **cardano-api** (`cardano-rpc/`) -- add `trace-dispatcher` dep, upstream tracing instances
2. **cardano-node** -- add `+rpc` flag with a single CPP shim module

### Key insight: upstream tracing instances to eliminate orphans

Currently `LogFormatting TraceRpc` and `MetaTrace TraceRpc` are orphan instances defined in `cardano-node` (`Tracing/Tracers/Rpc.hs`).
The classes live in `trace-dispatcher` (`Cardano.Logging.Types`, lines 151-183).
The types live in `cardano-rpc` (`Cardano.Rpc.Server.Internal.Tracing`).

By making `cardano-rpc` depend on `trace-dispatcher`, it can define the instances itself -- right next to the type definitions.
This eliminates all orphan instances and simplifies the `cardano-node` shim to pure re-exports.

### trace-dispatcher package info

- Source: `/work/hermod-tracing/trace-dispatcher/` (also published to CHaP)
- Version: 2.12.0
- `LogFormatting` class: `Cardano.Logging.Types` line 151; methods: `forMachine`, `forHuman`, `asMetrics`
- `MetaTrace` class: `Cardano.Logging.Types` line 171; methods: `namespaceFor`, `severityFor`, `privacyFor`, `detailsFor`, `documentFor`, `metricsDocFor`, `allNamespaces`
- Supporting types: `Namespace`, `SeverityS`, `Privacy`, `DetailLevel`, `Metric`
- Re-export module: `Cardano.Logging` re-exports everything from `Cardano.Logging.Types`
- Dependencies: `aeson`, `text`, `containers`, `contra-tracer`, `serialise`, `deepseq`, `stm`, `network`, `hostname`, etc.

---

## Part 1: Upstream tracing instances to `cardano-rpc`

**Repo**: cardano-api (`cardano-rpc/`)

### 1.1 Add `trace-dispatcher` dependency

**File**: `cardano-rpc/cardano-rpc.cabal` (line 72-94, library build-depends)

Add `trace-dispatcher,` to the build-depends list.
Most of its transitive deps are already deps of `cardano-rpc` through `cardano-api`.

Also need to add `trace-dispatcher` to `cabal.project` in cardano-api if not already present (check if hermod-tracing is a source-repository-package or CHaP dependency).

### 1.2 Move instances into `Cardano.Rpc.Server.Internal.Tracing`

**File**: `cardano-rpc/src/Cardano/Rpc/Server/Internal/Tracing.hs`

This file currently defines the trace types (88 lines).
It already has the necessary language extensions and imports `Cardano.Api.Pretty` (for `pretty`, `prettyException`, `pshow`, `docToText`).

**Add these imports:**
```haskell
import Cardano.Logging (LogFormatting (..), MetaTrace (..), Metric (..), Namespace (..), SeverityS (..))
import Data.Aeson (Object, Value (..), (.=))
```

**Add these instances** (move from `cardano-node/cardano-node/src/Cardano/Node/Tracing/Tracers/Rpc.hs`):

```haskell
instance LogFormatting TraceRpc where
  forMachine _dtal tr =
    mconcat $
      ("reason" .= prettyShow tr)
        : case tr of
          TraceRpcFatalError _ -> ["kind" .= String "FatalError"]
          TraceRpcError _ -> ["kind" .= String "Error"]
          TraceRpcQuery queryTrace ->
            ["kind" .= String "QueryService"]
              <> case queryTrace of
                TraceRpcQueryParamsSpan s ->
                  [ "queryName" .= String "ReadParams"
                  , spanToObject s
                  ]
                TraceRpcQueryReadUtxosSpan s ->
                  [ "queryName" .= String "ReadUtxos"
                  , spanToObject s
                  ]
          TraceRpcSubmit submitTrace ->
            ["kind" .= String "SubmitService"]
              <> case submitTrace of
                TraceRpcSubmitN2cConnectionError _ -> []
                TraceRpcSubmitTxDecodingError _ -> []
                TraceRpcSubmitTxValidationError _ -> []
                TraceRpcSubmitSpan s -> [spanToObject s]

  forHuman = docToText . pretty

  asMetrics = \case
    TraceRpcQuery (TraceRpcQueryParamsSpan (SpanBegin _)) -> [CounterM "rpc.request.QueryService.ReadParams" Nothing]
    TraceRpcQuery (TraceRpcQueryReadUtxosSpan (SpanBegin _)) -> [CounterM "rpc.request.QueryService.ReadUtxos" Nothing]
    TraceRpcSubmit (TraceRpcSubmitSpan (SpanBegin _)) -> [CounterM "rpc.request.SubmitService.SubmitTx" Nothing]
    _ -> []

instance MetaTrace TraceRpc where
  namespaceFor =
    Namespace [] . \case
      TraceRpcFatalError _ -> ["FatalError"]
      TraceRpcError _ -> ["Error"]
      TraceRpcQuery queryTrace ->
        "QueryService"
          : case queryTrace of
            TraceRpcQueryParamsSpan _ -> ["ReadParams", "Span"]
            TraceRpcQueryReadUtxosSpan _ -> ["ReadUtxos", "Span"]
      TraceRpcSubmit submitTrace ->
        "SubmitService"
          : case submitTrace of
            TraceRpcSubmitN2cConnectionError _ -> ["N2cConnectionError"]
            TraceRpcSubmitTxDecodingError _ -> ["TxDecodingError"]
            TraceRpcSubmitTxValidationError _ -> ["TxValidationError"]
            TraceRpcSubmitSpan _ -> ["SubmitTx", "Span"]

  severityFor (Namespace _ nsInner) _ = case nsInner of
    ["FatalError"] -> Just Error
    ["Error"] -> Just Debug
    ["QueryService", "ReadParams", "Span"] -> Just Debug
    ["QueryService", "ReadUtxos", "Span"] -> Just Debug
    ["SubmitService", "SubmitTx", "Span"] -> Just Debug
    ["SubmitService", "N2cConnectionError"] -> Just Warning
    ["SubmitService", "TxDecodingError"] -> Just Debug
    ["SubmitService", "TxValidationError"] -> Just Debug
    _ -> Nothing

  documentFor (Namespace _ nsInner) = case nsInner of
    ["FatalError"] -> Just "RPC startup critical error."
    ["Error"] -> Just "Normal operation errors such as request errors. Those are not harmful to the RPC server itself."
    ["QueryService", "ReadParams", "Span"] -> Just "Span for the ReadParams UTXORPC method."
    ["QueryService", "ReadUtxos", "Span"] -> Just "Span for the ReadUtxos UTXORPC method."
    ["SubmitService", "SubmitTx", "Span"] -> Just "Span for the SubmitTx UTXORPC method."
    ["SubmitService", "N2cConnectionError"] ->
      Just
        "Node connection error. This should not happen, as this means that there is an issue in cardano-rpc configuration."
    ["SubmitService", "TxDecodingError"] -> Just "A regular request error, when submitted transaction decoding fails."
    ["SubmitService", "TxValidationError"] -> Just "A regular request error, when submitted transaction is invalid."
    _ -> Nothing

  metricsDocFor (Namespace _ nsInner) = case nsInner of
    ["QueryService", "ReadParams", "Span"] ->
      [("rpc.request.QueryService.ReadParams", "Span for the ReadParams UTXORPC method.")]
    ["QueryService", "ReadUtxos", "Span"] ->
      [("rpc.request.QueryService.ReadUtxos", "Span for the ReadUtxos UTXORPC method.")]
    ["SubmitService", "SubmitTx", "Span"] ->
      [("rpc.request.SubmitService.SubmitTx", "Span for the SubmitTx UTXORPC method.")]
    _ -> []

  allNamespaces =
    Namespace []
      <$> [ ["FatalError"]
          , ["Error"]
          , ["QueryService", "ReadParams", "Span"]
          , ["QueryService", "ReadUtxos", "Span"]
          , ["SubmitService", "SubmitTx", "Span"]
          , ["SubmitService", "N2cConnectionError"]
          , ["SubmitService", "TxDecodingError"]
          , ["SubmitService", "TxValidationError"]
          ]

spanToObject :: TraceSpanEvent -> Object
spanToObject =
  mconcat . \case
    SpanBegin spanId -> ["span" .= String "begin", "spanId" .= spanId]
    SpanEnd spanId -> ["span" .= String "end", "spanId" .= spanId]
```

**Note**: `prettyShow` is from `Cardano.Api.Pretty` (already imported).
`docToText` is from `Cardano.Api.Pretty` (already imported).
`SeverityS` constructors like `Error`, `Debug`, `Warning` come from `Cardano.Logging`.

### 1.3 No changes to `Cardano.Rpc.Server` re-exports

`Cardano.Rpc.Server` (line 11-19) already re-exports `TraceRpc(..)`, `TraceRpcSubmit(..)`, `TraceRpcQuery(..)`, `TraceSpanEvent(..)`.
The `LogFormatting`/`MetaTrace` instances travel with the types automatically.

---

## Part 2: `+rpc` flag in `cardano-node`

### 2.1 Add flag and conditional deps in `cardano-node.cabal`

**File**: `cardano-node/cardano-node/cardano-node.cabal`

Add flag after line 25 (after the `systemd` flag):
```cabal
flag rpc
  description:  Enable gRPC endpoint support (adds cardano-rpc, grapesy, grpc-spec, proto-lens dependencies)
  default:      False
  manual:       True
```

In the `library` section (lines 49-213):
- Remove line 158: `cardano-rpc ^>= 10.2` from unconditional `build-depends`
- Add conditional block after line 59 (after systemd conditional):
  ```cabal
  if flag(rpc)
    cpp-options:   -DRPC
    build-depends: cardano-rpc ^>= 10.2
  ```
- In `exposed-modules`, replace line 114 `Cardano.Node.Tracing.Tracers.Rpc` with `Cardano.Node.Rpc`

In `test-suite cardano-node-test` (lines 237-280):
- Change line 250 `cardano-rpc` to be conditional:
  ```cabal
  if flag(rpc)
    build-depends: cardano-rpc
  ```

### 2.2 Create `Cardano.Node.Rpc` -- the single CPP shim

**New file**: `cardano-node/cardano-node/src/Cardano/Node/Rpc.hs`

This is the **only** new CPP module.
Since instances are now in `cardano-rpc` itself, the `+rpc` branch is just re-exports.

```haskell
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StandaloneDeriving #-}

module Cardano.Node.Rpc
  ( TraceRpc
  , RpcConfig
  , PartialRpcConfig
  , RpcConfigF (..)
  , makeRpcConfig
  , nodeSocketPathToRpcSocketPath
  , runRpcServer
  ) where

#ifdef RPC

import           Cardano.Rpc.Server (TraceRpc, runRpcServer)
import           Cardano.Rpc.Server.Config (RpcConfig, PartialRpcConfig, RpcConfigF (..),
                   makeRpcConfig, nodeSocketPathToRpcSocketPath)

#else

import           Cardano.Api (File (..), SocketPath)

import           "contra-tracer" Control.Tracer (Tracer)
import           Control.Monad (when)
import           Control.Monad.Except (MonadError, throwError)
import           Data.Functor.Identity (Identity)
import           Data.Maybe (fromMaybe, isNothing)
import           Data.Monoid (Last (..))
import           Data.Void (Void)
import           GHC.Generics (Generic)
import           Generic.Data (gmappend, gmempty)
import           Ouroboros.Network.Magic (NetworkMagic)
import           System.FilePath (takeDirectory, (</>))

type TraceRpc = Void

type PartialRpcConfig = RpcConfigF Last

type RpcConfig = RpcConfigF Identity

data RpcConfigF m = RpcConfig
  { isEnabled :: !(m Bool)
  , rpcSocketPath :: !(m SocketPath)
  , nodeSocketPath :: !(m SocketPath)
  }

deriving instance Show (RpcConfigF Identity)
deriving instance Eq (RpcConfigF Identity)
deriving instance Show (RpcConfigF Last)
deriving instance Eq (RpcConfigF Last)
deriving instance Generic (RpcConfigF Last)

instance Semigroup (RpcConfigF Last) where
  (<>) = gmappend

instance Monoid (RpcConfigF Last) where
  mempty = gmempty

makeRpcConfig
  :: MonadError String m
  => PartialRpcConfig
  -> m RpcConfig
makeRpcConfig
  RpcConfig
    { isEnabled = Last mIsEnabled
    , rpcSocketPath = Last mRpcSocketPath
    , nodeSocketPath = Last mNodeSocketPath
    } = do
    let isEnabled = fromMaybe False mIsEnabled
        nodeSocketPath = fromMaybe "./node.socket" mNodeSocketPath
        rpcSocketPath = fromMaybe (nodeSocketPathToRpcSocketPath nodeSocketPath) mRpcSocketPath
    when (isEnabled && isNothing mNodeSocketPath) $
      throwError
        "Configuration error: gRPC endpoint was enabled but node socket file was not specified. Cannot run gRPC server without node socket."
    pure $
      RpcConfig
        (pure isEnabled)
        (pure rpcSocketPath)
        (pure nodeSocketPath)

nodeSocketPathToRpcSocketPath :: SocketPath -> SocketPath
nodeSocketPathToRpcSocketPath nodeSocketPath = do
  let socketDir = takeDirectory $ unFile nodeSocketPath
  File $ socketDir </> "rpc.sock"

runRpcServer :: Tracer IO TraceRpc -> (RpcConfig, NetworkMagic) -> IO ()
runRpcServer _ _ = pure ()

#endif
```

### 2.3 Add `MetaTrace Void` instance

**File**: `cardano-node/cardano-node/src/Cardano/Node/Tracing/Formatting.hs`

Currently 76 lines.
Add import of `MetaTrace` and `Namespace` (line 12 currently imports `LogFormatting` only):

Change:
```haskell
import           Cardano.Logging (LogFormatting (..))
```
To:
```haskell
import           Cardano.Logging (LogFormatting (..), MetaTrace (..), Namespace (..))
```

Add after the `LogFormatting Void` instance (after line 28):
```haskell
instance MetaTrace Void where
  namespaceFor = absurd
  severityFor _ _ = Nothing
  documentFor _ = Nothing
  allNamespaces = []
```

Also add `absurd` import -- `Data.Void` is already imported on line 21 but only imports `Void`.
Change:
```haskell
import           Data.Void (Void)
```
To:
```haskell
import           Data.Void (Void, absurd)
```

### 2.4 Update imports in consuming modules (no CPP changes)

**2.4.1 `cardano-node/cardano-node/src/Cardano/Node/Configuration/POM.hs`**

Line 40-41, change:
```haskell
import           Cardano.Rpc.Server.Config (PartialRpcConfig, RpcConfig, RpcConfigF (..),
                   makeRpcConfig)
```
To:
```haskell
import           Cardano.Node.Rpc (PartialRpcConfig, RpcConfig, RpcConfigF (..),
                   makeRpcConfig)
```

**2.4.2 `cardano-node/cardano-node/src/Cardano/Node/Parsers.hs`**

Line 26, change:
```haskell
import           Cardano.Rpc.Server.Config (PartialRpcConfig, RpcConfigF (..))
```
To:
```haskell
import           Cardano.Node.Rpc (PartialRpcConfig, RpcConfigF (..))
```

**2.4.3 `cardano-node/cardano-node/src/Cardano/Node/Tracing.hs`**

Line 21, change:
```haskell
import           Cardano.Rpc.Server (TraceRpc)
```
To:
```haskell
import           Cardano.Node.Rpc (TraceRpc)
```

**2.4.4 `cardano-node/cardano-node/src/Cardano/Node/Run.hs`**

Lines 58-59, change:
```haskell
import           Cardano.Rpc.Server
import           Cardano.Rpc.Server.Config
```
To:
```haskell
import           Cardano.Node.Rpc (runRpcServer)
```

Note: `Run.hs` already has `{-# LANGUAGE CPP #-}` (line 3) for `#ifdef UNIX`. No new CPP needed -- we are just changing the import.

**2.4.5 `cardano-node/cardano-node/src/Cardano/Node/Tracing/Tracers.hs`**

Line 39, remove:
```haskell
import           Cardano.Node.Tracing.Tracers.Rpc ()
```

(Orphan instances are no longer needed -- they come from `cardano-rpc` transitively via `Cardano.Node.Tracing` -> `Cardano.Node.Rpc` -> `Cardano.Rpc.Server`.)

**2.4.6 `cardano-node/cardano-node/src/Cardano/Node/Tracing/Documentation.hs`**

Line 52, remove:
```haskell
import           Cardano.Node.Tracing.Tracers.Rpc ()
```

Line 55, change:
```haskell
import           Cardano.Rpc.Server (TraceRpc)
```
To:
```haskell
import           Cardano.Node.Rpc (TraceRpc)
```

**2.4.7 `cardano-node/cardano-node/src/Cardano/Node/Tracing/Consistency.hs`**

Line 40, remove:
```haskell
import           Cardano.Node.Tracing.Tracers.Rpc ()
```

Line 43, change:
```haskell
import           Cardano.Rpc.Server (TraceRpc)
```
To:
```haskell
import           Cardano.Node.Rpc (TraceRpc)
```

**2.4.8 `cardano-node/cardano-node/src/Cardano/Tracing/Tracers.hs`** (legacy)

Line 53, remove:
```haskell
import           Cardano.Node.Tracing.Tracers.Rpc ()
```

**2.4.9 `cardano-node/cardano-node/test/Test/Cardano/Node/POM.hs`**

Line 21, change:
```haskell
import           Cardano.Rpc.Server.Config (makeRpcConfig)
```
To:
```haskell
import           Cardano.Node.Rpc (makeRpcConfig)
```

### 2.5 Delete `Cardano.Node.Tracing.Tracers.Rpc`

**Delete**: `cardano-node/cardano-node/src/Cardano/Node/Tracing/Tracers/Rpc.hs`

Its 126 lines of content (orphan `LogFormatting TraceRpc`, `MetaTrace TraceRpc` instances, `spanToObject` helper) are now in `cardano-rpc`'s `Cardano.Rpc.Server.Internal.Tracing`.

### 2.6 `cardano-testnet` -- no source changes needed

`cardano-testnet` keeps its unconditional dependency on `cardano-rpc` (line 62 of cardano-testnet.cabal for library, line 266 for test suite).
It uses the **client** side (`Cardano.Rpc.Client`, proto types) to test a running node -- this compiles regardless of cardano-node's `+rpc` flag.
Its library code imports `nodeSocketPathToRpcSocketPath` directly from `Cardano.Rpc.Server.Config` (Testnet/Types.hs line 57) -- still works.
No flag, no CPP needed in cardano-testnet.

RPC test files (unchanged):
- `cardano-testnet/test/cardano-testnet-test/Cardano/Testnet/Test/Rpc/Query.hs`
- `cardano-testnet/test/cardano-testnet-test/Cardano/Testnet/Test/Rpc/Transaction.hs`

Test main module (unchanged): `cardano-testnet/test/cardano-testnet-test/cardano-testnet-test.hs` lines 140-143.

### 2.7 Flag is off by default everywhere

The `rpc` flag is `manual: True, default: False` in `cardano-node.cabal`.
No override in `cabal.project`.

To build with RPC (needed for testnet tests):
```
cabal build cardano-node -f +rpc
cabal test cardano-testnet-test -f +rpc
```

CI must set `-f +rpc` on jobs that run testnet RPC tests.

---

## CPP module summary

| Module | CPP before | CPP after |
|--------|-----------|-----------|
| `Run.hs` | `UNIX` | `UNIX` (unchanged) |
| `Configuration/Logging.hs` | `SYSTEMD` | `SYSTEMD` (unchanged) |
| `Configuration/Socket.hs` | `SYSTEMD`, `mingw32` | unchanged |
| **`Cardano.Node.Rpc`** | N/A (new) | **`RPC`** |

**New CPP files: 1.  Total CPP files: 4 (was 3).**

---

## Verification

1. **Without flag** (default): `cabal build cardano-node` -- must NOT pull in `cardano-rpc`/`grapesy`/`grpc-spec`/`proto-lens`.
   Verify: `cabal build cardano-node --dry-run 2>&1 | grep -E 'cardano-rpc|grapesy|grpc-spec|proto-lens'` -- no output.
2. **With flag**: `cabal build cardano-node -f +rpc` -- full RPC support.
3. **Tests without flag**: `cabal test cardano-node-test` -- passes, RPC tests skipped.
4. **Tests with flag**: `cabal test cardano-node-test -f +rpc` and `cabal test cardano-testnet-test -f +rpc` -- all pass.
5. **cardano-rpc standalone**: `cabal build cardano-rpc` in cardano-api -- still builds, now includes tracing instances.

---

## Execution order

1. **cardano-api/cardano-rpc**: Add `trace-dispatcher` dep, upstream tracing instances (PR to cardano-api)
2. **cardano-node**: Add `+rpc` flag, create shim, update imports (PR to cardano-node, depends on step 1)

Step 1 must land (or be available as a source-repository-package) before step 2.

---

## Gotchas and things to watch for

- `RpcConfigF` uses `NoFieldSelectors` extension in cardano-rpc.
  The stub in the `-rpc` branch of the shim must also use it (or verify no field name conflicts in cardano-node).
- `prettyShow` used in the `LogFormatting TraceRpc` instance comes from `Cardano.Api.Pretty`, not from `Prelude`.
  It is already imported in `Cardano.Rpc.Server.Internal.Tracing`.
- `SeverityS` constructors (`Error`, `Debug`, `Warning`) from `Cardano.Logging` may shadow Prelude.
  Use qualified import if needed: `import Cardano.Logging (SeverityS(..))`.
- The `-rpc` stub's `makeRpcConfig` must produce identical behaviour to the real one (same defaults, same validation logic).
  The implementation above is copied verbatim from `cardano-rpc/src/Cardano/Rpc/Server/Config.hs`.
- `Run.hs` line 572 calls `runRpcServer (rpcTracer tracers) (ncRpcConfig nc, networkMagic)`.
  After the change it imports `runRpcServer` from `Cardano.Node.Rpc`.
  In the `-rpc` case, `runRpcServer` is a no-op, `ncRpcConfig` has the stub `RpcConfig` type -- types align.
- `Run.hs` also uses `RpcConfig` pattern matching indirectly through `ncRpcConfig nc`.
  The `NodeConfiguration` record field `ncRpcConfig :: RpcConfig` will use the stub type when `-rpc` -- this is correct since `POM.hs` also imports from the shim.
- `Cardano.Node.Run` currently imports `Cardano.Logging.Types (LogFormatting)` (line 71).
  After removing the `Cardano.Rpc.Server` import, verify the `LogFormatting` import is still used (it is, for the `LogFormatting` constraint on `runNode`).
