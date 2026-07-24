# Build Instructions and Conventions

Project layout, build commands, and codebase conventions for the node kernel access work.

---

## 0. Project Layout

The working directory is `/work/`.
It contains git submodules, each a separate Haskell project with its own flake/cabal setup.
Only the submodules relevant to this task are listed here.

### Submodules relevant to this task

```
/work/
├── cardano-api/                    ← cardano-api repo (contains cardano-rpc)
│   ├── cardano-api/                  ← the cardano-api library package
│   │   └── src/Cardano/Api/          ← cardano-api source (Query, Network.IPC, Era, Consensus, etc.)
│   └── cardano-rpc/                  ← the cardano-rpc package (THIS IS THE MAIN TARGET)
│       ├── src/Cardano/Rpc/
│       │   ├── Client.hs             ← gRPC client (not modified)
│       │   ├── Proto/Api/            ← proto-lens API wrappers (not modified)
│       │   └── Server/
│       │       ├── Config.hs         ← RpcConfig (kept unchanged)
│       │       ├── Server.hs         ← runRpcServer entry point (MODIFIED)
│       │       └── Internal/
│       │           ├── Env.hs        ← RpcEnv record (MODIFIED)
│       │           ├── Monad.hs      ← Has typeclass, MonadRpc (MODIFIED)
│       │           ├── Error.hs      ← throwEither, throwExceptT (kept)
│       │           ├── Node.hs       ← getEra, getProtocolParamsJson (MODIFIED)
│       │           ├── Tracing.hs    ← TraceRpc types (MODIFIED)
│       │           ├── NodeKernelAccess.hs ← NEW FILE
│       │           ├── Orphans.hs    ← (not modified)
│       │           └── UtxoRpc/
│       │               ├── Query.hs  ← readParams, readUtxos, searchUtxos (MODIFIED)
│       │               ├── Submit.hs ← submitTx (MODIFIED)
│       │               ├── Type.hs   ← protobuf conversion (not modified)
│       │               └── Predicate.hs ← UTxO filtering (not modified)
│       ├── gen/                      ← generated proto-lens code (NEVER edit by hand)
│       ├── proto/                    ← .proto definitions
│       ├── test/                     ← unit tests
│       └── cardano-rpc.cabal        ← (MODIFIED - add NodeKernelAccess module)
│
├── cardano-node/                   ← cardano-node repo
│   ├── cardano-node/                ← the cardano-node package
│   │   ├── src/Cardano/Node/
│   │   │   ├── Run.hs              ← node startup, withAsync runRpcServer (MODIFIED)
│   │   │   ├── Queries.hs          ← existing nkQueryLedger pattern (reference only)
│   │   │   ├── Rpc/
│   │   │   │   └── NodeKernelAccess.hs ← NEW FILE - mkNodeKernelAccess from NodeKernel
│   │   │   └── Tracing/Tracers/
│   │   │       └── Rpc.hs          ← LogFormatting/MetaTrace instances (MODIFIED)
│   │   └── cardano-node.cabal      ← (MODIFIED - add new module)
│   ├── cardano-testnet/             ← integration test infrastructure
│   │   └── test/Cardano/Testnet/Test/Rpc/ ← gRPC integration tests (not modified)
│   └── @worktree/add-grpc-interface/ ← worktree for the gRPC feature branch
│
├── ouroboros-consensus/            ← consensus repo (READ ONLY - verify APIs here)
│   ├── ouroboros-consensus/src/ouroboros-consensus/Ouroboros/Consensus/
│   │   ├── Ledger/Query.hs         ← answerQuery function
│   │   ├── Ledger/Tables/          ← MapKind, LedgerTables definitions
│   │   ├── Storage/ChainDB/API.hs  ← getReadOnlyForkerAtPoint, getCurrentLedger
│   │   ├── Storage/LedgerDB/       ← Forker, BackingStore
│   │   ├── Mempool/API.hs          ← addLocalTxs
│   │   └── MiniProtocol/LocalStateQuery/Server.hs ← existing N2C query server (reference)
│   ├── ouroboros-consensus-diffusion/src/.../
│   │   ├── NodeKernel.hs           ← NodeKernel type definition
│   │   └── Network/NodeToClient.hs ← existing N2C wiring (reference)
│   └── ouroboros-consensus-cardano/src/shelley/.../
│       └── Shelley/Ledger/Query.hs ← Shelley-specific query answering
│
├── cardano-ledger/                 ← ledger repo (READ ONLY)
│   └── eras/shelley/impl/src/Cardano/Ledger/Shelley/LedgerState.hs
│
├── cardano-node-wiki/              ← documentation (THIS DOCUMENTATION SET)
│   └── docs/
│       ├── ADR-018-cardano-rpc-grpc-server.md
│       ├── ADR-019-node-kernel-access-for-cardano-rpc.md
│       ├── implementation-plan.md
│       ├── analysis-architecture.md
│       ├── analysis-utxohd-internals.md
│       ├── analysis-consensus-protocol.md
│       ├── prereqs-api-signatures.md
│       ├── prereqs-build-and-conventions.md       ← THIS FILE
│       └── prereqs-implementation-details.md
│
└── ouroboros-network/              ← network repo (READ ONLY - RemoteAddress, etc.)
```

### Key relationships

- **cardano-rpc** depends on **cardano-api** (types, re-exports) but NOT on consensus
- **cardano-node** depends on both **cardano-rpc** and **ouroboros-consensus**
- The `NodeKernelAccess` record lives in **cardano-rpc** (cardano-api types only)
- The `mkNodeKernelAccess` implementation lives in **cardano-node** (knows consensus types)
- This is dependency inversion: cardano-rpc defines the interface, cardano-node implements it

### NodeKernelAccess type (snapshot-based design)

Note: this snapshot design is forward-looking - it is the target interface for pieces 4-7, not current code (as built, `NodeKernelAccess` is the thinner `chainDb` / `systemStart` / `readEraHistory` record; see `01-node-access-types.md`).

```haskell
data NodeKernelAccess = NodeKernelAccess
  { nkaWithSnapshot :: forall a. (LedgerSnapshot -> IO a) -> IO a
  , nkaSubmitTx     :: TxInMode -> IO (SubmitResult TxValidationErrorInCardanoMode)
  }

newtype LedgerSnapshot = LedgerSnapshot
  { runQuery :: forall result. QueryInMode result -> IO result }
```

`nkaWithSnapshot` acquires a single `ReadOnlyForker` and provides a `LedgerSnapshot` that
runs any `QueryInMode` against that forker.
All queries within one `nkaWithSnapshot` call see the same ledger state.
`nkaSubmitTx` goes directly to the mempool and does not need a snapshot.

### Path convention in the implementation plan

The implementation plan uses paths relative to `/work/`:
- `cardano-api/cardano-rpc/src/...` means `/work/cardano-api/cardano-rpc/src/...`
- `cardano-node/cardano-node/src/...` means `/work/cardano-node/cardano-node/src/...`

---

## 2. Nix Build Instructions

### The git submodule problem

`/work` contains git submodules.
Each submodule's `.git` file points to a parent modules directory that doesn't exist in this environment.
This breaks `nix build .#...` because nix's git fetcher fails.

### Workaround: use `path:` instead of `.`

```bash
# In cardano-api submodule:
cd /work/cardano-api
nix build 'path:/work/cardano-api#cardano-rpc:lib:cardano-rpc' \
  --allow-import-from-derivation \
  --accept-flake-config

# In cardano-node submodule:
cd /work/cardano-node
nix build 'path:/work/cardano-node#cardano-node:lib:cardano-node' \
  --allow-import-from-derivation \
  --accept-flake-config
```

### Required flags (always)

- `--allow-import-from-derivation` - haskell.nix needs IFD to enumerate packages
- `--accept-flake-config` - to trust the flake's nixConfig settings

### Package names (haskell.nix component style)

| Package | Nix attribute |
|---------|---------------|
| cardano-rpc library | `cardano-rpc:lib:cardano-rpc` |
| cardano-rpc gen library | `cardano-rpc:lib:gen` |
| cardano-rpc tests | `cardano-rpc:test:cardano-rpc-test` |
| cardano-api library | `cardano-api:lib:cardano-api` |
| cardano-node library | `cardano-node:lib:cardano-node` |

### Listing all packages

```bash
nix eval 'path:/work/cardano-api#packages.x86_64-linux' --apply 'builtins.attrNames' \
  --allow-import-from-derivation --accept-flake-config
```

### Running code generation (proto-lens)

Never manually edit files in `gen/`.
Use:
```bash
nix develop --command bash -c "cd cardano-rpc && buf generate proto"
```

---

## 3. Codebase Conventions and Gotchas

### RIO import hiding

RIO re-exports most of Prelude but **hides some common functions**:
- `sortBy` - import from `Data.List`
- `on` - import from `Data.Function` or `Data.Ord`
- `toList` - import from `GHC.IsList` (not `Data.Foldable`)

Always check what RIO re-exports before assuming a function is in scope.

### Proto wrapper: `Proto msg`

`Proto` is a grapesy newtype wrapper.
Rules:
- Handler signatures use `Proto` in parameters and return types
- Internal functions use **plain proto-lens types** (unwrapped)
- Use `getProto` / `fmap getProto` only at the RPC handler boundary
- Never wrap intermediate values in `Proto`

### `Inject` typeclass

After removing `import Cardano.Api` from `Monad.hs`, the `Inject` typeclass (used by
`putTrace`) must be imported explicitly:
```haskell
import Cardano.Api.Era (Inject (..))
```

### hlint preferences

- Prefer backtick-infix sections over lambdas: `` (`f` y) `` not `\x -> f x y`
- Don't mix styles in the same function
- hlint will catch these

### GHC version

The project uses GHC 9.10 (from the `inotifywait.sh` commit: "bump ghc to 9.10").

### `-Wunused-packages` is enabled

The cabal file has `-Wunused-packages`.
If you remove an import that was the only use of a dependency, the build will fail.
Conversely, don't add deps without checking if they're already transitively available.

### `-Wredundant-constraints` is enabled

Don't add typeclass constraints that aren't actually used.
The `IsEra` constraint mistake in the past was caught by this.

---

## 6. Directory layout and worktrees

### Main checkout paths

The cardano-node files are available in the main checkout:
```
/work/cardano-node/cardano-node/src/Cardano/Node/Run.hs
/work/cardano-node/cardano-node/src/Cardano/Node/Tracing/Tracers/Rpc.hs
/work/cardano-node/cardano-node/cardano-node.cabal
```

The cardano-rpc files are in the cardano-api submodule:
```
/work/cardano-api/cardano-rpc/src/Cardano/Rpc/Server/...
/work/cardano-api/cardano-rpc/cardano-rpc.cabal
```

### Worktree for the gRPC feature branch

A worktree exists at `/work/cardano-node/@worktree/add-grpc-interface/`.
This is the branch where the gRPC feature was originally developed.
The analysis file references files in this worktree path - they may differ from the main checkout if the branch has been merged or rebased.

**When implementing:** Check whether to work in the main checkout or the worktree, depending on the current git branch state.
Use `git branch` and `git log` to determine which is current.

### Worktree rules (from AGENTS.md)

Worktrees ALWAYS reside in each subproject's `@worktree/` directory.
After creating a git worktree inside a submodule, update its `.git` configuration to use relative paths.
`git worktree add` writes absolute paths in submodules in two places that must both be fixed:
1. `<worktree>/.git` - the `gitdir:` line pointing to the worktree metadata
2. `.git/modules/<submodule>/worktrees/<name>/gitdir` - the back-pointer to the worktree
