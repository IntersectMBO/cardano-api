# cardano-rpc vs cardano-node-api (Blink Labs)

Comparison of the Haskell `cardano-rpc` implementation (this repo) with the Go
[cardano-node-api](https://github.com/blinklabs-io/cardano-node-api) by Blink Labs.

**TL;DR** — Both projects expose Cardano node data over gRPC using the UTxO RPC
spec, but they differ in scope, deployment model, and maturity.
**cardano-node-api** is a standalone Go sidecar that implements all four
Ouroboros mini-protocols (ChainSync, LocalStateQuery, LocalTxMonitor,
LocalTxSubmission) and offers a broad feature set: REST API with Swagger,
WebSocket chain-sync streaming, mempool monitoring, SearchUtxos, and multiple
gRPC server-streaming endpoints — but has no built-in security and must sit
behind a reverse proxy. **cardano-rpc** is a Haskell library embedded directly inside cardano-node,
sharing the same `cardano-api`/`cardano-ledger` types as the node itself, and
plugging into the node's tracing/metrics infrastructure. It currently implements a smaller surface
(ReadParams, ReadUtxos, SubmitTx) with typed span tracing and Prometheus
counters fed through cardano-node, but lacks streaming, mempool, SearchUtxos,
and a REST interface. cardano-rpc uses the newer v1beta proto spec with
Conway-era governance types defined at the proto level.

## Overview

| Aspect | **cardano-node-api** (Blink Labs) | **cardano-rpc** (this repo) |
|---|---|---|
| Language | Go | Haskell |
| Transport | REST (port 8080) + gRPC (port 9090) | gRPC only (Unix socket) |
| Node connection | Ouroboros NtC via `gouroboros` (socket/TCP) | Local State Query via `cardano-api` (socket) |
| Proto spec | UTxO RPC **v1alpha** | UTxO RPC **v1beta** (newer) |
| Connection model | Fresh connection per request (no pooling) | Fresh connection per request (pooling TODO) |
| Deployment | Standalone sidecar process | Embedded async thread inside cardano-node |

## Services & Endpoints

| Capability | **cardano-node-api** | **cardano-rpc** |
|---|---|---|
| QueryService.ReadParams | Yes | Yes |
| QueryService.ReadUtxos | Yes | Yes |
| QueryService.SearchUtxos | Yes | No |
| QueryService.ReadData | No | No |
| QueryService.ReadTx | No | No |
| QueryService.ReadGenesis | No | No |
| QueryService.ReadEraSummary | No | No |
| SubmitService.SubmitTx | Yes | Yes |
| SubmitService.EvalTx | No | No |
| SubmitService.ReadMempool | Yes | No |
| SubmitService.WaitForTx | Yes (server-stream) | No |
| SubmitService.WatchMempool | Yes (server-stream) | No |
| SyncService.FetchBlock | Partial (incomplete) | No |
| SyncService.DumpHistory | Partial (incomplete) | No |
| SyncService.ReadTip | Yes | No |
| SyncService.FollowTip | Yes (server-stream) | No |
| WatchService.WatchTx | Yes (server-stream) | No |
| REST API | Yes (full suite) | No |
| WebSocket ChainSync | Yes | No |
| Mempool monitoring | Yes (REST + gRPC) | No |
| Prometheus metrics | Yes (standalone, port 8081) | Yes (via cardano-node) |

## Key Differences

### 1. Streaming / ChainSync

**cardano-node-api** implements all four Ouroboros mini-protocols (ChainSync,
LocalStateQuery, LocalTxMonitor, LocalTxSubmission) and exposes streaming via
WebSocket and gRPC server-streams.

**cardano-rpc** only uses LocalStateQuery and LocalTxSubmission — no ChainSync
or TxMonitor support, so no streaming, mempool monitoring, or block following.

### 2. Type Conversions

**cardano-node-api** relies on `gouroboros` types and `go-codegen` for UTxO RPC
protobuf conversion.

**cardano-rpc** has 600+ lines of hand-written type conversions (`Type.hs`)
mapping `cardano-api`/`cardano-ledger` types to proto-lens generated types, with
full Conway-era governance support (proposals, voting, constitutional actions,
certificates).

### 3. Proto Spec Version

**cardano-node-api** uses `utxorpc/go-codegen` v0.18.1 (**v1alpha**).

**cardano-rpc** uses **v1beta** — a newer revision with additional message types
(ReadData, ReadTx, ReadGenesis, ReadEraSummary, EvalTx, CertificatePattern,
etc.).

### 4. Error Handling & Tracing

**cardano-node-api** uses Go's `slog` structured logging.

**cardano-rpc** has span-based distributed tracing (`TraceSpanEvent` with 8-byte
span IDs), typed exceptions with call-stack preservation, and integration with
cardano-node's tracer infrastructure.

## What cardano-rpc is missing (that cardano-node-api has)

1. **SearchUtxos** — predicate-based UTxO search
2. **ChainSync / block streaming** — FollowTip, FetchBlock, WatchTx
3. **Mempool** — ReadMempool, WatchMempool, mempool sizes/tx lookup
4. **Tx confirmation** — WaitForTx
5. **ReadTip** — simple chain tip via gRPC SyncService
6. **REST API** — no HTTP/JSON interface
7. **Metrics** — no standalone Prometheus endpoint (counters via cardano-node)

## What cardano-rpc has that cardano-node-api doesn't

1. **Richer proto definitions** — v1beta with governance types, certificate
   patterns, era summaries, genesis data, EvalTx, predicate composition
   (proto-level only — not all implemented as handlers yet)
2. **Deep ledger integration** — embedded in cardano-node, uses the same
   `cardano-api`/`cardano-ledger` Haskell types as the node itself
3. **Full Conway governance types** — proposals, voting procedures,
   constitutional committee actions in proto definitions

## Architecture

### cardano-node-api

```
REST Clients              gRPC Clients
    |                         |
[port 8080]              [port 9090]
Gin Router              ConnectRPC Mux
    |                         |
    +----+----+----+    +-----+-----+-----+
    |    |    |    |    |     |     |     |
  Chain LSQ TxMon Sub  Query Submit Sync Watch
    |    |    |    |    |     |     |     |
    +----+----+----+----+-----+-----+-----+
                   |
           gouroboros NtC
         (UNIX socket / TCP)
                   |
             cardano-node
```

### cardano-rpc

```
gRPC Clients
     |
 [Unix socket]
   grapesy
     |
  +--+--+------+
  |     |      |
Node  Query  Submit
  |     |      |
  +--+--+------+
     |
 cardano-api
 (LocalStateQuery / LocalTxSubmission)
     |
 cardano-node (same process)
```

## REST API Schema (cardano-node-api)

The REST API contract is defined via a **swaggo/swag annotation pipeline**:

1. Go handler functions carry `// @Summary`, `// @Param`, `// @Success`,
   `// @Router` annotations alongside response struct definitions with `json`
   tags.
2. The `swag` CLI parses these to generate a **Swagger 2.0** spec
   (`docs/swagger.json`, `docs/swagger.yaml`) and a `docs.go` file embedding
   the spec.
3. The Swagger 2.0 spec is converted to **OpenAPI 3.0.1**
   (`openapi/api/openapi.yaml`).
4. `openapi-generator-cli` produces a **standalone Go client SDK** in `openapi/`
   — a separate Go module importable by API consumers.

At runtime, the server serves an interactive **Swagger UI** at `/swagger/*`.

**Response types** are plain Go structs:

```go
type responseLocalStateQueryTip struct {
    Era     string `json:"era"`
    Hash    string `json:"hash"`
    EpochNo int    `json:"epoch_no"`
    BlockNo int64  `json:"block_no"`
    Slot    uint64 `json:"slot_no"`
}
```

**Gaps:** Three response schemas (`EraHistory`, `ProtocolParams`,
`GenesisConfig`) are empty structs — the handlers serialize opaque upstream
library types directly, so clients get `"type": "object"` with no typed
properties. The `Assets` field in UTxO items is `interface{}` (untyped).
Request validation is done manually in handler code, not via Gin's binding
framework.

cardano-rpc has no REST API — its client contract is defined entirely by
Protocol Buffer `.proto` files under `cardano-rpc/proto/`, from which
`proto-lens` generates typed Haskell bindings.

## HTTP & Security Features: cardano-node-api vs Envoy

Since cardano-node-api exposes HTTP and gRPC endpoints, it is important to
understand what security and operational controls it provides out of the box,
and what would need to be added by infrastructure. This section audits the Go
server's built-in security features and compares them against
[Envoy](https://www.envoyproxy.io/) — a high-performance open-source edge and
service proxy widely used for TLS termination, authentication, rate limiting,
and observability in production deployments.

cardano-node-api has virtually no security features of its own. It is designed to
be deployed behind a reverse proxy. The table below compares what it provides
against what Envoy would provide in front of it.

### TLS

| Feature | cardano-node-api | Envoy |
|---|---|---|
| Server-side TLS | Optional (cert + key only) | Full: version pinning, cipher suites, ECDH curves |
| mTLS (client certs) | No | Yes: `require_client_certificate`, SAN matching, SPKI pinning |
| TLS hardening | None (Go defaults) | `MinVersion`, cipher allowlist, `CurvePreferences` |
| Certificate rotation | None | SDS hot-reload, file watch, Vault/SPIFFE integration |
| OCSP stapling | No | Yes: `LENIENT`, `STRICT`, `MUST_STAPLE` policies |
| SNI-based routing | No | Yes: per-SNI `FilterChainMatch` with different certs |
| Private key offload | No | Yes: `cryptomb` (Intel QAT), pluggable providers |

cardano-node-api uses `router.RunTLS()` / `server.ListenAndServeTLS()` with no
`tls.Config` struct — no `MinVersion`, no `CipherSuites`, no `ClientAuth`.

### Authentication

| Feature | cardano-node-api | Envoy |
|---|---|---|
| JWT validation | No | Built-in filter: JWKS fetch, claim forwarding, per-route rules |
| OAuth2 | No | Built-in filter: Authorization Code + PKCE, token refresh |
| External auth | No | `ext_authz`: gRPC/HTTP callout, allow/deny + header injection |
| API keys | No | Via `ext_authz`, Lua filter, or RBAC header match |
| Basic / Bearer auth | No | Via `ext_authz` or Lua |
| RBAC | No | Built-in: match on IP, TLS SAN, headers, path, method |

cardano-node-api has zero authentication. All endpoints (REST + gRPC) are
publicly accessible. gRPC reflection is enabled, exposing the full API schema.

### Rate Limiting

| Feature | cardano-node-api | Envoy |
|---|---|---|
| Local rate limiting | No | Token bucket per listener/route, per-connection scoping |
| Global rate limiting | No | External Rate Limit Service with descriptor-based rules |
| Per-client limiting | No | Descriptors on `remote_address`, headers, JWT claims |
| Rate limit headers | No | `x-ratelimit-limit`, `x-ratelimit-remaining`, `x-ratelimit-reset` |
| Per-route policies | No | Yes: `typed_per_filter_config` |

### DoS Protection

| Feature | cardano-node-api | Envoy |
|---|---|---|
| Connection limits | None | `connection_limit` filter, circuit breaker `max_connections` |
| Request body limits | None (`io.ReadAll` unbounded) | `max_request_bytes` (buffer filter), `per_connection_buffer_limit_bytes` |
| Header size limits | None | `max_request_headers_kb` (default 60 KB) |
| Request timeout | None on REST server | `request_timeout`, `stream_idle_timeout`, route-level timeouts |
| Read header timeout | gRPC only (60s) | `request_headers_timeout` |
| Idle timeout | None | Connection + stream `idle_timeout`, `max_connection_duration` |
| Circuit breaking | None | Per-cluster: `max_connections`, `max_pending_requests`, `max_requests`, `max_retries` |
| Overload management | None | Resource monitors (heap, FDs), graduated degradation actions |
| Slowloris protection | gRPC only (`ReadHeaderTimeout`) | Full: header timeout + read timeout + idle timeout |
| Panic recovery | Gin `Recovery()` middleware | N/A (proxy model — crashes isolated) |

**Critical gap:** the REST server (Gin) is started via `router.Run()` with no
`http.Server` timeouts at all — no `ReadTimeout`, `WriteTimeout`, or
`IdleTimeout`. The tx submission endpoint calls `io.ReadAll(c.Request.Body)`
with no size cap. The whole-UTxO query (`GetUTxOWhole()`) can return an
unbounded data set.

### Input Validation & Path Safety

| Feature | cardano-node-api | Envoy |
|---|---|---|
| Path normalization | No | RFC 3986, `merge_slashes`, escaped slash policy |
| Header validation | Go stdlib basic parsing | UHV: strict RFC compliance, underscore control |
| Parameter validation | Basic (hex check on `policy_id`) | Via `ext_authz` or Lua |
| gRPC message size | ConnectRPC defaults (no explicit limit) | `max_grpc_message_size` per route |

### CORS

| Feature | cardano-node-api | Envoy |
|---|---|---|
| CORS policy | None (WebSocket rejects cross-origin by default) | Declarative: origin matchers, methods, headers, credentials |
| Shadow mode | No | Yes: log violations without enforcing |

### HTTP Security Headers

| Feature | cardano-node-api | Envoy |
|---|---|---|
| HSTS | No | `response_headers_to_add` |
| CSP | No | `response_headers_to_add` |
| X-Frame-Options | No | `response_headers_to_add` |
| X-Content-Type-Options | No | `response_headers_to_add` |
| Strip upstream headers | No | `response_headers_to_remove` (e.g. `Server`, `X-Powered-By`) |

### gRPC Security

| Feature | cardano-node-api | Envoy |
|---|---|---|
| Auth interceptors | None | `ext_authz`, JWT filter, RBAC — all apply to gRPC routes |
| Message size limits | None | `max_grpc_message_size` |
| Reflection control | Always on (schema exposed) | Not exposed by default; controlled by routing |
| gRPC-Web bridging | ConnectRPC handles it | `grpc_web` filter |
| JSON transcoding | No | `grpc_json_transcoder` (REST-to-gRPC from proto descriptors) |
| Per-method metrics | No | `grpc_stats` filter |

### Connection Management

| Feature | cardano-node-api | Envoy |
|---|---|---|
| Connection pooling (upstream) | No (fresh conn per request) | Per-cluster pools, `max_connections`, HTTP/2 stream multiplexing |
| Keepalive | Go defaults | Configurable TCP keepalive probes |
| Max requests/conn | No | `max_requests_per_connection` |
| Graceful shutdown | None (`select{}` blocks forever) | Drain timeout, `GOAWAY`, hot restart |
| Max connection duration | No | `max_connection_duration` |

### Observability (summary)

| Feature | cardano-node-api | Envoy |
|---|---|---|
| Access logging | Basic JSON (method, path, IP, status) | Structured, templated, filterable, multiple sinks |
| Prometheus metrics | Yes (port 8081, basic) | Rich: per-cluster, per-route, per-gRPC-method, histograms |
| Distributed tracing | No | Zipkin, Jaeger, OpenTelemetry, Datadog, X-Ray |
| Health check endpoint | `/healthcheck` (always 200) | Configurable: 200/503 based on upstream health |

See the [Observability Deep Dive](#observability-deep-dive) section below for full
details on every log line, metric name, and trace point.

### Advanced (Envoy only)

| Feature | Envoy |
|---|---|
| Fault injection | Configurable delays and aborts per route |
| Traffic mirroring | Shadow cluster with percentage control |
| External processing | `ext_proc`: gRPC callout for request/response mutation |
| Wasm filters | Custom logic in sandboxed WebAssembly |
| Adaptive concurrency | Auto-tuned concurrency limits from latency |
| Traffic capture (tap) | Full request/response capture with filters |
| Compression | gzip, brotli, zstd with content-type filtering |
| Load balancing | Round robin, least request, ring hash, Maglev, random |
| Outlier detection | Passive ejection on consecutive errors or success rate |
| Zone-aware routing | Locality-weighted, priority-based failover |

### Verdict

cardano-node-api is a **transparent, unauthenticated proxy** to a Cardano node.
It has no rate limiting, no authentication, no connection limits, no request size
limits, and incomplete timeout configuration. It **must** be deployed behind a
reverse proxy (Envoy, nginx, cloud LB) for any production use.

An Envoy sidecar in front of cardano-node-api would add:
- mTLS termination and client cert validation
- JWT/OAuth2 authentication or external auth callout
- Per-client rate limiting with descriptors
- Connection limits, circuit breaking, and overload management
- Request body/header size limits
- Full timeout coverage (read, write, idle, stream)
- Security headers (HSTS, CSP, etc.)
- gRPC reflection control and message size limits
- Distributed tracing and rich Prometheus metrics
- Graceful draining and hot restart

---

## Observability Deep Dive

### cardano-node-api

#### Logging Infrastructure

Two `log/slog` JSON loggers write to stdout:

| Logger | `component` tag | Level | Purpose |
|---|---|---|---|
| Application | `"main"` | Configurable (default: info) | Startup, errors, lifecycle |
| Access | `"access"` | Always info | HTTP request/response |

Every JSON log line includes `"timestamp"` (RFC3339), `"level"`, `"msg"`,
`"component"`.

**Configuration:**
- `LOGGING_LEVEL` — debug / info / warn / error (default: info)
- `LOGGING_HEALTHCHECKS` — when true, suppresses access logs for `/healthcheck`

#### Access Logging

Custom Gin middleware emits two log lines per request (unless the path is suppressed):

**Request received:**
```json
{"timestamp":"...","level":"INFO","msg":"request received","component":"access","method":"GET","path":"/api/localstatequery/tip","remote_addr":"10.0.0.1"}
```

**Response sent:**
```json
{"timestamp":"...","level":"INFO","msg":"response sent","component":"access","method":"GET","path":"/api/localstatequery/tip","remote_addr":"10.0.0.1","status":200}
```

Fields: `method`, `path`, `remote_addr` (client IP via `c.ClientIP()`), `status`
(response only).

**Not logged:** request duration, response size, request ID, user-agent, referer.

#### Application Log Lines (exhaustive)

**Startup (`main.go`):**

| Level | Message | Fields |
|---|---|---|
| Info | `"starting cardano-node-api version X.Y.Z"` | `"version"` |
| Debug | `"skipping node check"` | — |
| Error | `"failed to connect to node:"` | `"error"` |
| Info | `"starting debug listener on host:port"` | — |
| Error | `"failed to start debug listener:"` | `"error"` |
| Error | `"failed to start API:"` | `"error"` |
| Error | `"failed to start gRPC:"` | `"error"` |

**REST API (`api.go`):**

| Level | Message | Fields |
|---|---|---|
| Info | `"starting API listener on host:port"` | — |
| Info | `"starting API TLS listener on host:port"` | — |
| Info | `"disabling access logs for /healthcheck"` | — |
| Info | `"starting metrics listener on host:port"` | — |
| Error | `"failed to start metrics listener:"` | `"error"` (calls `os.Exit(1)`) |

**Tx Submission (`localtxsubmission.go`):**

| Level | Message | Fields |
|---|---|---|
| Error | `"invalid request body, should be application/cbor"` | — |
| Error | `"failed to read request body:"` | `"error"` |
| Error | `"failed to close request body:"` | `"error"` |
| Error | `"failure communicating with node:"` | `"error"` |

**gRPC server (`utxorpc/api.go`):**

| Level | Message | Fields |
|---|---|---|
| Info | `"starting gRPC listener on host:port"` | — |
| Info | `"starting gRPC TLS listener on host:port"` | — |

#### gRPC Handler Logging (stdlib `log.Printf` — unstructured)

The UTxO RPC handlers use Go's stdlib `log.Printf` (NOT `slog`), producing
unstructured text lines to stderr. This is inconsistent with the REST layer.

**QueryService:**

| Handler | Log lines |
|---|---|
| ReadParams | `"Got a ReadParams request with fieldMask %v"` |
| ReadUtxos | `"Got a ReadUtxos request with keys %v"`, datum hash details, `"Prepared response with LedgerTip: Slot=%v, Hash=%v"`, `"Final response: %v"` |
| SearchUtxos | `"Got a SearchUtxos request with predicate %v"`, address part decoding, `"ERROR: %s"` on query failures |

**SubmitService:**

| Handler | Log lines |
|---|---|
| SubmitTx | `"Got a SubmitTx request with 1 transaction"` |
| ReadMempool | `"Got a ReadMempool request"`, `"ERROR: %s"` |
| WatchMempool | `"Got a WatchMempool request with predicate %v and fieldMask %v"`, `"ERROR: %s"` |

**WaitForTx** is the sole exception — uses structured `slog` with `"component":
"WaitForTx"`:

| Level | Message | Fields |
|---|---|---|
| Info | `"Received WaitForTx request"` | `"transaction_count"` |
| Debug | `"Transaction reference"` | `"index"`, `"ref"` (hex) |
| Debug | `"Establishing connection..."` | — |
| Error | `"Failed to connect to node"` | `"error"` |
| Debug | `"Current chain tip retrieved"` | `"tip"` |
| Debug | `"Received TransactionEvent"` | `"hash"` |
| Info | `"Transaction matches reference"` | `"hash"` |
| Warn | `"Client disconnected while sending response"` | `"error"` |
| Error | `"Error sending response to client"` | `"transaction_hash"`, `"error"` |
| Info | `"Confirmation response sent"` | `"transaction_hash"` |

**SyncService:**

| Handler | Log lines |
|---|---|
| FetchBlock | `"Got a FetchBlock request..."`, `"points: %v"` |
| DumpHistory | `"Got a DumpHistory request..."`, start/end token details |
| ReadTip | `"Got a ReadTip request"`, `"ReadTip response: slot: %d, hash: %x, height: %d"` |
| FollowTip | `"Got a FollowTip request with intersect %v"`, block refs, `"ERROR: %s"`, `"block: slot: %d, hash: %s"` |

**WatchService:**

| Handler | Log lines |
|---|---|
| WatchTx | `"Got a WatchTx request..."`, `"ERROR: %s"`, `"event: %v"` |

#### Handlers with NO Logging

These REST handlers return errors to clients but never log them:

- `handleChainSync` (chainsync.go)
- All `handleLocalStateQuery*` functions (localstatequery.go)
- All `handleLocalTxMonitor*` functions (localtxmonitor.go)
- All `internal/node/*.go` connection/protocol functions

#### Prometheus Metrics

Served on a **separate port** (default 8081) at `/` via `gin-metrics`.

The middleware is installed only on the `/api` route group. **Not instrumented:**
`/healthcheck`, `/swagger/*`, and all gRPC endpoints on port 9090.

**Active metrics (REST API only):**

| Metric | Type | Description |
|---|---|---|
| `gin_request_total` | Counter | Total requests |
| `gin_request_uv` | Counter | Unique client IPs |
| `gin_uri_request_total` | Counter | Requests per URI (label: `uri`) |
| `gin_request_body_total` | Counter | Total request body bytes |
| `gin_response_body_total` | Counter | Total response body bytes |
| `gin_request_duration` | Histogram | Duration in seconds (buckets: 0.1, 0.3, 1.2, 5, 10) |
| `gin_slow_request_total` | Counter | Requests exceeding 5s threshold |
| `go_*` | Various | Go runtime metrics (from `prometheus/client_golang`) |
| `process_*` | Various | Process metrics (open FDs, memory, CPU) |

**Dead code (commented out, never registered):**

| Metric | Handler |
|---|---|
| `tx_submit_count` | localtxsubmission.go (success) |
| `tx_submit_fail_count` | localtxsubmission.go (failure, 3 sites) |

**No custom metrics exist.** No per-endpoint latency breakdown beyond `gin_uri_request_total`.
No gRPC metrics at all.

#### Distributed Tracing

**None.** No OpenTelemetry, Jaeger, Zipkin, or X-Ray dependencies. No trace ID
generation or context propagation. No request correlation IDs.

#### Health Checking

| Endpoint | Behavior |
|---|---|
| REST `/healthcheck` | Always returns `{"failed": false}` with 200. No actual health logic. |
| gRPC health | Static checker: always reports `SERVING` for all four services. |

#### Debug / Profiling

`net/http/pprof` registered on a separate server. Disabled by default
(`DEBUG_PORT=0`). When enabled, exposes `/debug/pprof/*` with no authentication.

---

### cardano-rpc

#### Tracing Infrastructure

Uses `contra-tracer` (`Control.Tracer`) — the same tracer infrastructure as
cardano-node itself. The RPC server receives a `Tracer m TraceRpc` from its
caller (cardano-node), which handles the actual log output, formatting, and
routing.

All observability goes through the tracer — cardano-rpc emits typed `TraceRpc`
values, and the cardano-node `trace-dispatcher` pipeline converts them into both
structured JSON log lines and Prometheus counter metrics (via EKG).

#### Trace Type Hierarchy

```
TraceRpc                              -- root
├── TraceRpcQuery   TraceRpcQuery     -- query service traces
├── TraceRpcSubmit  TraceRpcSubmit    -- submit service traces
├── TraceRpcError   SomeException    -- per-request exception
└── TraceRpcFatalError SomeException -- server-level fatal error
```

#### Span Tracing

Each implemented RPC method is wrapped in a begin/end span via `wrapInSpan`.
Span IDs are random 8-byte `Word64` values rendered in hex
(`UsingRawBytesHex`).

The span pattern:
1. Generate random `spanId`
2. Emit `SpanBegin spanId`
3. Execute handler, force evaluation to NF
4. Emit `SpanEnd spanId` in `finally` (guaranteed even on exception)

#### All Trace Points (exhaustive)

**Query Service — `TraceRpcQuery`:**

| Constructor | Event | Log message |
|---|---|---|
| `TraceRpcQueryParamsSpan (SpanBegin id)` | ReadParams start | `"Started query params method"` |
| `TraceRpcQueryParamsSpan (SpanEnd id)` | ReadParams end | `"Finished query params method"` |
| `TraceRpcQueryReadUtxosSpan (SpanBegin id)` | ReadUtxos start | `"Started query read UTXO method"` |
| `TraceRpcQueryReadUtxosSpan (SpanEnd id)` | ReadUtxos end | `"Finished query read UTXO method"` |

**Submit Service — `TraceRpcSubmit`:**

| Constructor | Event | Log message |
|---|---|---|
| `TraceRpcSubmitSpan (SpanBegin id)` | SubmitTx start | `"Started submit method"` |
| `TraceRpcSubmitSpan (SpanEnd id)` | SubmitTx end | `"Finished submit method"` |
| `TraceRpcSubmitTxDecodingError e` | CBOR decode failure | `"Failed to decode transaction: " <> pshow e` |
| `TraceRpcSubmitN2cConnectionError e` | Node connection failure | `"N2C connection error while trying to submit a transaction: " <> prettyException e` |
| `TraceRpcSubmitTxValidationError e` | Tx validation failure | `"Failed to submit transaction: " <> pshow e` |

**Top-level — `TraceRpc`:**

| Constructor | Event | Log message |
|---|---|---|
| `TraceRpcError e` | Any handler exception | `"Exception when processing RPC request:\n" <> prettyException e` |
| `TraceRpcFatalError e` | Server crash | `"RPC server fatal error: " <> prettyException e` |

**Unwrapped methods (no span tracing):** the custom Node service methods —
exceptions still caught by `TraceRpcError`.

**Total:** 11 trace constructors (6 span events + 3 submit errors + 2 top-level).

#### Error Tracing Flow (Submit)

```
submitTxMethod
  ├── deserialiseTx fails → TraceRpcSubmitTxDecodingError → trace → throw
  ├── node connection fails → TraceRpcSubmitN2cConnectionError → trace → throw
  ├── tx validation fails → TraceRpcSubmitTxValidationError → trace → throw
  └── any of above → caught by topLevelHandler → TraceRpcError → trace
```

The `putTraceThrowEither` helper traces the error THEN throws, so the submit
error and the top-level error are both emitted for the same failure.

#### Prometheus Metrics (via cardano-node)

cardano-rpc does not run its own Prometheus endpoint. Instead, `SpanBegin`
events are converted to `CounterM` metrics by the `LogFormatting` instance in
`Cardano.Node.Tracing.Tracers.Rpc`. The cardano-node `trace-dispatcher`
pipeline feeds these into EKG, which exposes them on cardano-node's Prometheus
endpoint.

**Metric pipeline:**
1. RPC handler emits `TraceRpcQuery (TraceRpcQueryParamsSpan (SpanBegin id))`
2. `asMetrics` in `LogFormatting TraceRpc` converts it to
   `CounterM "rpc.request.QueryService.ReadParams" Nothing`
3. `trace-dispatcher` EKG tracer increments the counter by 1
4. Prometheus exposition normalises dots to underscores

**Active counters:**

| Prometheus name | Trace source | Description |
|---|---|---|
| `rpc_request_QueryService_ReadParams` | `TraceRpcQueryParamsSpan (SpanBegin _)` | ReadParams requests |
| `rpc_request_QueryService_ReadUtxos` | `TraceRpcQueryReadUtxosSpan (SpanBegin _)` | ReadUtxos requests |
| `rpc_request_SubmitService_SubmitTx` | `TraceRpcSubmitSpan (SpanBegin _)` | SubmitTx requests |

**Not counted:** Error traces (`TraceRpcError`, submit errors) produce no
metrics — they are logged only. SearchUtxos is not yet implemented so has no
counter.

#### Span Timers (in logs)

Each `SpanBegin` and `SpanEnd` event receives its own timestamp when formatted
by `trace-dispatcher` (via `getCurrentTime` in `preFormatted`). Because both
events share the same `spanId`, the logs carry timer data: the duration of an
RPC call is `SpanEnd.at - SpanBegin.at` for a matching `spanId`.

This means:
- **Logs have timers** — begin/end timestamps per span ID enable duration
  computation by cardano-tracer or any log consumer
- **Prometheus has counters** — request counts only, no latency histograms

#### Machine-Readable Log Format (JSON)

The `trace-dispatcher` `preFormatted` step captures a timestamp, thread ID, and
namespace for each event. Combined with `forMachine` from `LogFormatting
TraceRpc`, the JSON output looks like:

**SpanBegin:**
```json
{"at":"2026-03-18T12:00:00.000Z","ns":"RPC.QueryService.ReadParams.Span","sev":"Debug","thread":"42","host":"node1","data":{"kind":"QueryService","queryName":"ReadParams","span":"begin","spanId":"a1b2c3d4e5f6a7b8","reason":"Started query params method"}}
```

**SpanEnd (same spanId, later timestamp):**
```json
{"at":"2026-03-18T12:00:00.047Z","ns":"RPC.QueryService.ReadParams.Span","sev":"Debug","thread":"42","host":"node1","data":{"kind":"QueryService","queryName":"ReadParams","span":"end","spanId":"a1b2c3d4e5f6a7b8","reason":"Finished query params method"}}
```

**Submit error:**
```json
{"at":"...","ns":"RPC.SubmitService.TxValidationError","sev":"Debug","thread":"42","host":"node1","data":{"kind":"SubmitService","reason":"Failed to submit transaction: ..."}}
```

**Top-level fields** (from `trace-dispatcher`): `at` (UTC timestamp), `ns`
(namespace), `sev` (severity), `thread`, `host`.

**Data fields** (from `forMachine`): `kind` (service or error category),
`reason` (human-readable via `Pretty`), and for spans: `queryName` (query
methods only), `span` (`"begin"`/`"end"`), `spanId` (hex).

#### MetaTrace Metadata

Each trace has severity, privacy, and documentation annotations via the
`MetaTrace TraceRpc` instance:

| Namespace | Severity | Documentation |
|---|---|---|
| `RPC.FatalError` | Error | "RPC startup critical error." |
| `RPC.Error` | Debug | "Normal operation errors such as request errors." |
| `RPC.QueryService.ReadParams.Span` | Debug | "Span for the ReadParams UTXORPC method." |
| `RPC.QueryService.ReadUtxos.Span` | Debug | "Span for the ReadUtxos UTXORPC method." |
| `RPC.SubmitService.SubmitTx.Span` | Debug | "Span for the SubmitTx UTXORPC method." |
| `RPC.SubmitService.N2cConnectionError` | Warning | "Node connection error." |
| `RPC.SubmitService.TxDecodingError` | Debug | "Submitted transaction decoding fails." |
| `RPC.SubmitService.TxValidationError` | Debug | "Submitted transaction is invalid." |

All traces are `Public` privacy. Filtering is controlled by cardano-node's
tracer configuration (e.g. `TraceOptionSeverity`).

#### Tracer Wiring

In `Cardano.Node.Tracing.Tracers`:
```haskell
rpcTr <- mkCardanoTracer trBase trForward mbTrEKG ["RPC"]
```

In `Cardano.Node.Run`:
```haskell
withAsync (runRpcServer (rpcTracer tracers) (ncRpcConfig nc, networkMagic)) ...
```

The `mkCardanoTracer` call sets up the full pipeline: `forMachine` JSON
formatting, `asMetrics` counter extraction, EKG registration, log forwarding
(to cardano-tracer if configured), and severity filtering.

#### What cardano-rpc Does NOT Have

- **No access logging** — no request method/path/IP/status logging
- **No standalone Prometheus** — counters only available through cardano-node
- **No latency histograms in Prometheus** — durations are in logs (span
  begin/end timestamps), not exported as histogram metrics
- **No request/response size tracking**
- **No per-client tracking** — no client IP or connection info in traces
- **No error rate counters** — errors are traced but not counted as metrics
- **No SearchUtxos** — proto defined but handler not implemented yet

---

### Observability Comparison

| Capability | cardano-node-api | cardano-rpc |
|---|---|---|
| **Logging framework** | `slog` JSON + stdlib `log.Printf` (inconsistent) | `contra-tracer` (delegated to cardano-node) |
| **Access logs** | Yes: method, path, IP, status | No |
| **Structured logging** | Partial (REST yes, gRPC mostly no) | Yes (all traces are typed ADTs) |
| **Span tracing / timers** | No | Yes: begin/end pairs with timestamps per span ID (timers in logs) |
| **Error tracing** | Inconsistent (some logged, some silent) | Consistent: all errors traced via typed constructors |
| **Prometheus metrics** | 7 auto metrics (REST only), no gRPC | 3 counters via cardano-node EKG/Prometheus |
| **Custom metrics** | None active (tx_submit_count commented out) | Request counters in Prometheus, timers in logs |
| **gRPC metrics** | None | 3 counters (ReadParams, ReadUtxos, SubmitTx) |
| **Runtime metrics** | `go_*`, `process_*` via Prometheus | Via cardano-node EKG (when configured) |
| **Distributed tracing** | None | Span IDs with timestamps; no OpenTelemetry/Jaeger integration |
| **Health checks** | REST: static 200; gRPC: static SERVING | None exposed (embedded in node) |
| **Profiling** | pprof endpoint (disabled by default) | None |
| **Request correlation** | No request IDs | Span IDs per method call |
| **Log output** | stdout (JSON) + stderr (unstructured) | Routed through cardano-node tracer |
| **Log filtering** | Level-based + healthcheck suppression | Controlled by cardano-node tracer config |

#### Gaps in Both

Neither implementation has:
- Per-endpoint latency histograms in Prometheus (cardano-rpc has per-method
  timers in logs via span begin/end timestamps; cardano-node-api has overall
  `gin_request_duration` but no per-route gRPC breakdown)
- Error rate counters in Prometheus
- Request/response payload size per endpoint
- Client identity tracking
- Alerting thresholds or SLO definitions
- Log correlation between REST and gRPC requests
- Audit trail for tx submissions (who submitted what, when)

---

## References

### cardano-node-api (Blink Labs)

| What | Location |
|---|---|
| Repository | https://github.com/blinklabs-io/cardano-node-api |
| REST router, access logging, metrics setup | [`internal/api/api.go`](https://github.com/blinklabs-io/cardano-node-api/blob/main/internal/api/api.go) |
| Tx submission handler (error logging, commented-out metrics) | [`internal/api/localtxsubmission.go`](https://github.com/blinklabs-io/cardano-node-api/blob/main/internal/api/localtxsubmission.go) |
| WebSocket chain-sync handler | [`internal/api/chainsync.go`](https://github.com/blinklabs-io/cardano-node-api/blob/main/internal/api/chainsync.go) |
| Local state query handlers | [`internal/api/localstatequery.go`](https://github.com/blinklabs-io/cardano-node-api/blob/main/internal/api/localstatequery.go) |
| Local tx monitor handlers | [`internal/api/localtxmonitor.go`](https://github.com/blinklabs-io/cardano-node-api/blob/main/internal/api/localtxmonitor.go) |
| gRPC server setup (TLS, ConnectRPC, health, reflection) | [`internal/utxorpc/api.go`](https://github.com/blinklabs-io/cardano-node-api/blob/main/internal/utxorpc/api.go) |
| gRPC QueryService (ReadParams, ReadUtxos, SearchUtxos) | [`internal/utxorpc/query.go`](https://github.com/blinklabs-io/cardano-node-api/blob/main/internal/utxorpc/query.go) |
| gRPC SubmitService (SubmitTx, WaitForTx, ReadMempool, WatchMempool) | [`internal/utxorpc/submit.go`](https://github.com/blinklabs-io/cardano-node-api/blob/main/internal/utxorpc/submit.go) |
| gRPC SyncService (FetchBlock, DumpHistory, ReadTip, FollowTip) | [`internal/utxorpc/sync.go`](https://github.com/blinklabs-io/cardano-node-api/blob/main/internal/utxorpc/sync.go) |
| gRPC WatchService (WatchTx) | [`internal/utxorpc/watch.go`](https://github.com/blinklabs-io/cardano-node-api/blob/main/internal/utxorpc/watch.go) |
| Logging infrastructure (slog setup) | [`internal/logging/logging.go`](https://github.com/blinklabs-io/cardano-node-api/blob/main/internal/logging/logging.go) |
| Configuration (TLS, logging, metrics, node, debug) | [`internal/config/config.go`](https://github.com/blinklabs-io/cardano-node-api/blob/main/internal/config/config.go) |
| Node connection (Ouroboros NtC) | [`internal/node/node.go`](https://github.com/blinklabs-io/cardano-node-api/blob/main/internal/node/node.go) |
| Entry point (startup, pprof, signal handling) | [`cmd/cardano-node-api/main.go`](https://github.com/blinklabs-io/cardano-node-api/blob/main/cmd/cardano-node-api/main.go) |
| gin-metrics library (Prometheus auto-metrics) | https://github.com/penglongli/gin-metrics |
| Generated Swagger 2.0 spec | [`docs/swagger.json`](https://github.com/blinklabs-io/cardano-node-api/blob/main/docs/swagger.json) |
| OpenAPI 3.0.1 spec | [`openapi/api/openapi.yaml`](https://github.com/blinklabs-io/cardano-node-api/blob/main/openapi/api/openapi.yaml) |
| Generated Go client SDK | [`openapi/`](https://github.com/blinklabs-io/cardano-node-api/tree/main/openapi) |

### cardano-rpc

| What | Location |
|---|---|
| Trace type definitions (`TraceRpc`, `TraceRpcQuery`, `TraceRpcSubmit`, `TraceSpanEvent`) | `cardano-rpc/src/Cardano/Rpc/Server/Internal/Tracing.hs` |
| Span wrapping, `putTrace`, `newSpanId` helpers | `cardano-rpc/src/Cardano/Rpc/Server/Internal/Monad.hs` |
| gRPC server setup, method wiring, top-level exception handler | `cardano-rpc/src/Cardano/Rpc/Server.hs` |
| Submit handler with error tracing (`putTraceThrowEither`) | `cardano-rpc/src/Cardano/Rpc/Server/Internal/UtxoRpc/Submit.hs` |
| Query handlers (ReadParams, ReadUtxos) | `cardano-rpc/src/Cardano/Rpc/Server/Internal/UtxoRpc/Query.hs` |
| Node service handlers | `cardano-rpc/src/Cardano/Rpc/Server/Internal/Node.hs` |
| Type conversions (Cardano types ↔ protobuf) | `cardano-rpc/src/Cardano/Rpc/Server/Internal/UtxoRpc/Type.hs` |
| RPC environment and node connection info | `cardano-rpc/src/Cardano/Rpc/Server/Internal/Env.hs` |
| Configuration (`RpcConfig`) | `cardano-rpc/src/Cardano/Rpc/Server/Config.hs` |
| Proto definitions (Cardano types) | `cardano-rpc/proto/utxorpc/v1beta/cardano/cardano.proto` |
| Proto definitions (Query service) | `cardano-rpc/proto/utxorpc/v1beta/query/query.proto` |
| Proto definitions (Submit service) | `cardano-rpc/proto/utxorpc/v1beta/submit/submit.proto` |
| Proto definitions (custom Node service) | `cardano-rpc/proto/cardano/rpc/node.proto` |

### cardano-node (trace consumers)

| What | Location |
|---|---|
| `LogFormatting TraceRpc` — JSON format + `asMetrics` counters | `cardano-node/src/Cardano/Node/Tracing/Tracers/Rpc.hs` |
| `MetaTrace TraceRpc` — namespaces, severities, metric docs | `cardano-node/src/Cardano/Node/Tracing/Tracers/Rpc.hs` |
| Tracer wiring (`mkCardanoTracer`, `rpcTr`) | `cardano-node/src/Cardano/Node/Tracing/Tracers.hs` |
| `Tracers` record (`rpcTracer` field) | `cardano-node/src/Cardano/Node/Tracing.hs` |
| RPC server launch (`runRpcServer` call) | `cardano-node/src/Cardano/Node/Run.hs` |
| Trace documentation generation | `cardano-node/src/Cardano/Node/Tracing/Documentation.hs` |
| EKG metric registration and Prometheus exposition | `trace-dispatcher/src/Cardano/Logging/Tracer/EKG.hs` |
| Prometheus text format exposition | `trace-dispatcher/src/Cardano/Logging/Prometheus/Exposition.hs` |
| `Metric` type (`CounterM`, `IntM`, etc.) | `trace-dispatcher/src/Cardano/Logging/Types.hs` |
| Metrics formatter (extracts `asMetrics` from traces) | `trace-dispatcher/src/Cardano/Logging/Formatter.hs` |

### Envoy Proxy

| What | Location |
|---|---|
| Documentation home | https://www.envoyproxy.io/docs/envoy/latest/ |
| TLS / mTLS configuration | https://www.envoyproxy.io/docs/envoy/latest/intro/arch_overview/security/ssl |
| JWT authentication filter | https://www.envoyproxy.io/docs/envoy/latest/configuration/http/http_filters/jwt_authn_filter |
| External authorization filter | https://www.envoyproxy.io/docs/envoy/latest/configuration/http/http_filters/ext_authz_filter |
| Local rate limiting filter | https://www.envoyproxy.io/docs/envoy/latest/configuration/http/http_filters/local_rate_limit_filter |
| Global rate limiting filter | https://www.envoyproxy.io/docs/envoy/latest/configuration/http/http_filters/rate_limit_filter |
| Circuit breaking | https://www.envoyproxy.io/docs/envoy/latest/intro/arch_overview/upstream/circuit_breaking |
| Overload manager | https://www.envoyproxy.io/docs/envoy/latest/configuration/operations/overload_manager/overload_manager |
| CORS filter | https://www.envoyproxy.io/docs/envoy/latest/configuration/http/http_filters/cors_filter |
| gRPC-Web filter | https://www.envoyproxy.io/docs/envoy/latest/configuration/http/http_filters/grpc_web_filter |
| gRPC-JSON transcoder | https://www.envoyproxy.io/docs/envoy/latest/configuration/http/http_filters/grpc_json_transcoder_filter |
| RBAC filter | https://www.envoyproxy.io/docs/envoy/latest/configuration/http/http_filters/rbac_filter |
| Fault injection | https://www.envoyproxy.io/docs/envoy/latest/configuration/http/http_filters/fault_filter |

### UTxO RPC Specification

| What | Location |
|---|---|
| UTxO RPC project | https://utxorpc.org/ |
| Proto definitions (upstream) | https://github.com/utxorpc/spec |
| Go codegen (used by cardano-node-api) | https://github.com/utxorpc/go-codegen |
