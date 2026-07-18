# cardano-wasm API regression suite

Drives the real built wasm library (`../../../lib-wrapper/`) in Node and checks the
API surface the demo depends on: wallet generate/restore round-trip, `getTxId`
stability across witnesses, certificate round-trips (register / delegate /
unregister), `inspectAddress`, and a probe that notices when the `newConwayTx`
export gets fixed upstream.

Build the wasm library first (see `../../../README.md`), then:

```bash
npm install       # fetches the WASI shim used to run the wasm under Node
node api-regression.mjs
```

Prints one PASS/FAIL line per group; exits non-zero on failure.
