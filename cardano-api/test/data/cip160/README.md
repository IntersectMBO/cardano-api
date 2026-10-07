# CIP-160 V4 fixtures

Generated with the ledger Plutus preprocessor against Plutus 1.71 proposal commit 3ddfba3e01998eb98e2c906c1caecebee242b609. `receiving-even-datum` requires nonempty protected outputs for its own Receiving hash and an even inline integer datum on every such output. Ordinary same-hash outputs are ignored. `inputs-outputs-not-empty-no-datum` requires nonempty body inputs and outputs, and works for Receiving outputs without datums. `always-succeeds-no-datum` accepts any no-datum purpose context.

The tests embed these exact CBOR bytes to avoid filesystem-dependent test execution. Script hashes, in order: 86016b1378d4090890d47e9099c2cd4072ac43e689a12f5a7de32496, 758c9b987e7c15ff6e0b76cb958664b2ae567748c5b77a3774d3b105, df3c23786bd8473dcf099b4b34b9d04944701b65d369a53052a2819c.
