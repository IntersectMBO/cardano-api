# CIP-160 V4 fixtures

Generated with the ledger Plutus preprocessor against Plutus 1.71 proposal commit `dcbb7e3232c3322557410fe341ec84f3cd78dc04`. Two generation passes produced identical output, and all 41 legacy fixtures retained their bytes.

`receiving-even-datum` checks the even inline integer datum of its own resolved protected output. `receiving-redeemer-matches-datum` checks that the supplied integer redeemer equals that output's inline integer datum, that the raw output index resolves the same output, and that its protected payment hash matches the Receiving purpose. Same-hash siblings have separate contexts. `inputs-outputs-not-empty-no-datum` requires nonempty body inputs and outputs. `always-succeeds-no-datum` accepts any no-datum purpose context.

The tests embed the exact CBOR bytes to avoid filesystem-dependent execution. Each `.plutus` file is a standard `PlutusScriptV4` text envelope containing its adjacent `.cbor` file.

| Fixture | Script hash | CBOR SHA256 |
| --- | --- | --- |
| receiving-even-datum | `28d467557081773a051b5f83982574abfccceb079bc8012f192505e2` | `dbce2a96a7a81771a27ea29635b0c05d4f7c45fee8714e63a2ab83b1cd436fbc` |
| receiving-redeemer-matches-datum | `4698ecb2f771339e0675ffdad1a473c53d2ff3dd1fe6298db8d3851c` | `d07a28ee0e99c504ffbf9b7a6670b1f1cb46ef2553864599f20d28a8da843a86` |
| inputs-outputs-not-empty-no-datum | `758c9b987e7c15ff6e0b76cb958664b2ae567748c5b77a3774d3b105` | `b0b256b72817a72dea06a2c641a834e8dfeafef5acf57a3100640c4ddd53f4c9` |
| always-succeeds-no-datum | `df3c23786bd8473dcf099b4b34b9d04944701b65d369a53052a2819c` | `ac52002385bfe622ec844b12a99ec680d22df3b2f733601355e5489ad5c59ea1` |
