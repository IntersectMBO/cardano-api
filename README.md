## Overview of the `cardano-api` repository

Integration of the [`ledger`](https://github.com/IntersectMBO/cardano-ledger), [`consensus`](https://github.com/IntersectMBO/ouroboros-consensus) and
[`networking`](https://github.com/IntersectMBO/ouroboros-network/tree/master/ouroboros-network) repositories.

## Contributing

See the [Contributing guide](CONTRIBUTING.md) for how to contribute to this project.

## Development Setup

### Using Claude Code

For developers using [Claude Code](https://claude.ai/code), project-specific configuration can be set up by:

1. Clone the omni-dev-config repository:
   ```bash
   git clone https://github.com/input-output-hk/omni-dev-config
   ```

2. Symlink `CLAUDE.md` from omni-dev-config to this repository:
   ```bash
   ln -s /path/to/omni-dev-config/CLAUDE.md /path/to/cardano-api/CLAUDE.md
   ```

This provides Claude Code with project-specific context, coding standards, and best practices.

## Core maintainers

* [Jordan Millar](https://github.com/Jimbo4350)
* [John Ky](https://github.com/newhoggy)
* [Mateusz Gałażyn](https://github.com/carbolymer)
* [Pablo Lamela](https://github.com/palas)

## Documentation

Package-specific documentation:
- [cardano-api](cardano-api/README.md) - Main library for constructing and submitting transactions
- [cardano-api-gen](cardano-api-gen/README.md) - Test generators and property testing utilities
- [cardano-rpc](cardano-rpc/README.md) - gRPC client and server implementing UTxO RPC protocol
- [cardano-wasm](cardano-wasm/README.md) - WebAssembly compilation support

Development documentation can be found in [Cardano Node Wiki](https://github.com/input-output-hk/cardano-node-wiki/wiki).

Haddock documentation is available at: https://cardano-api.cardano.intersectmbo.org/

[![x86\_64-linux](https://img.shields.io/endpoint?url=https://ci.iog.io/job/IntersectMBO-cardano-api/master/x86_64-linux.required/shield&style=flat-square&label=x86_64-linux)](https://ci.iog.io/job/IntersectMBO-cardano-api/master/x86_64-linux.required)
[![x86\_64-darwin](https://img.shields.io/endpoint?url=https://ci.iog.io/job/IntersectMBO-cardano-api/master/x86_64-darwin.required/shield&style=flat-square&label=x86_64-darwin)](https://ci.iog.io/job/IntersectMBO-cardano-api/master/x86_64-darwin.required)
[![GHA Build](https://img.shields.io/github/actions/workflow/status/intersectmbo/cardano-api/haskell.yml?branch=master&label=GHA%20Build&style=flat-square)](https://github.com/IntersectMBO/cardano-api/actions/workflows/haskell.yml?query=branch%3Amaster)
[![Haddock](https://img.shields.io/github/actions/workflow/status/intersectmbo/cardano-api/github-page.yml?branch=master&label=Haddocks&style=flat-square)](https://github.com/IntersectMBO/cardano-api/actions/workflows/github-page.yml?query=branch%3Amaster)

