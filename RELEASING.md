# Versioning and release process

## Release process

When making a new release, you have to decide on a new version number for the release.
In order to choose which version number bump, follow the [`cardano-ledger` versioning guidelines](https://github.com/input-output-hk/cardano-ledger/blob/master/RELEASING.md#versioning-process).
After deciding on the version number, set the correct `version` field in cabal files in this repo.

### Changelog preparation
The changelog preparation workflow is using `cardano-updates` to gather all information and produce changelog in markdown format.
The full documentation for scripts is located in [`cardano-updates` repository](https://github.com/input-output-hk/cardano-updates/blob/main/scripts/README.md).

This part requires user to have the following tools installed on your local machine:
* https://github.com/cli/cli
* https://jqlang.github.io/jq/
* https://mikefarah.gitbook.io/yq/

In order to generate changelog files in markdown format use the following steps:

1. Clone the `cardano-updates` repo (necessary only when doing it for the first time):
    ```bash
    git clone https://github.com/input-output-hk/cardano-updates
    cd cardano-updates
    ```

1. Download all PRs from the `cardano-api` repo.
    This will take some time if the number of all PRs is large.
    ```bash
    ./scripts/download-prs.sh input-output-hk/cardano-api
    ```
    This script extracts changelog entries from PR descriptions.
    It would be advisable to make changelog lines corrections in the descriptions of GitHub PRs itself, as this would let us use GitHub PRs as a single source of truth for the changelog generation process.
    This also means, that after making change to PR description, the process of generating changelogs needs to be restarted from this download step.

1. Process just downloaded PRs data by splitting it into separate data files related to separate repository components.
    Provide the date range you're interested in - only PRs from that period will be processed.
    ```bash
    ./scripts/distribute-merged-prs.sh input-output-hk/cardano-api . 2023-01-01 2023-06-01
    ```
    For each repository component (cabal packages, nix configuration) a file with PRs details (in yaml format) will be generated in `gen/input-output-hk/cardano-api/detail` directory.
    Those data files can be modified before next step to alter the information being put into final changelogs in markdown format.
    For example, to exclude a PR from the changelog, set its `included` field value to `false`.

1. Generate markdown changelogs from yaml details files:
    ```bash
    ./scripts/summarise-merged-prs.sh input-output-hk/cardano-api .
    ```
    The output files will be placed in the `gen/input-output-hk/cardano-api/summary` subdirectory.

1. Add generated changelogs to `CHANGELOG.md` files in respective cabal packages in `cardano-api` repository, and create a PR from a new branch back to main.
    Make sure that the release PR contains:
    * updated changelogs
    * bumped version fields in cabal files

### Tagging the commit
**After merging of the release PR** into the `main` branch, prepare the tag.
Firstly, make sure that:
1. Your `HEAD` is on the commit which you are planning to make a release from.
1. Your `HEAD` is included in `main` history.

Then you can use the following script to prepare the tag:
```bash
./scripts/tag.sh
```
This script will extract the version numbers from cabal files, create the tag and **push it to the `origin` remote**.

### Releasing to `cardano-haskell-packages`
After the `cardano-api` version gets tagged, it needs to be pushed into `cardano-haskell-packages` (aka **CHaP**).
Detailed description of the release process is in [CHaP repository README](https://github.com/input-output-hk/cardano-haskell-packages#how-to-add-a-new-package-version).
Briefly, the process requires executing of the following steps:

1. Clone `cardano-haskell-packages`:
    ```bash
    git clone https://github.com/input-output-hk/cardano-haskell-packages
    cd cardano-haskell-packages
    ```

1.  Run the following script, replacing `<commit-hash>` with the just tagged commit hash:
    ```bash
    ./scripts/add-from-github.sh https://github.com/input-output-hk/cardano-api <commit-hash> cardano-api cardano-api-gen
    ```
    List all packages names for release in the script invocation, after the commit hash.
    The script will create a separate commit for each package.

1. Push your `HEAD` to a new branch, and create a PR in `CHaP`.

1. Merge the PR - you don't need additional approvals for that if you belong to the correct GitHub access group.

After package gets released, you can check the released version at: https://input-output-hk.github.io/cardano-haskell-packages/all-package-versions/#


## References
1. https://github.com/input-output-hk/cardano-updates/tree/main/scripts
1. https://github.com/input-output-hk/cardano-ledger/blob/master/RELEASING.md#
1. https://input-output-hk.github.io/cardano-haskell-packages/index.html
1. https://input-output-hk.github.io/cardano-engineering-handbook/policy/haskell/packaging/versioning.html
