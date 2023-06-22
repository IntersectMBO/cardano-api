# Versioning and release process

## Release process

When making a new release, firstly you have to decide on a new version number for the release.

### Version bumping
`cardano-api` is using [Haskell Package Versioning Policy](https://pvp.haskell.org/) for numbering each release version.

In order to decide which version number needs to be bumped up, it is necessary to know what was the latest released version of a package.
Three simple ways are:
* look at the latest version on [`cardano-haskell-packages` (aka **CHaP**)](https://input-output-hk.github.io/cardano-haskell-packages/index.html) - the most reliable way
* current version in the changelog
* look at the latest git tag for the version

When you found out the current version of `cardano-api`, the next step is to find out if the changes within the scope of the release are breaking or not.
To make this process easier, each pull request has information about that - [see the `compatibility` field in the example changelog here](https://github.com/input-output-hk/cardano-api/pull/53).
This information becomes available in the next step of the process, in the changelog preparation after executing `generate-pr-changelogs.sh` script.
You can defer decision about the version bump to that point.

In general, the [PVP decision tree](https://pvp.haskell.org/#decision-tree) may become useful in the process.
For example, if the current version of cardano-api is `8.4.1.2`, you need to bump version to:
* `8.4.1.3` - if there are only backwards-compatible bug-fixes
* `8.4.2.0` - if there are only backwards-compatible features or bug-fixes
* `8.5.0.0` - if there are any breaking changes
* `9.0.0.0` - for major Cardano releases

After deciding on the version number, set the correct `version` field in all cabal files in this repo.

### Changelog preparation
The changelog preparation workflow is using `cardano-updates` to gather all information and produce the changelog in markdown format.
The full documentation for scripts is located in [`cardano-updates` repository](https://github.com/input-output-hk/cardano-updates/blob/main/scripts/README.md).

This part requires user to have the following tools installed on your local machine:
* https://github.com/cli/cli
* https://jqlang.github.io/jq/
* https://mikefarah.gitbook.io/yq/

In order to generate changelog files in markdown format use the following steps:

1. *When doing this guide for the first time:* Clone the `cardano-dev` repo at the same level as `cardano-api`:
    ```bash
    git clone https://github.com/input-output-hk/cardano-dev
    ```
    Check that you're authenticated to GitHub using GitHub CLI:
    ```bash
    gh auth status
    ```
    If you're not authenticated, follow the steps shown on the command output.

1. Create a new branch, for example:
    ```bash
    git checkout -b <github-user-name>/new-version-cardano-api-8.3.0.0
    ```
    It does not matter much, as after the merge to `main` branch, this branch will be deleted.

1. Download all PRs data from the `cardano-api` repo.
    This will take some time if the number of all PRs is large.
    ```bash
    ../cardano-dev/scripts/download-prs.sh input-output-hk/cardano-api
    ```
    It would be advisable to make changelog entries corrections in the descriptions of GitHub PRs itself, as this would let us use GitHub PRs as a single source of truth for the changelog generation process.
    This also means, that after making a change to a changelog in a PR description, the whole procedure needs to be restarted from this download step.
    The output changelog can be reviewed in the next step.

    The downloaded PRs can be inspected in `~/.cache/cardano-updates/` directory.

1. Find out the hash of the previous tag.
    You can list all the tags and their hashes using the following command:
    ```bash
    git show-ref --tags --dereference | sort -V -t '/' -k 3,3
    ```

1. Generate markdown changelogs from yaml detail file providing the hash of the previous release tag in the command line argument.
    For example for the changelog between the tag `cardano-api-8.2.0.0` and `HEAD`:
    ```bash
    ../cardano-dev/scripts/generate-pr-changelogs.sh input-output-hk/cardano-api 89fd11781d8ba19ce50f516ecef30607d2e704e8..HEAD
    ```
    This will process downloaded PRs and use those marked with `feature` or `bug` to produce the changelog to the standard output.

1. Add generated changelog in the previous step to `CHANGELOG.md` file in respective cabal package in `cardano-api` repository, near the top of the file, adding a new section for the version being prepared, for example: `## 8.3.0.0`.
    After doing that, create a PR from a new branch back to main.
    Make sure that the release PR contains:
    * updated changelogs
    * bumped version fields in cabal files

### Tagging the commit
**After merging of the release PR** into the `main` or release branch, prepare the tag.
Firstly, make sure that:
1. Your `HEAD` is on the commit which you are planning to make a release from.
1. Your `HEAD` is included in `main` or in `release/packagename-version.x` branch history on the origin remote.

Then you can use the following script to prepare the tag:
```bash
../cardano-dev/scripts/tag.sh
```
This script will extract the version numbers from cabal files, create the tag and **push it to the `origin` remote**.
Please note that the tagging process will fail if:
1. The tag already exists on the origin remote
1. The `packagename/CHANGELOG.md` does not contain entry for the new version.

### Releasing to `cardano-haskell-packages`
After the `cardano-api` version gets tagged, it needs to be pushed into `cardano-haskell-packages` (aka **CHaP**).
Detailed description of the release process is described in [CHaP repository README](https://github.com/input-output-hk/cardano-haskell-packages#how-to-add-a-new-package-version).
Briefly speaking, it requires executing of the following steps:

1. Clone `cardano-haskell-packages`:
    ```bash
    git clone git@github.com:input-output-hk/cardano-haskell-packages.git
    cd cardano-haskell-packages
    ```

1.  Run the following script, replacing `<commit-hash>` with the just tagged commit hash:
    ```bash
    ./scripts/add-from-github.sh https://github.com/input-output-hk/cardano-api <commit-hash> cardano-api cardano-api-gen
    ```
    List all packages names for release in the script invocation, after the commit hash like in the example above.
    The script will create a separate commit for each package.

1. Push your `HEAD` to a new branch, and create a PR in CHaP.
    An example release PR which you might want to use as a reference: https://github.com/input-output-hk/cardano-haskell-packages/pull/345 .

1. Merge the PR - you don't need additional approvals for that if you belong to the correct GitHub access group.

After package gets released, you can check the released version at: https://input-output-hk.github.io/cardano-haskell-packages/all-package-versions/ and update the version in the dependant packages, in their cabal files, for example: `cardano-api ^>= 8.3`


## References
1. https://github.com/input-output-hk/cardano-updates/tree/main/scripts
1. https://github.com/input-output-hk/cardano-ledger/blob/master/RELEASING.md
1. https://input-output-hk.github.io/cardano-haskell-packages/index.html
1. https://input-output-hk.github.io/cardano-engineering-handbook/policy/haskell/packaging/versioning.html
