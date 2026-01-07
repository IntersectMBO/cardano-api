# Versioning and release process

## Release process

When making a new release, firstly you have to decide on a new version number for the release.

### Version bumping
`cardano-api` is using [Haskell Package Versioning Policy](https://pvp.haskell.org/) for numbering each release version.

In order to decide which version number needs to be bumped up, it is necessary to know what was the latest released version of a package of the specific series you are targeting (e.g., the latest 8.4.x release).

Three simple ways are:
* look at the latest version on [`cardano-haskell-packages` (aka **CHaP**)](https://chap.intersectmbo.org/index.html) - the most reliable way
* current version in the changelog
* look at the latest git tag for the version

When you found out the current version of `cardano-api`, the next step is to find out if the changes within the scope of the release are breaking or not.
To make this process easier, each pull request has information about that - [see the `compatibility` field in the example changelog here](https://github.com/IntersectMBO/cardano-api/pull/53).
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
The full documentation for scripts is located in [`cardano-updates` repository](https://github.com/IntersectMBO/cardano-updates/blob/master/scripts/README.md).

This part requires user to have the following tools installed on your local machine:
* https://github.com/cli/cli
* https://jqlang.github.io/jq/
* https://mikefarah.gitbook.io/yq/

Alternatively, you can enter the `cardano-api` nix-shell, where these tools are already available, by running
```bash
nix develop
```
in `cardano-api` directory.

>:bulb: **Tip**
>
> Steps which are only required when performing this process for the first time are marked with :four_leaf_clover: .

In order to generate changelog files in markdown format use the following steps:

1. :four_leaf_clover: Clone the `cardano-dev` repo at the same level as `cardano-api`:
    ```bash
    git clone https://github.com/input-output-hk/cardano-dev
    ```
    Check that you're authenticated to GitHub using GitHub CLI:
    ```bash
    gh auth status
    ```
    If you're not authenticated, follow the steps shown on the command output.

1. Create a release branch in `cardano-api`, for example:
    ```bash
    git checkout -b release/cardano-api-8.3.0.0
    ```
    >:high_brightness: **Note**
    >
    >A separate branch needs to be created for every cabal package you're planning to release.
    >For example if you'd like to release both `cardano-api` and `cardano-api-gen` you need to create two branches (with correct version numbers): `release/cardano-api-8.3.0.0` and `release/cardano-api-gen-8.1.0.0`.

1. Download all PRs data from the `cardano-api` repo.
    This will take some time if the number of all PRs is large. From `cardano-api` directory, run:
    ```bash
    ../cardano-dev/scripts/download-prs.sh IntersectMBO/cardano-api
    ```
    The downloaded PRs can be inspected in `~/.cache/cardano-updates/` directory.

    >:high_brightness: **Note**
    >
    >It would be advisable to make changelog entries corrections in the descriptions of GitHub PRs itself, as this would let us use GitHub PRs as a single source of truth for the changelog generation process.
    >This also means, that after making a change to a changelog in a PR description, the whole procedure needs to be restarted from this download step.
    >The output changelog can be reviewed in the next step.

1. Generate markdown changelogs from Yaml detail file providing the hash of the previous release tag in the command line argument.
    For example for the changelog between the tag `cardano-api-8.2.0.0` and `HEAD`:
    ```bash
    ../cardano-dev/scripts/generate-pr-changelogs.sh IntersectMBO/cardano-api cardano-api-8.2.0.0..HEAD
    ```
    This will process downloaded PRs and use those marked with `feature` or `bug` to produce the changelog to the standard output.

    >:bulb: **Tip**
    >
    >You can sort all tags ascendingly using: `git show-ref --tags --dereference | sort -V -t '/' -k 3,3`

1. Add generated changelog in the previous step to `CHANGELOG.md` file in respective cabal package in `cardano-api` repository, near the top of the file, adding a new section for the version being prepared, for example: `## 8.3.0.0`.
    After doing that, create a PR from a new branch back to `master`.
    Make sure that the release PR contains:
    * updated changelogs
    * bumped version fields in cabal files

>:high_brightness: **Note**
>
>Usually the release PR should only contain a changelog update and a version bump.
>If you are making a release which aims to contain everything from `master` branch, there should be no additional code changes in the release PR.
>An exception to that would be a release with a backported fix for example, where the release PR should contain required code changes too.

>:bulb: **Tip**
>
>Hold off on tagging and merging of the release PR, until CHaP PR gets merged. See: p. 5 in [Releasing to `cardano-haskell-packages`](#releasing-to-cardano-haskell-packages).

>:bulb: **Tip**
>
>Avoid unnecessary rebasing of the release PR to prevent accidental inclusion of unwanted changes.
>The release PR should be merged using merge queue with an explicit merge commit.


### Releasing to `cardano-haskell-packages`

**After verifying the release PR diff** that it contains the correct contents, it should be uploaded to `cardano-haskell-packages` (aka **CHaP**).

Detailed description of the release process is described in [CHaP repository README](https://github.com/intersectmbo/cardano-haskell-packages#how-to-add-a-new-package-version).
Briefly speaking, it requires executing of the following steps:

1. :four_leaf_clover:  Clone `cardano-haskell-packages`:
    ```bash
    git clone https://github.com/IntersectMBO/cardano-haskell-packages
    cd cardano-haskell-packages
    ```

1.  Run the following script, replacing `<commit-hash>` with the just tagged commit hash:
    ```bash
    ./scripts/add-from-github.sh https://github.com/IntersectMBO/cardano-api <commit-hash> cardano-api cardano-api-gen
    ```
    List all packages names for release in the script invocation, after the commit hash like in the example above.
    The script will create a separate commit for each package.

1. Push your `HEAD` to a new branch, and create a PR in CHaP.
    An example release PR which you might want to use as a reference: https://github.com/intersectmbo/cardano-haskell-packages/pull/345 .

1. Merge the PR - you don't need additional approvals for that if you belong to the correct GitHub access group.

    After package gets released, you can check the released version at: https://chap.intersectmbo.org/all-package-versions/ and update the version in the dependant packages, in their cabal files, for example: `cardano-api ^>= 8.3`
    Don't forget to bump the CHaP index in cabal.project and flake.lock too.
    See [`CONTRIBUTING.md` section on updating dependencies](https://github.com/IntersectMBO/cardano-cli/blob/master/CONTRIBUTING.md#updating-dependencies) how to to do so.

>:bulb: **Tip**
>
>CHaP CI build can fail due to various reasons, like invalid haddock syntax.
>Tagging and merging the release PR after CHaP PR allows to accommodate for potential issues which can arise here.

### Tagging the release version

After successful CI build in CHaP, the release PR (in the `cardano-api` repo) can be tagged and then enqueued to merge.

1. Make sure that:
   1. Your `HEAD` is on the commit you're going to tag - **this has to be the same commit which was released to CHaP**
   1. Your `HEAD` is in `release/packagename-version.x` branch history on the `origin` remote (the `.x` suffix is optional).

1. Use the following script to prepare the tag:
   ```bash
   ../cardano-dev/scripts/tag.sh
   ```
   This script will extract the version numbers from cabal files, create the tag and **push it to the `origin` remote**.
   Please note that the tagging process will fail if either:
   1. The tag already exists on the origin remote
   1. The `packagename/CHANGELOG.md` does not contain an entry for the new version.

#### GitHub release pipeline

If the repo has a release pipeline configured, it will be triggered on the tag push.

1. If the release pipeline (if any, see e.g. [here for CLI](https://github.com/IntersectMBO/cardano-cli/actions/workflows/release-upload.yaml)) fails
   during the _Get specific check run status_ step of the _Wait for Hydra check-runs_ pipeline, this means Hydra did not
   run on the tagged commit.
   This can happen if the tagged commit is not the remote `HEAD` when you create the PR, or if you change the tag after the fact.

   To make hydra run on the tagged commit, checkout this commit, create a branch whose name starts with `ci/`
   (see [Hydra's code](https://github.com/input-output-hk/hydra-tools/commit/854620a3426957be72fa618c4dfc68f03842617b)) and push this branch.
   Hydra will pick it up and you can then retrigger release creation as follows (the branch from which you execute this command
   doesn't matter much): `gh workflow run "Release Upload" -r $(git branch --show-current) -f target_tag=cardano-api-8.2.0.0`.
1. If a GitHub release is automatically created by the CI, as visible on https://github.com/IntersectMBO/cardano-api/releases,
   undraft the release by clicking the pen on the top right, then untick _Set as a pre-release_, and
   finally select _Update release_.

   >:warning: **GitHub bug**
   >
   > If you try to undraft a PR using the [gh API](https://docs.github.com/fr/rest/releases/releases?apiVersion=2022-11-28#update-a-release),
   > you will observe that the `PATCH` endpoint messes up existing metadata of the release (author, associated commit, etc.).
   > So you HAVE to use the UI, as described above.

## Backporting

If a bug affecting a release is discovered long after that release has been made (and potentially after several subsequent releases), it may be necessary to create a patch that fixes each of the affected legacy versions. In this section, we describe how this is done.

### 1. Identifying the version numbers

Once the commit that introduced the issue is identified, you can determine the list of affected releases by checking which tags include that commit (GitHub often displays this list under the commit title).

After identifying the versions for the affected releases, you should generate a new patch versions for each of them. New versions are typically calculated by incrementing the third digit of the latest release in each affected series (see the [Version bumping](./RELEASING.md#version-bumping) section for more information). By increasing the third or fourth digit, we ensure dependencies automatically pick up the fix.

### 2. Backporting the fix

For each version we decide to patch, we create a branch starting from the tag of the unpatched version. For example, for `10.14.2.0`, we create a new branch `release/cardano-api-10.14.2.0` based on the tag `cardano-api-10.14.1.0`.

Next, we create a dedicated branch for the backported fix, such as `backport/cardano-api-10.14/fix-bug`. We cherry-pick the bug fix onto this backport branch, resolving any conflicts that arise. Finally, we issue a Pull Request (PR) from the backport branch to the patch release branch (e.g., `release/cardano-api-10.14.2.0`).

The reason we use a PR for the backport is to ensure the changelog gathering script works correctly in later steps. (Alternatively, commits can be added directly to the release branch, but this requires manually writing the changelog entries later.)

Finally, we merge the backport PR. This process can be repeated if multiple bugs need to be patched; ideally, create one PR per bug fix.

### 3. Release preparation (changelog & versioning)

Once the backport is merged, we proceed to prepare the release. We generate the changelog entry running the usual scripts:

```bash
../cardano-dev/scripts/download-prs.sh IntersectMBO/cardano-api
../cardano-dev/scripts/generate-pr-changelogs.sh IntersectMBO/cardano-api cardano-api-10.14.1.0..HEAD

```

We add the generated entry to the `CHANGELOG.md`, bump the version number in the `.cabal` file, and create a release PR targeting `master`.

### 4. Publication to CHaP (Cardano Haskell Packages)

In the same way than for normal releases (see [Releasing to `cardano-haskell-packages`](./RELEASING.md#releasing-to-cardano-haskell-packages) section), we now use `add-from-github.sh` to create a CHaP entry for each of our new releases. We open a PR in the CHaP repository, and once the build passes and is approved, we merge it.

### 5. Finalization and Tagging

Lastly, we use `tag.sh` to create a release tag in the repo of the package we are fixing.

Unlike in the release workflow for normal releases, for backports, we will **not** merge the release PR into `master` (assuming the backported fix is already there). Attempting to do so may cause issues with conflicts, CI errors, and the risk of the same changes being applied several times to `master`.

Instead, we will just close the backport release PRs and open a new PR, directly forked from `master`, that will simply update the change-log for all the release PRs we haven't merged. This should be done just once for all the releases, after all the patches have been released.

## Troubleshooting

### Build fails due to `installed package instance does not exist`
If you notice that your build fails due to an error similar to the following one:
```
 Configuring library for cardano-ledger-conway-1.8.0.0..
Error: cabal: The following package dependencies were requested
--dependency='cardano-ledger-alonzo=cardano-ledger-alonzo-1.4.1.0-b1d2cdacf3fecf8f57f465701c6cc39a19521597ceee354f7a1ea4688dec9d9f'
--dependency='cardano-ledger-babbage=cardano-ledger-babbage-1.4.4.0-3f75b69fa5a14215f31de708afe86d5d69fbecea8ff284dc3265e0701eada7b6'
however the given installed package instance does not exist.
```
increase the cabal cache version number in [.github/workflows/haskell.yml](.github/workflows/haskell.yml):
```yaml
CABAL_CACHE_VERSION: "2023-08-22"
```
Usually setting this date to the current date is enough.
If it is already set to the current date, you can add a suffix to it - the important part is to make it unique across all builds which occurred until now, for example `2023-08-22-1`.
This issue happens due to frequent cache collisions in the [`cabal-cache`](https://github.com/haskell-works/cabal-cache).

## References
1. https://github.com/input-output-hk/cardano-updates/tree/master/scripts
1. https://github.com/IntersectMBO/cardano-ledger/blob/master/RELEASING.md
1. https://chap.intersectmbo.org/index.html
1. https://input-output-hk.github.io/cardano-engineering-handbook/policy/haskell/packaging/versioning.html

<!-- vim: set spell textwidth=0: -->

