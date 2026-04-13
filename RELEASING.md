# Versioning and release process

## Release process

### Using Github Action

To release a new version of `cardano-api`, use the [Actions > Release](https://github.com/intersectmbo/cardano-api/actions/workflows/release.yml) workflow (or via CLI):

```bash
gh workflow run release.yml -f package=cardano-api
```

To override the auto-computed version:
```bash
gh workflow run release.yml -f package=cardano-api -f version=8.5.0.0
```

The action ([`.github/workflows/release.yml`](.github/workflows/release.yml)) will:
- Compute the next version from `.changes/` fragments (or use the explicit one)
- Create a `release/<package>-<version>` branch
- Run `herald batch` -- update changelog, bump `.cabal` version, remove consumed changelog fragments
- Commit, tag, and push the branch and tag
- Open a release PR against the target branch
- Add a `release` changelog fragment for the next cycle

The release PR URL will be printed at the end of the workflow execution.

>:high_brightness: **Note**
>
>Usually the release PR should only contain a changelog update and a version bump.
>If you are making a release which aims to contain everything from `master` branch, there should be no additional code changes in the release PR.
>An exception to that would be a release with a backported fix for example, where the release PR should contain required code changes too.

>:bulb: **Tip**
>
>Hold off on tagging and merging of the release PR, until CHaP PR gets merged. See: p. 5 in [Releasing to `cardano-haskell-packages`](#releasing-to-cardano-haskell-packages).

#### Manual release process

In case one would like to have more manual control over the release process, it's possible to do what the GHA is doing by hand.

1. Run herald to obtain the next version providing the released package name:
```bash
nix run github:input-output-hk/cardano-dev#herald -- next cardano-api
```

1. Create new branch e.g. `release/cardano-api-8.5.0.0`:
```bash
git checkout -b release/cardano-api-8.5.0.0
```

1. Run herald to generate changelog and release commit:
```bash
nix run github:input-output-hk/cardano-dev#herald -- batch --commit-tag
```

1. Push the branch, open a release PR.


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


### GitHub releases pipeline

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

If a bug affecting a release is discovered long after that release has been made (and potentially after several subsequent releases), it may be necessary to create a patch that fixes each of the affected legacy versions. We can achieve this through the following steps:

1. Identify the commit that introduced the issue and determine the list of affected releases by checking which tags include that commit (GitHub often displays this list under the commit title).

1. Generate a new patch versions for each of the affected releases. New versions are typically calculated by incrementing the third digit of the latest release in each affected series (see the [Version bumping](./RELEASING.md#version-bumping) section for more information). By increasing the third or fourth digit, we ensure dependencies automatically pick up the fix.

1. For each version we decide to patch, create a branch starting from the tag of the unpatched version. For example, for `10.14.2.0`, we create a new branch `release/cardano-api-10.14.2.0` based on the tag `cardano-api-10.14.1.0`.

1. From each of the release branches, create a dedicated branch for the backported fix, such as `backport/cardano-api-10.14/fix-bug`. Cherry-pick the bug fix onto this backport branch, resolving any conflicts that arise. And issue and merge a Pull Request (PR) from the backport branch to the patch release branch (e.g., `release/cardano-api-10.14.2.0`).

   This step can be repeated if multiple bugs need to be patched; create one PR per bug fix.

1. Trigger the **Release** workflow (see above how to do it). 
  You can also manually create changelog using `herald` (see description above).

1. Publish the releases to CHaP (Cardano Haskell Packages) in the same way than for normal releases (see [Releasing to `cardano-haskell-packages`](./RELEASING.md#releasing-to-cardano-haskell-packages) section): Use `add-from-github.sh` to create a CHaP entry for each of our new releases, open a PR in the CHaP repository, and once the build passes and is approved, we merge them.

1. Unlike in the release workflow for normal releases, for backports, we will **not** merge the release PR into `master` (assuming the backported fix is already there). Attempting to do so may cause issues with conflicts, CI errors, and the risk of the same changes being applied several times to `master`.

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
1. https://github.com/IntersectMBO/cardano-ledger/blob/master/RELEASING.md
1. https://chap.intersectmbo.org/index.html
1. https://input-output-hk.github.io/cardano-engineering-handbook/policy/haskell/packaging/versioning.html

<!-- vim: set spell textwidth=0: -->

