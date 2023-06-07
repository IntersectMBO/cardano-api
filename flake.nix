{
  description = "cardano-api";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    iohkNix.url = "github:input-output-hk/iohk-nix";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";

    cardano-mainnet-mirror.url = "github:input-output-hk/cardano-mainnet-mirror";
    cardano-mainnet-mirror.flake = false;

    CHaP.url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
    CHaP.flake = false;

    # cicero
    tullia.url = "github:input-output-hk/tullia";

    # non-flake nix compatibility
    flake-compat.url = "github:edolstra/flake-compat";
    flake-compat.flake = false;
  };

  outputs = inputs: let
    supportedSystems = [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-linux"
      "aarch64-darwin"
    ];
  in
    inputs.flake-utils.lib.eachSystem supportedSystems (
      system: let
        # setup our nixpkgs with the haskell.nix overlays, and the iohk-nix
        # overlays...
        nixpkgs = import inputs.nixpkgs {
          overlays = with inputs; [
            # iohkNix.overlays.crypto provide libsodium-vrf, libblst and libsecp256k1.
            inputs.iohkNix.overlays.crypto
            # haskellNix.overlay can be configured by later overlays, so need to come before them.
            inputs.haskellNix.overlay
            # configure haskell.nix to use iohk-nix crypto librairies.
            inputs.iohkNix.overlays.haskell-nix-crypto
          ];
          inherit system;
          inherit (inputs.haskellNix) config;
        };
        inherit (nixpkgs) lib;

        # see flake `variants` below for alternative compilers
        defaultCompiler = "ghc928";
        # We use cabalProject' to ensure we don't build the plan for
        # all systems.
        cabalProject = nixpkgs.haskell-nix.cabalProject' ({config, ...}: {
          src = ./.;
          name = "cardano-api";
          compiler-nix-name = lib.mkDefault defaultCompiler;

          # we also want cross compilation to windows on linux (and only with default compiler).
          crossPlatforms = p:
            lib.optional (system == "x86_64-linux" && config.compiler-nix-name == defaultCompiler)
            p.mingwW64;

          # CHaP input map, so we can find CHaP packages (needs to be more
          # recent than the index-state we set!). Can be updated with
          #
          #  nix flake lock --update-input CHaP
          #
          inputMap = {
            "https://input-output-hk.github.io/cardano-haskell-packages" = inputs.CHaP;
          };

          # tools we want in our shell, from hackage
          shell.tools =
            {
              cabal = "3.10.1.0";
              ghcid = "0.8.8";
            }
            // lib.optionalAttrs (config.compiler-nix-name == defaultCompiler) {
              # tools that work or should be used only with default compiler
              haskell-language-server = "2.0.0.0";
              hlint = "3.5";
              stylish-haskell = "0.14.4.0";
            };
          # and from nixpkgs or other inputs
          shell.nativeBuildInputs = with nixpkgs;
            [
            ]
            ++ (with inputs.tullia.packages.${system}; [
              # for testing tullia ci locally
              tullia
              nix-systems
            ]);
          # disable Hoogle until someone request it
          shell.withHoogle = false;
          # Skip cross compilers for the shell
          shell.crossPlatforms = _: [];

          # package customizations as needed. Where cabal.project is not
          # specific enough, or doesn't allow setting these.
          modules = [
            ({pkgs, ...}: {
              packages.cardano-api = {
                configureFlags = ["--ghc-option=-Werror"];
                components = {
                  tests.cardano-api-golden = {
                    preCheck = ''
                      export CREATE_GOLDEN_FILES=1
                    '';
                  };
                };
              };
            })
          ];
        });
        # ... and construct a flake from the cabal project
        flake =
          cabalProject.flake (
            lib.optionalAttrs (system == "x86_64-linux") {
              # on linux, build/test other supported compilers
              variants = lib.genAttrs ["ghc8107"] (compiler-nix-name: {
                inherit compiler-nix-name;
              });
            }
          )
          # add cicero logic.
          // (let
            actionCiInputName = "GitHub event";
          in
            inputs.tullia.fromSimple system {
              tasks = {
                ci = {
                  config,
                  lib,
                  ...
                }: {
                  preset = {
                    nix.enable = true;
                    github.ci = {
                      # Tullia tasks can run locally or on Cicero.
                      # When no facts are present we know that we are running locally and vice versa.
                      # When running locally, the current directory is already bind-mounted into the container,
                      # so we don't need to fetch the source from GitHub and we don't want to report a GitHub status.
                      enable = config.actionRun.facts != {};
                      repository = "input-output-hk/cardano-api";
                      remote = config.preset.github.lib.readRepository actionCiInputName null;
                      revision = config.preset.github.lib.readRevision actionCiInputName null;
                    };
                  };

                  command.text = ''
                    # filter out systems that we cannot build for:
                    systems=$(nix eval .#packages --apply builtins.attrNames --json |
                      nix-systems -i |
                      jq -r 'with_entries(select(.value)) | keys | .[]')
                    targets=$(for s in $systems; do echo .#hydraJobs."$s".required; done)
                    # shellcheck disable=SC2086
                    nix build --keep-going $targets || nix build --keep-going -L $targets
                  '';

                  memory = 1024 * 8;
                  nomad.driver = "exec";
                  nomad.resources.cpu = 10000;
                };
              };

              actions = {
                "cardano-api/ci" = {
                  task = "ci";
                  io = ''
                    // This is a CUE expression that defines what events trigger a new run of this action.
                    // There is no documentation for this yet. Ask SRE if you have trouble changing this.
                    let github = {
                      #input: "${actionCiInputName}"
                      #repo: "input-output-hk/cardano-api"
                    }

                    #lib.merge
                    #ios: [
                      {#lib.io.github_push, github, #default_branch: true},
                      {#lib.io.github_pr,   github},
                    ]
                  '';
                };
              };
            });
      in
        nixpkgs.lib.recursiveUpdate flake rec {
          # add a required job, that's basically all hydraJobs.
          hydraJobs =
            nixpkgs.callPackages inputs.iohkNix.utils.ciJobsAggregates
            {
              ciJobs =
                flake.hydraJobs
                // {
                  # This ensure hydra send a status for the required job (even if no change other than commit hash)
                  revision = nixpkgs.writeText "revision" (inputs.self.rev or "dirty");
                };
            };
          legacyPackages = rec {
            inherit cabalProject nixpkgs;
            # also provide hydraJobs through legacyPackages to allow building without system prefix:
            inherit hydraJobs;
          };
          devShells = let
            profillingShell = p: {
              # `nix develop .#profiling` (or `.#ghc927.profiling): a shell with profiling enabled
              profiling = (p.appendModule {modules = [{enableLibraryProfiling = true;}];}).shell;
            };
          in
            profillingShell cabalProject
            # Additional shells for every GHC version supported by haskell.nix, eg. `nix develop .#ghc927`
            // lib.mapAttrs (compiler-nix-name: _: let
              p = cabalProject.appendModule {inherit compiler-nix-name;};
            in
              p.shell // (profillingShell p))
            nixpkgs.haskell-nix.compiler;
          # formatter used by nix fmt
          formatter = nixpkgs.alejandra;
        }
    );

  nixConfig = {
    extra-substituters = [
      "https://cache.iog.io"
    ];
    extra-trusted-public-keys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
    allow-import-from-derivation = true;
  };
}
