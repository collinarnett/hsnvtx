{
  nixConfig = {
    extra-trusted-substituters = [
      "https://cache.flox.dev"
    ];
    extra-trusted-public-keys = [
      "flox-cache-public-1:7F4OyH7ZCnFhcze3fJdfyXYLQw/aV7GEed86nQ7IsOs="
    ];
  };

  description = "Haskell bindings to NVIDIA Tools Extension (NVTX)";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    git-hooks-nix.url = "github:cachix/git-hooks.nix";

    git-hooks-nix.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nixpkgs, flake-parts, git-hooks-nix, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {

      imports = [ inputs.git-hooks-nix.flakeModule ];
      systems = [ "x86_64-linux" ];

      flake = {
        overlays.default = final: prev: {
          haskellPackages = prev.haskell.packages.ghc98.override {
            overrides = hfinal: hprev: {
              hsnvtx = hfinal.callCabal2nix "hsnvtx" ./. {
                nvToolsExt = final.cudaPackages.cuda_nvtx;
              };
            };
          };
        };
      };

      perSystem = { config, system, pkgs, lib, ... }: {
        _module.args.pkgs = import inputs.nixpkgs {
          inherit system;
          config.cudaSupport = true;
          config.allowUnfree = true;
        };

        pre-commit.settings.hooks = {
          ruff-format.enable = true;
          nixfmt-rfc-style.enable = true;
          cabal-fmt.enable = true;
          ormolu.enable = true;
        };

        devShells.default = pkgs.haskell.packages.ghc98.shellFor {
          packages = ps:
            [
              (ps.callCabal2nix "hsnvtx" ./. {
                nvToolsExt = pkgs.cudaPackages.cuda_nvtx;
              })
            ];

            nativeBuildInputs =
              with pkgs;
              [
                # Debugging
                gdb
                nvtopPackages.nvidia
                cudaPackages.nsight_systems
                # Haskell
                cabal-install
                haskellPackages.cabal-fmt
                haskell.packages.ghc98.haskell-language-server
                ormolu
              ];
        };
      };
    };
}
