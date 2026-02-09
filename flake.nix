{
  description = "be2";

  inputs = {
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };
    ghciwatch-compat = {
      url = "github:evanrelf/ghciwatch-compat";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "github:NixOS/nixpkgs";
    systems.url = "github:nix-systems/default";
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = import inputs.systems;

      perSystem = { config, inputs', pkgs, system, ... }: {
        _module.args.pkgs =
          import inputs.nixpkgs {
            localSystem = system;
            overlays = [
              inputs.ghciwatch-compat.overlays.default
            ];
          };

        packages = {
          default = config.packages.be2;

          be2 =
            pkgs.haskell.lib.justStaticExecutables
              (pkgs.haskellPackages.callCabal2nix "be2" ./. { });

          be2-parser =
            pkgs.rustPlatform.buildRustPackage (attrs: {
              name = "be2-parser";
              src = ./tools/be2-parser;
              cargoLock.lockFile = "${attrs.src}/Cargo.lock";
            });
        };

        devShells.default =
          pkgs.mkShell {
            inputsFrom = [ config.packages.be2.env ];
            packages = [
              pkgs.cabal-install
              pkgs.cargo
              pkgs.clippy
              pkgs.ghciwatch-compat-ghcid
              pkgs.nixpkgs-fmt
              pkgs.rustc
              pkgs.rustfmt
            ];
          };
      };
    };
}
