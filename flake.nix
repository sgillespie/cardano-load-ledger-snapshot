{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    iohkNix.url = "github:input-output-hk/iohk-nix";

    chap = {
      url = "github:intersectmbo/cardano-haskell-packages?ref=repo";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays =
          builtins.attrValues (inputs.iohkNix.overlays) ++ [
            haskellNix.overlay
          ];

        project =
          pkgs.haskell-nix.cabalProject' {
            src = ./.;
            compiler-nix-name = "ghc96";

            inputMap = {
              "https://chap.intersectmbo.org/" = inputs.chap;
            };

            shell.tools = {
              cabal = {};
              fourmolu = {};
              hlint = {};
              haskell-language-server = {};
            };

            shell.buildInputs = with pkgs; [
              nixpkgs-fmt
            ];
          };


        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };

        flake = project.flake { };

      in
        flake // {
          # Built by `nix build .`
          packages.default = flake.packages."cardano-load-ledger-snapshot:exe:cardano-load-ledger-snapshot";
        });
}
