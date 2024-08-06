{
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }@attrs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          haskellNix.overlay
        ];

        project =
          pkgs.haskell-nix.cabalProject' {
            src = ./.;
            compiler-nix-name = "ghc96";

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
