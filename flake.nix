{
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        overlays = [ haskellNix.overlay ];

        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };

        project = pkgs.haskell-nix.project' {
          src = ./.;
          compiler-nix-name = "ghc963";
          shell.tools = {
            cabal = {};
            hlint = {};
            haskell-language-server = {};
          };
        };

        flake = project.flake {};
    in
      pkgs.lib.recursiveUpdate flake {
        packages.default = flake.packages."safe-int:exe:safe-int";
      });
}
