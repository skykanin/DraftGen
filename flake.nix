{
  description = "Package build and dev environment for DraftGen";
  inputs = {
    # Unofficial library of utilities for managing Nix Flakes.
    flake-utils.url = "github:numtide/flake-utils";

    # Nix package set
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };
  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachSystem
    (with flake-utils.lib.system; [ x86_64-linux x86_64-darwin aarch64-darwin ])
    (system:
      let
        compiler-version = "ghc946";
        pkgs = nixpkgs.legacyPackages.${system};
        inherit (pkgs) lib;
        hpkgs = pkgs.haskell.packages.${compiler-version};
      in {
        devShells.default =
          let
            tools = with hpkgs; [
              pkgs.haskell-language-server
              ghc
              cabal-install
              cabal-fmt
              ghcid
              fourmolu_0_10_1_0
            ];
            libraries = [
              pkgs.zlib
            ];
            libraryPath = "${lib.makeLibraryPath libraries}";
          in
            hpkgs.shellFor {
              name = "dev-shell";

              packages = p: [];
              withHoogle = true;
              buildInputs = tools ++ libraries;

              LD_LIBRARY_PATH = libraryPath;
              LIBRARY_PATH = libraryPath;
            };
    });
}
