{
  description = "Package build and dev environment for DraftGen";
  inputs = {
    # Unofficial library of utilities for managing Nix Flakes.
    flake-utils.url = "github:numtide/flake-utils";

    # Nix package set
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };
  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachSystem
    (with flake-utils.lib.system; [ x86_64-linux x86_64-darwin aarch64-darwin ])
    (system:
      let
        compiler-version = "ghc923";
        pkgs = nixpkgs.legacyPackages.${system};
        inherit (pkgs) lib;
        hpkgs = pkgs.haskell.packages.${compiler-version};
      in {
        devShells.default =
          let
            tools = [
              pkgs.haskell-language-server
              hpkgs.ghc
              hpkgs.cabal-install
              hpkgs.ghcid
              hpkgs.fourmolu_0_7_0_1
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
