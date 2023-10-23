{
  description = "Package build and dev environment for DraftGen";
  # Ensure nixos and cachix cache is configured.
  nixConfig = {
    extra-substituters = ["https://cache.nixos.org" "https://draftgen.cachix.org"];

    trusted-public-keys = [
      "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
      "draftgen.cachix.org-1:/BJtp3sNMKJjHdzgOP34PWlqyjGg/R8fJDPPiI9BhUY="
    ];
  };
  inputs = {
    # Unofficial library of utilities for managing Nix Flakes.
    flake-utils.url = "github:numtide/flake-utils";

    # Nix package set
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };
  outputs = {
    self,
    flake-utils,
    nixpkgs,
  }:
    flake-utils.lib.eachSystem
    (with flake-utils.lib.system; [x86_64-linux x86_64-darwin aarch64-darwin])
    (system: let
      compiler-version = "ghc946";
      # Be explicit about which overlays are in use
      overlays = {alejandra = import ./nix/overlays/alejandra/default.nix;};
      inherit (nixpkgs) lib;
      pkgs =
        builtins.foldl' (acc: overlay: acc.extend overlay)
        nixpkgs.legacyPackages.${system} (builtins.attrValues overlays);
      hpkgs = pkgs.haskell.packages.${compiler-version};
      formatter = pkgs.alejandra;
    in {
      apps.check-formatting = {
        type = "app";
        program = let
          name = "nix-check-formatting";
          script =
            pkgs.writeShellScriptBin name
            "${formatter}/bin/alejandra --check * --exclude dist-newstyle";
        in "${script}/bin/${name}";
      };
      inherit formatter;
      devShells.default = let
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
