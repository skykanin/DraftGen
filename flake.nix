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
    # Nix module based flake organisation library
    flake-parts.url = "github:hercules-ci/flake-parts";
    # Nix package set
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };
  outputs = inputs @ {flake-parts, ...}:
    flake-parts.lib.mkFlake {inherit inputs;} {
      systems = ["x86_64-linux" "aarch64-linux" "aarch64-darwin"];
      perSystem = {
        lib,
        pkgs,
        system,
        self',
        ...
      }: {
        _module.args.pkgs = let
          overlays = {
            alejandra = import ./nix/overlays/alejandra/default.nix;
          };
        in
          builtins.foldl' (acc: overlay: acc.extend overlay)
          inputs.nixpkgs.legacyPackages.${system} (builtins.attrValues overlays);

        formatter = pkgs.alejandra;

        apps.check-formatting = {
          type = "app";
          program = let
            name = "nix-check-formatting";
            script =
              pkgs.writeShellScriptBin name
              "${self'.formatter}/bin/alejandra --check * --exclude dist-newstyle";
          in "${script}/bin/${name}";
        };
        devShells.default = let
          hpkgs = pkgs.haskell.packages.ghc98;
          tools = with hpkgs; [
            cabal-fmt
            cabal-install
            fourmolu
            ghc
            ghc-prof-flamegraph
            ghcid
            profiteur
          ] ++ (with pkgs; [
            haskell-language-server
            nixd
          ]);
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
      };
    };
}
