# DraftGen
An MTG booster pack generator for [Tabletop Simulator](https://store.steampowered.com/app/286160/Tabletop_Simulator/).

## Develop
### Prerequisites
If you want to use the development environment includeded in this repository you
will need to install [nix](https://nixos.org/download.html#nix-quick-install) and
enable experimental features by running:
- `nix-env -iA nixpkgs.nixFlakes`
- `mkdir ~/.config/nix && echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf`

### Start contributing!
Run `nix develop` to enter the development environment.

This adds everything listed in the `flake.nix` files `devShell` attribute to your path.

### Run
On the first run you will be prompted to run `cabal update`.

After that you can run the project with `cabal run`.

### Build
To build the project using cabal you can run
`cabal build [...opts]`
However if you want to build the executable through nix you can run
`nix-build -A lastpass-tui.components.exes.lpt`
The resulting executable will be under `./result/bin/lpt`
