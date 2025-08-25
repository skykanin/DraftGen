# DraftGen
[![GitHub issues by-label](https://img.shields.io/github/workflow/status/skykanin/DraftGen/Release?logo=GitHub&style=for-the-badge)](https://github.com/skykanin/DraftGen/actions?query=workflow%3A%22Release%22) [![simple haskell ](https://img.shields.io/badge/Simple-Haskell-purple?style=for-the-badge)](https://www.simplehaskell.org/) [![license](https://img.shields.io/github/license/skykanin/DraftGen?color=%23BD0000&style=for-the-badge)](https://www.gnu.org/licenses/gpl-3.0.en.html)

An MTG booster pack generator for [Tabletop Simulator](https://store.steampowered.com/app/286160/Tabletop_Simulator/).

# Documentation
Use `--help` for documentation.

DraftGen lets you generate booster packs for MTG. The default options are set to simulate the rules and drop rates of regular booster packs. The default card set to generate packs from is the latest core set. These options can be changed by passing arguments through the command line. All arguments are listed in the help section `dg --help`. DraftGen will print the paths where the resulting json files are written depending upon your platform.

Lastly copy the json files to your `Saved Objects` folder for Tabletop Simulator and you can access them in the game.

There is also an argument `get-card` which lets you search for a single card. This
can be used to retrieve helper cards such as a double-faced substitute card.

# Develop
## Prerequisites
If you want to use the development environment includeded in this repository you
will need to install [nix](https://nixos.org/download.html#nix-quick-install) and
enable experimental features by running:
- `nix-env -iA nixpkgs.nixFlakes`
- `mkdir ~/.config/nix && echo "experimental-features = nix-command flakes" >> ~/.config/nix/nix.conf`

## Start contributing!
Run `nix develop` to enter the development environment.

This adds everything listed in the `flake.nix` files `devShell` attribute to your path. It is also possible to automatically hook into this environment using `direnv` see instructions on how to set this up [here](https://github.com/direnv/direnv/wiki/Nix).

## Run
On the first run you will be prompted to run `cabal update`.

After that you can run the project with `cabal run`. To pass command line arguments to cabal you run `cabal run <artifact> -- <args go here>`.

## Build

### With Cabal
To build the project using cabal you can run
`cabal build [...opts]`

### With Nix
To build the project through nix you can run
```sh
nix build .#draftgen
```
If you want a static binary instead you can get that with
```sh
nix build .#draftgen-static
```

## Retrieve changelog
To get the changelog between the latest tagged commit and the previous tagged commit one can run `cabal run -z GenChangelog.hs` to print out the list.
