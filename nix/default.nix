{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
, customConfig ? {}
, sourcesOverride ? {}
, gitrev ? null
}:
let
  flakeSources = let
    flakeLock = (builtins.fromJSON (builtins.readFile ../flake.lock)).nodes;
    compat = s: builtins.fetchGit {
      url = "https://github.com/${s.locked.owner}/${s.locked.repo}.git";
      inherit (s.locked) rev;
      ref = s.original.ref or "master";
    };
  in {
    "haskell.nix" = compat flakeLock.haskellNix;
    "iohk-nix" = compat flakeLock.iohkNix;
  };
  sources = flakeSources // sourcesOverride;
  haskellNix = import sources."haskell.nix" { inherit system sourcesOverride; };
  nixpkgs = haskellNix.sources.nixpkgs-unstable;
  iohkNix = import sources.iohk-nix { inherit system; };
  # for inclusion in pkgs:
  overlays =
    # Haskell.nix (https://github.com/input-output-hk/haskell.nix)
    haskellNix.nixpkgsArgs.overlays
    # haskell-nix.haskellLib.extra: some useful extra utility functions for haskell.nix
    ++ iohkNix.overlays.haskell-nix-extra
    ++ iohkNix.overlays.crypto
    # iohkNix: nix utilities:
    ++ iohkNix.overlays.iohkNix
    ++ iohkNix.overlays.utils
    # our own overlays:
    ++ [
      (pkgs: _: {
        gitrev = if gitrev == null
          then iohkNix.commitIdFromGitRepoOrZero ../.git
          else gitrev;
        customConfig = pkgs.lib.recursiveUpdate
          (import ../custom-config pkgs.customConfig)
          customConfig;
        inherit (pkgs.iohkNix) cardanoLib;
        # commonLib: mix pkgs.lib with iohk-nix utils and our own:
        commonLib = with pkgs; lib // cardanoLib // iohk-nix.lib
          // import ./util.nix { inherit haskell-nix; }
          # also expose our sources, nixpkgs and overlays
          // { inherit overlays sources nixpkgs; };
      })
      # And, of course, our haskell-nix-ified cabal project:
      (import ./pkgs.nix)
    ];

  pkgs = import nixpkgs {
    inherit system crossSystem overlays;
    config = haskellNix.nixpkgsArgs.config // config;
  };

in pkgs
