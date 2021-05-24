{ system ? builtins.currentSystem
, crossSystem ? null
# allows to cutomize haskellNix (ghc and profiling, see ./nix/haskell.nix)
, config ? {}
# override scripts with custom configuration
, customConfig ? {}
# allows to override dependencies of the project without modifications,
# eg. to test build against local checkout of nixpkgs and iohk-nix:
# nix build -f default.nix cardano-node --arg sourcesOverride '{
#   iohk-nix = ../iohk-nix;
# }'
, sourcesOverride ? {}
# pinned version of nixpkgs augmented with overlays (iohk-nix and our packages).
, pkgs ? import ./nix/default.nix { inherit system crossSystem config customConfig sourcesOverride gitrev; }
# Git sha1 hash, to be passed when not building from a git work tree.
, gitrev ? null
}:
with pkgs; with commonLib;
let

  haskellPackages = recRecurseIntoAttrs
    # the Haskell.nix package set, reduced to local packages.
    (selectProjectPackages cardanoNodeHaskellPackages);

  packages = {
    inherit haskellPackages
      cardano-node cardano-node-profiled cardano-node-eventlogged
      cardano-cli db-converter cardano-ping
      scripts environments dockerImage submitApiDockerImage bech32;

    nixosTests = recRecurseIntoAttrs nixosTests;

    # so that eval time gc roots are cached (nix-tools stuff)
    inherit (cardanoNodeProject) roots plan-nix;

    inherit (haskellPackages.cardano-node.identifier) version;

    exes = mapAttrsRecursiveCond (as: !(isDerivation as)) rewriteStatic (collectComponents' "exes" haskellPackages);

    # `tests` are the test suites which have been built.
    tests = collectComponents' "tests" haskellPackages;
    # `benchmarks` (only built, not run).
    benchmarks = collectComponents' "benchmarks" haskellPackages;

    checks = recurseIntoAttrs {
      # `checks.tests` collect results of executing the tests:
      tests = collectChecks haskellPackages;

      hlint = callPackage hlintCheck {
        inherit (cardanoNodeProject.projectModule) src;
      };
    };

    shell = import ./shell.nix {
      inherit pkgs;
      withHoogle = true;
    };
};
in packages
