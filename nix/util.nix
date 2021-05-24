{ haskell-nix }:

with haskell-nix.haskellLib;
{

  inherit
    selectProjectPackages
    collectComponents';

  inherit (extra)
    recRecurseIntoAttrs
    collectChecks;
}
