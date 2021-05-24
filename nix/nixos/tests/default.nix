{ pkgs
, supportedSystems ? [ "x86_64-linux" ]
}:

with pkgs;
with pkgs.commonLib;

 let
  forAllSystems = genAttrs supportedSystems;
  importTest = fn: args: system: let
    imported = import fn;
    test = import (pkgs.path + "/nixos/tests/make-test-python.nix") imported;
  in test ({
    inherit pkgs system config;
  } // args);
  callTest = fn: args: forAllSystems (system: hydraJob (importTest fn args system));
in rec {
  # only tests that port is open since the test can't access network to actually sync
  cardanoNodeEdge  = callTest ./cardano-node-edge.nix {};
}
