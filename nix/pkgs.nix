# our packages overlay
final: prev: with final;
  let
    compiler = config.haskellNix.compiler or "ghc8104";
  in {
  cardanoNodeProject = import ./haskell.nix {
    inherit compiler
      pkgs
      lib
      stdenv
      haskell-nix
      buildPackages
      gitrev
      ;
  };
  cardanoNodeHaskellPackages = cardanoNodeProject.hsPkgs;
  cardanoNodeProfiledHaskellPackages = (import ./haskell.nix {
    inherit compiler
      pkgs
      lib
      stdenv
      haskell-nix
      buildPackages
      gitrev
      ;
    inherit (cardanoNodeProject) projectPackages;
    inherit (cardanoNodeProject.projectModule) src cabalProjectLocal;
    profiling = true;
  }).hsPkgs;
  cardanoNodeEventlogHaskellPackages = (import ./haskell.nix {
    inherit compiler
      pkgs
      lib
      stdenv
      haskell-nix
      buildPackages
      gitrev
      ;
    inherit (cardanoNodeProject) projectPackages;
    inherit (cardanoNodeProject.projectModule) src cabalProjectLocal;
    eventlog = true;
  }).hsPkgs;
  cardanoNodeAssertedHaskellPackages = (import ./haskell.nix {
    inherit compiler
      pkgs
      lib
      stdenv
      haskell-nix
      buildPackages
      gitrev
      ;
    inherit (cardanoNodeProject) projectPackages;
    inherit (cardanoNodeProject.projectModule) src cabalProjectLocal;
    assertedPackages = [
      "ouroboros-consensus"
      "ouroboros-consensus-cardano"
      "ouroboros-consensus-byron"
      "ouroboros-consensus-shelley"
      "ouroboros-network"
      "network-mux"
    ];
  }).hsPkgs;

  #Grab the executable component of our package.
  inherit (cardanoNodeHaskellPackages.cardano-node.components.exes) cardano-node;
  inherit (cardanoNodeHaskellPackages.cardano-cli.components.exes) cardano-cli;
  inherit (cardanoNodeHaskellPackages.cardano-topology.components.exes) cardano-topology;
  inherit (cardanoNodeHaskellPackages.bech32.components.exes) bech32;
  inherit (cardanoNodeHaskellPackages.cardano-submit-api.components.exes) cardano-submit-api;
  cardano-node-profiled = cardanoNodeProfiledHaskellPackages.cardano-node.components.exes.cardano-node;
  cardano-node-eventlogged = cardanoNodeEventlogHaskellPackages.cardano-node.components.exes.cardano-node;
  cardano-node-asserted = cardanoNodeAssertedHaskellPackages.cardano-node.components.exes.cardano-node;

  # expose the db-converter and cardano-ping from the ouroboros-network we depend on
  inherit (cardanoNodeHaskellPackages.ouroboros-consensus-byron.components.exes) db-converter;
  inherit (cardanoNodeHaskellPackages.network-mux.components.exes) cardano-ping;

  cabal = haskell-nix.tool compiler "cabal" {
    version = "latest";
    inherit (cardanoNodeProject) index-state;
  };

  hlint = haskell-nix.tool compiler "hlint" {
    version = "3.2.7";
    inherit (cardanoNodeProject) index-state;
  };

  cardanolib-py = callPackage ./cardanolib-py {};

  scripts = lib.recursiveUpdate (import ./scripts.nix { inherit pkgs; })
    (import ./scripts-submit-api.nix { inherit pkgs; });

  dockerImage = let
    defaultConfig = {
      stateDir = "/data";
      dbPrefix = "db";
      socketPath = "/ipc/node.socket";
    };
  in callPackage ./docker.nix {
    exe = "cardano-node";
    scripts = import ./scripts.nix {
      inherit pkgs;
      customConfigs = [ defaultConfig customConfig ];
    };
    script = "node";
  };

  submitApiDockerImage = let
    defaultConfig = {
      socketPath = "/ipc/node.socket";
    };
  in callPackage ./docker.nix {
    exe = "cardano-submit-api";
    scripts = import ./scripts-submit-api.nix {
      inherit pkgs;
      customConfigs = [ defaultConfig customConfig ];
    };
    script = "submit-api";
  };

  # NixOS tests run a node and submit-api and validate it listens
  nixosTests = import ./nixos/tests {
    inherit pkgs;
  };

  clusterTests = import ./supervisord-cluster/tests { inherit pkgs; };
}
