{ pkgs
, mkCluster
}: let
  inherit (pkgs) mkCluster cardano-cli cardanolib-py cardano-node;
  stateDir = "./state-cluster-test";
  # We want a really short duration for tests
  cluster' = mkCluster {
    genesisParams = {
      slotLength = 0.1;
      decentralisationParam = 0.8;
    };
    inherit stateDir;
  };
  # Library bash functions for cluster tests
  pythonDeps = with pkgs.python3Packages; [ pkgs.python3 requests pyyaml docopt cardanolib-py ];
  clusterDeps = [ cluster'.start cluster'.stop pkgs.jq cardano-cli cardano-node ];
  # If any command in start-cluster, stop-cluster or cluster-commands exits
  # with a status other than 0, the cluster will fail the test
  mkClusterTest = name: testScript: pkgs.runCommand name { buildInputs = clusterDeps ++ pythonDeps; } ''
    export CARDANO_NODE_SOCKET_PATH=${stateDir}/bft0.socket
    start-cluster
    export NETWORK_MAGIC=$(jq .networkMagic < ${stateDir}/shelley/genesis.json)
    cp ${testScript} script.py
    chmod -R u+w ${stateDir}
    cp ${../genesis-utxo.vkey} ${stateDir}/shelley/genesis-utxo.vkey
    cp ${../genesis-utxo.skey} ${stateDir}/shelley/genesis-utxo.skey
    cp ${./base.py} base.py
    python3 ./script.py --network-magic "$NETWORK_MAGIC" --state-dir "${stateDir}" --num-delegs 1
    stop-cluster
    touch $out
  '';

in {
  # Base Test: Start cluster, register 2 pools, transfer funds, stop cluster
  base = mkClusterTest "base" ./base.py;
  updateProposal = mkClusterTest "update-proposal" ./update-proposal.py;
}
