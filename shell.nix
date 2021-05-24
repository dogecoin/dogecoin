let defaultCustomConfig = import ./custom-config defaultCustomConfig;
# This file is used by nix-shell.
# It just takes the shell attribute from default.nix.
in
{ config ? {}
, sourcesOverride ? {}
, withHoogle ? defaultCustomConfig.withHoogle
, clusterProfile ? defaultCustomConfig.localCluster.profileName
, autoStartCluster ? defaultCustomConfig.localCluster.autoStartCluster
, workbenchDevMode ? defaultCustomConfig.localCluster.workbenchDevMode
, customConfig ? {
    inherit withHoogle;
    localCluster =  {
      inherit autoStartCluster workbenchDevMode;
      profileName = clusterProfile;
    };
  }
, pkgs ? import ./nix {
    inherit config sourcesOverride customConfig;
  }
}:
with pkgs;
let
  inherit (pkgs) customConfig;
  inherit (customConfig) withHoogle localCluster;
  inherit (localCluster) autoStartCluster workbenchDevMode;
  commandHelp =
    ''
      echo "
        Commands:
          * nix flake update --update-input <iohkNix|haskellNix> - update imput
          * cardano-cli - used for key generation and other operations tasks
          * wb - cluster workbench
          * start-cluster - start a local development cluster
          * stop-cluster - stop a local development cluster

      "
    '';

  # This provides a development environment that can be used with nix-shell or
  # lorri. See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  # NOTE: due to some cabal limitation,
  #  you have to remove all `source-repository-package` entries from cabal.project
  #  after entering nix-shell for cabal to use nix provided dependencies for them.
  mkCluster =
    { useCabalRun }:
    callPackage ./nix/supervisord-cluster
      { inherit useCabalRun;
        workbench = pkgs.callPackage ./nix/workbench { inherit useCabalRun; };
      };

  shell =
    let cluster = mkCluster { useCabalRun = true; };
    in cardanoNodeProject.shellFor {
    name = "cabal-dev-shell";

    inherit withHoogle;

    packages = lib.attrVals cardanoNodeProject.projectPackages;

    tools = {
      haskell-language-server = {
        version = "latest";
        inherit (cardanoNodeProject) index-state;
      };
    };

    # These programs will be available inside the nix-shell.
    nativeBuildInputs = with haskellPackages; [
      cardano-ping
      cabalWrapped
      ghcid
      weeder
      nixWrapped
      pkgconfig
      profiteur
      profiterole
      python3Packages.supervisor
      ghc-prof-flamegraph
      sqlite-interactive
      tmux
      pkgs.git
      pkgs.hlint
    ]
    ## Workbench's main script is called directly in dev mode.
    ++ lib.optionals (!workbenchDevMode)
    [
      cluster.workbench.workbench
    ]
    ## Local cluster not available on Darwin,
    ## because psmisc fails to build on Big Sur.
    ++ lib.optionals (!stdenv.isDarwin)
    [
      cluster.start
      cluster.stop
    ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;

    shellHook = ''
      ${cluster.workbench.shellHook}

      ${lib.optionalString autoStartCluster ''
      function atexit() {
          if wb backend is-running
          then echo "workbench:  stopping cluster (because 'autoStartCluster' implies this):"
               stop-cluster
          fi
      }
      trap atexit EXIT
      ''}
      unset NIX_ENFORCE_PURITY

      ${lib.optionalString autoStartCluster ''
      echo "workbench:  starting cluster (because 'autoStartCluster' is true):"
      start-cluster
      ''}

      ${commandHelp}

      set +e
    '';
  };

  devops =
    let cluster = mkCluster { useCabalRun = false; };
    in stdenv.mkDerivation {
    name = "devops-shell";
    nativeBuildInputs = [
      nixWrapped
      cardano-cli
      bech32
      cardano-node
      python3Packages.supervisor
      python3Packages.ipython
      cluster.start
      cluster.stop
      cardanolib-py
      cluster.workbench.workbench
    ];
    shellHook = ''
      echo "DevOps Tools" \
      | ${figlet}/bin/figlet -f banner -c \
      | ${lolcat}/bin/lolcat

      wb explain-mode

      ${cluster.workbench.shellHook}

      # Socket path default to first node launched by "start-cluster":
      export CARDANO_NODE_SOCKET_PATH=$(wb backend get-node-socket-path ${cluster.stateDir})

      # Unless using specific network:
      ${lib.optionalString (__hasAttr "network" customConfig) ''
        export CARDANO_NODE_SOCKET_PATH="$PWD/state-node-${customConfig.network}/node.socket"
        ${lib.optionalString (__hasAttr "utxo" pkgs.commonLib.cardanoLib.environments.${customConfig.network}) ''
          # Selfnode and other test clusters have public secret keys that we pull from iohk-nix
          echo "To access funds use UTXO_SKEY and UTXO_VKEY environment variables"
          export UTXO_SKEY="${pkgs.commonLib.cardanoLib.environments.${customConfig.network}.utxo.signing}"
          export UTXO_VKEY="${pkgs.commonLib.cardanoLib.environments.${customConfig.network}.utxo.verification}"
        ''}

      ''}

      echo "NOTE: you may need to use a github access token if you hit rate limits with nix flake update:"
      echo '      edit ~/.config/nix/nix.conf and add line `access-tokens = "github.com=23ac...b289"`'
      ${commandHelp}

      ${lib.optionalString autoStartCluster ''
      echo "workbench:  starting cluster (because 'autoStartCluster' is true):"
      start-cluster
      ''}
    '';
  };

in

 shell // { inherit devops; }
