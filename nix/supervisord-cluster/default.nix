let
  basePortDefault    = 30000;
  cacheDirDefault    = "${__getEnv "HOME"}/.cache";
  stateDirDefault    = "state-cluster";
  profileNameDefault = "default-mary";
in
{ pkgs
, workbench
, lib
, bech32
, basePort              ? basePortDefault
, stateDir              ? stateDirDefault
, cacheDir              ? cacheDirDefault
, extraSupervisorConfig ? {}
, useCabalRun           ? false
, workbenchDevMode      ? false
, enableEKG             ? true
##
, profileName           ? profileNameDefault
, batchName             ? "plain"
, ...
}:
with lib;
let
  backend =
    rec
    { ## Generic Nix bits:
      topologyForNode =
        { profile, nodeSpec }:
        let inherit (nodeSpec) name i; in
        workbench.runWorkbench "topology-${name}.json"
          (if nodeSpec.isProducer
           then "topology for-local-node     ${toString i}   ${profile.topology.files} ${toString basePort}"
           else "topology for-local-observer ${profile.name} ${profile.topology.files} ${toString basePort}");

      finaliseNodeService =
        { name, i, isProducer, ... }: svc: recursiveUpdate svc
          ({
            stateDir       = stateDir + "/${name}";
            ## Everything is local in the supervisord setup:
            socketPath     = "node.socket";
            topology       = "topology.json";
            nodeConfigFile = "config.json";
          } // optionalAttrs useCabalRun {
            executable     = "cabal run exe:cardano-node --";
          } // optionalAttrs isProducer {
            operationalCertificate = "../genesis/node-keys/node${toString i}.opcert";
            kesKey         = "../genesis/node-keys/node-kes${toString i}.skey";
            vrfKey         = "../genesis/node-keys/node-vrf${toString i}.skey";
          });

      finaliseNodeConfig =
        { port, ... }: cfg: recursiveUpdate cfg
          ({
            ShelleyGenesisFile   = "../genesis/genesis.json";
            ByronGenesisFile     = "../genesis/byron/genesis.json";
          } // optionalAttrs enableEKG {
            hasEKG               = port + supervisord.portShiftEkg;
            hasPrometheus        = [ "127.0.0.1" (port + supervisord.portShiftPrometheus) ];
            setupBackends = [
              "EKGViewBK"
            ];
          });

      ## Backend-specific Nix bits:
      supervisord =
        {
          inherit
            extraSupervisorConfig;

          portShiftEkg        = 100;
          portShiftPrometheus = 200;

          mkSupervisorConf =
            profile:
            pkgs.callPackage ./supervisor-conf.nix
            { inherit (profile) node-services;
              inherit
                pkgs lib stateDir
                basePort
                extraSupervisorConfig;
            };
        };
    };

  ## IMPORTANT:  keep in sync with envArgs in 'workbench/default.nix/generateProfiles/environment'.
  envArgs =
    {
      inherit (pkgs) cardanoLib;
      inherit
        stateDir
        cacheDir basePort;
      staggerPorts = true;
    };

  workbenchProfiles = workbench.generateProfiles
    { inherit pkgs backend envArgs; };
in

let
  profile = workbenchProfiles.profiles."${profileName}"
    or (throw "No such profile: ${profileName};  Known profiles: ${toString (__attrNames workbenchProfiles.profiles)}");

  inherit (profile.value) era composition monetary;

  path = makeBinPath path';
  path' =
    [ bech32 pkgs.jq pkgs.gnused pkgs.coreutils pkgs.bash pkgs.moreutils
    ]
    ## In dev mode, call the script directly:
    ++ optionals (!workbenchDevMode)
    [ workbench.workbench ];

  start = pkgs.writeScriptBin "start-cluster" ''
    set -euo pipefail

    workbench-prebuild-executables

    batch_name=${batchName}

    while test $# -gt 0
    do case "$1" in
        --batch-name ) batch_name=$2; shift;;
        --trace | --debug ) set -x;;
        --trace-wb | --trace-workbench ) export WORKBENCH_EXTRA_FLAGS=--trace;;
        * ) break;; esac; shift; done

    export PATH=$PATH:${path}

    wb backend assert-is supervisor
    wb backend pre-run-hook    "${stateDir}"

    wb_run_allocate_args=(
        --cache-dir            "${cacheDir}"
        --base-port             ${toString basePort}
        --stagger-ports
        --port-shift-ekg        100
        --port-shift-prometheus 200
      )
    wb run allocate $batch_name ${profile.name} "''${wb_run_allocate_args[@]}"
    rm -f                      "${stateDir}"
    ln -sf         run/current "${stateDir}"

    current_run_path=$(wb run current-path)
    ${workbench.initialiseProfileRunDirShellScript profile "$current_run_path"}

    wb_run_start_args=(
        --supervisor-conf      "${backend.supervisord.mkSupervisorConf profile}"
      )
    wb run start $(wb run current-name) "''${wb_run_start_args[@]}"

    echo 'workbench:  cluster started. Run `stop-cluster` to stop'
  '';
  stop = pkgs.writeScriptBin "stop-cluster" ''
    set -euo pipefail

    while test $# -gt 0
    do case "$1" in
        --trace | --debug ) set -x;;
        * ) break;; esac; shift; done

    ${pkgs.python3Packages.supervisor}/bin/supervisorctl stop all
    if wb backend is-running
    then
      if test -f "${stateDir}/supervisor/cardano-node.pids"
      then kill $(<${stateDir}/supervisor/supervisord.pid) $(<${stateDir}/supervisor/cardano-node.pids)
      else pkill supervisord
      fi
      echo "workbench:  cluster terminated"
      rm -f ${stateDir}/supervisor/supervisord.pid ${stateDir}/supervisor/cardano-node.pids
    else
      echo "workbench:  cluster is not running"
    fi
  '';

in
{
  inherit workbench;
  inherit (workbenchProfiles) profilesJSON;
  inherit profile stateDir start stop;
}
