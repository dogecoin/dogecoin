{ pkgs
, customConfigs ? [ pkgs.customConfig ]
}:
with pkgs.commonLib;
let
  mkScript = envConfig: let
    service = evalService {
      inherit pkgs customConfigs;
      serviceName = "cardano-node";
      modules = [
        ./nixos/cardano-node-service.nix
        ({config, ...}: {
          services.cardano-node = {
            hostAddr = mkDefault "0.0.0.0";
            environment = mkDefault envConfig.name;
            cardanoNodePkgs = mkDefault pkgs;
            stateDir = mkDefault "state-node-${config.services.cardano-node.environment}";
            runtimeDir = mkDefault null;
          } // optionalAttrs (envConfig ? topology) {
            topology = mkDefault envConfig.topology;
          };
        })
      ];
    };

  in pkgs.writeScriptBin "cardano-node-${service.environment}" ''
    #!${pkgs.runtimeShell}
    set -euo pipefail
    ${service.script} $@
  '';

  debugDeps = with pkgs; [
    coreutils
    findutils
    gnugrep
    gnused
    postgresql
    strace
    lsof
    dnsutils
    bashInteractive
    iproute
    curl
    netcat
    bat
    tree
  ];

in forEnvironments (environment: recurseIntoAttrs rec {
  node = mkScript environment;
  node-debug = pkgs.symlinkJoin {
    inherit (node) name;
    paths = [ node ] ++ debugDeps;
  };
})
