{ config, lib, pkgs, ... }:

# notes:
# this service exposes an http port, and connects to a cardano-node over a UNIX socket
let
  cfg = config.services.cardano-submit-api;
  inherit (cfg.cardanoNodePkgs) commonLib;
  envConfig = cfg.environment;
in {
  options = {
    services.cardano-submit-api = {
      enable = lib.mkEnableOption "enable the cardano-submit-api api";
      script = lib.mkOption {
        internal = true;
        type = lib.types.package;
      };
      package = lib.mkOption {
        type = lib.types.package;
        default = cfg.cardanoNodePkgs.cardanoNodeHaskellPackages.cardano-submit-api.components.exes.cardano-submit-api;
      };
      port = lib.mkOption {
        type = lib.types.port;
        default = 8090;
      };
      listenAddress = lib.mkOption {
        type = lib.types.str;
        default = "127.0.0.1";
      };
      socketPath = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
      };
      config = lib.mkOption {
        type = lib.types.nullOr lib.types.attrs;
        default = commonLib.defaultExplorerLogConfig;
      };
      network = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        description = "network name";
        default = null;
      };
      environment = lib.mkOption {
        type = lib.types.nullOr lib.types.attrs;
        default = commonLib.environments.${cfg.network};
      };
      cardanoNodePkgs = lib.mkOption {
        type = lib.types.attrs;
        default = import ../. {};
        defaultText = "cardano-node pkgs";
        description = ''
          The cardano-node packages and library that should be used.
          Main usage is sharing optimization:
          reduce eval time when service is instantiated multiple times.
        '';
      };
    };
  };
  config = let
    envNodeCfg = cfg.environment.nodeConfig;
    shelleyGenesisParams = __fromJSON (__readFile envNodeCfg.ShelleyGenesisFile);
    envFlag = if cfg.network == "mainnet" then "--mainnet" else "--testnet-magic ${toString shelleyGenesisParams.networkMagic}";
  in lib.mkIf cfg.enable {
    services.cardano-submit-api.script = pkgs.writeShellScript "cardano-submit-api" ''
      ${if (cfg.socketPath == null) then ''if [ -z "$CARDANO_NODE_SOCKET_PATH" ]
      then
        echo "You must set \$CARDANO_NODE_SOCKET_PATH"
        exit 1
      fi'' else "export \"CARDANO_NODE_SOCKET_PATH=${cfg.socketPath}\""}
      exec ${cfg.package}/bin/cardano-submit-api --socket-path "$CARDANO_NODE_SOCKET_PATH" ${envFlag} \
            --port ${toString cfg.port} \
            --listen-address ${cfg.listenAddress} \
            --config ${builtins.toFile "submit-api.json" (builtins.toJSON cfg.config)}
    '';
    systemd.services.cardano-submit-api = {
      serviceConfig = {
        ExecStart = config.services.cardano-submit-api.script;
        DynamicUser = true;
      };
      wantedBy = [ "multi-user.target" ];
      after = [ "cardano-node.service" ];
    };
  };
}
