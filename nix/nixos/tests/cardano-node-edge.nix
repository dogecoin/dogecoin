{ pkgs, ... }:
with pkgs;
{
  name = "cardano-node-edge-test";
  nodes = {
    machine = { config, ... }: {
      nixpkgs.pkgs = pkgs;
      imports = [
        ../.
      ];
      services.cardano-node = {
        enable = true;
        systemdSocketActivation = true;
        port = 3001;
        hostAddr = "127.0.0.1";
        environment = "mainnet";
        topology = commonLib.mkEdgeTopology {
          port = 3001;
          edgeNodes = [ "127.0.0.1" ];
        };
        cardanoNodePkgs = pkgs;
        nodeConfig = config.services.cardano-node.environments.${config.services.cardano-node.environment}.nodeConfig // {
          hasPrometheus = [ config.services.cardano-node.hostAddr 12798 ];
          # Use Journald output:
          setupScribes = [{
            scKind = "JournalSK";
            scName = "cardano";
            scFormat = "ScText";
          }];
          defaultScribes = [
            [
              "JournalSK"
              "cardano"
            ]
          ];
        };
      };
      systemd.services.cardano-node.serviceConfig.Restart = lib.mkForce "no";
      services.cardano-submit-api = {
        enable = true;
        port = 8101;
        network = "mainnet";
        socketPath = config.services.cardano-node.socketPath;
        cardanoNodePkgs = pkgs;
      };
      systemd.services.cardano-submit-api.serviceConfig.SupplementaryGroups = "cardano-node";
    };
  };
  testScript = ''
    start_all()
    machine.wait_for_unit("cardano-node.service")
    machine.succeed("stat /run/cardano-node")
    machine.succeed("stat /run/cardano-node/node.socket")
    machine.wait_for_open_port(12798)
    machine.wait_for_open_port(3001)
    machine.succeed("systemctl status cardano-node")
    machine.succeed(
        "${cardano-ping}/bin/cardano-ping -h 127.0.0.1 -c 1 -q --json | ${jq}/bin/jq -c"
    )
    machine.wait_for_open_port(8101)
    machine.succeed("systemctl status cardano-submit-api")
  '';

}
