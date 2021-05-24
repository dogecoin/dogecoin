{ pkgs
, runJq

## The backend is an attrset of AWS/supervisord-specific methods and parameters.
, backend

## Environmental settings:
##   - either affect semantics on all backends equally,
##   - or have no semantic effect
, environment

, profile
}:

with pkgs.lib;

let

  ## The AWS node is started with:
  ## cardano-node run
  ## --config /nix/store/nywkyj205skkqy27ip3p0678977kxq0b-config-1-0.json
  ## --database-path /var/lib/cardano-node/db-bench-dense-k51-10ep-2000kU-500kD-6600esec
  ## --topology /nix/store/sb8gn8wb4s8m7a4pmkb6hvlr4fhy5vn2-topology.yaml
  ## --shelley-vrf-key /var/lib/keys/cardano-node-vrf-signing
  ## --shelley-kes-key /var/lib/keys/cardano-node-kes-signing
  ## --shelley-operational-certificate /var/lib/keys/cardano-node-operational-cert
  ## +RTS -l-agu -t --machine-readable -RTS +RTS -N2 -A16m -qg -qb -M14336.000000M -RTS
  ##
  ## nodeSpecServiceConfig :: NodeSpec -> ServiceConfig
  ##
  nodeSpecServiceConfig =
    { name, i, kind, port, isProducer }@nodeSpec:

    backend.finaliseNodeService nodeSpec
    {
      inherit port;

      forceHardForks =
        {
          shelley = { Shelley = 0; };
          allegra = { Shelley = 0; Allegra = 0; };
          mary    = { Shelley = 0; Allegra = 0; Mary = 0; };
        }.${profile.value.era};

      nodeConfig =
       backend.finaliseNodeConfig nodeSpec
         ((removeAttrs
           (recursiveUpdate environment.cardanoLib.environments.testnet.nodeConfig
             {
               Protocol             = "Cardano";
               RequiresNetworkMagic = "RequiresMagic";

               TracingVerbosity     = "NormalVerbosity";
               minSeverity          = "Debug";

               TraceMempool         = true;
               TraceTxInbound       = true;

               defaultScribes = [
                 [ "StdoutSK" "stdout" ]
               ];
               setupScribes =
                 [{
                   scKind     = "StdoutSK";
                   scName     = "stdout";
                   scFormat   = "ScJson";
                 }];
               options = {
                 mapBackends = {
                   "cardano.node.resources" = [ "KatipBK" ];
                 };
               };

               LastKnownBlockVersion-Major = 0;
               LastKnownBlockVersion-Minor = 0;
               LastKnownBlockVersion-Alt   = 0;
             })
           [ "ByronGenesisHash"
             "ShelleyGenesisHash"
           ])
       //
       ({
         shelley =
           { TestShelleyHardForkAtEpoch = 0;
           };
         allegra =
           { TestShelleyHardForkAtEpoch = 0;
             TestAllegraHardForkAtEpoch = 0;
           };
         mary =
           { TestShelleyHardForkAtEpoch = 0;
             TestAllegraHardForkAtEpoch = 0;
             TestMaryHardForkAtEpoch    = 0;
           };
       }).${profile.value.era});
    };

  ## Given an env config, evaluate it and produce the node service.
  ## Call the given function on this service.
  ##
  ## nodeServiceConfigService :: NodeServiceConfig -> NodeService
  ##
  nodeServiceConfigService =
    serviceConfig:
    let
    systemdCompat.options = {
      systemd.services = mkOption {};
      systemd.sockets = mkOption {};
      users = mkOption {};
      assertions = mkOption {};
    };
    eval = let
      extra = {
        services.cardano-node = {
          enable = true;
          cardanoNodePkgs = pkgs;
        } // serviceConfig;
      };
    in evalModules {
      prefix = [];
      modules = import ../../nixos/module-list.nix ++ [ systemdCompat extra ];
      args = { inherit pkgs; };
    };
    in eval.config.services.cardano-node;

  ##
  ## node-services :: Map NodeName (NodeSpec, ServiceConfig, Service, NodeConfig, Script)
  ##
  node-services = mapAttrs
    (_: { name, i, ... }@nodeSpec:
      let
        serviceConfig = nodeSpecServiceConfig    nodeSpec;
        service       = nodeServiceConfigService serviceConfig;
      in {
        nodeSpec = {
          value = nodeSpec;
          JSON  = runJq "node-spec-${name}.json"
                    ''--null-input --sort-keys
                      --argjson x '${__toJSON nodeSpec}'
                    '' "$x";
        };

        serviceConfig = {
          value = serviceConfig;
          JSON  = runJq "node-service-config-${name}.json"
                    ''--null-input --sort-keys
                      --argjson x '${__toJSON serviceConfig}'
                    '' "$x";
        };

        service = {
          value = service;
          JSON  = runJq "node-service-${name}.json"
                    ''--null-input --sort-keys
                      --argjson x '${__toJSON service}'
                    '' "$x";
        };

        nodeConfig = {
          value = service.nodeConfig;
          JSON  = runJq "node-config-${name}.json"
                    ''--null-input --sort-keys
                      --argjson x '${__toJSON service.nodeConfig}'
                    '' "$x";
        };

        topology = rec {
          JSON  = backend.topologyForNode { inherit profile nodeSpec; };
          value = __fromJSON (__readFile JSON);
        };

        startupScript =
          pkgs.writeScript "startup-${name}.sh"
            ''
            #!${pkgs.stdenv.shell}

            ${service.script}
            '';
      })
    profile.node-specs.value;
in
{
  inherit node-services;
}
