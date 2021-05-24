{ pkgs
, lib
, stateDir
, basePort
, node-services
  ## Last-moment overrides:
, extraSupervisorConfig
}:

with lib;

let
  ##
  ## nodeSvcSupervisorProgram :: NodeService -> SupervisorConfSection
  ##
  ## Refer to: http://supervisord.org/configuration.html#program-x-section-settings
  ##
  nodeSvcSupervisorProgram = { nodeSpec, service, startupScript, ... }:
    nameValuePair "program:${nodeSpec.value.name}" {
      directory      = "${service.value.stateDir}";
      command        = "${startupScript}";
      stdout_logfile = "${service.value.stateDir}/stdout";
      stderr_logfile = "${service.value.stateDir}/stderr";
    };

  ##
  ## supervisorConf :: SupervisorConf
  ##
  ## Refer to: http://supervisord.org/configuration.html
  ##
  supervisorConf =
    {
      supervisord = {
        logfile = "${stateDir}/supervisor/supervisord.log";
        pidfile = "${stateDir}/supervisor/supervisord.pid";
        strip_ansi = true;
      };
      supervisorctl = {};
      inet_http_server = {
        port = "127.0.0.1:9001";
      };
      "rpcinterface:supervisor" = {
        "supervisor.rpcinterface_factory" = "supervisor.rpcinterface:make_main_rpcinterface";
      };
    }
    //
    listToAttrs
      (mapAttrsToList (_: nodeSvcSupervisorProgram) node-services)
    //
    {
      "program:webserver" = {
        command = "${pkgs.python3}/bin/python -m http.server ${toString (basePort - 1)}";
        directory = "${stateDir}/shelley/webserver";
      };
    }
    //
    extraSupervisorConfig;

in
  pkgs.writeText "supervisor.conf"
    (generators.toINI {} supervisorConf)
