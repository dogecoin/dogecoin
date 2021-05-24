############################################################################
# Docker image builder
#
# To build and load into the Docker engine:
#
#   docker load -i $(nix-build -A dockerImage --no-out-link)
#
# To launch with pre-loaded configuration, using the NETWORK env.
# An example using a docker volume to persist state:
#
#   docker run -v /data -e NETWORK=mainnet inputoutput/cardano-node
#
# Provide a complete command otherwise:
#
#   docker run -v $PWD/configuration/defaults/byron-mainnet:/configuration \
#     inputoutput/cardano-node run \
#      --config /configuration/configuration.yaml \
#      --topology /configuration/topology.json \
#      --database-path /db
#
# Mount a volume into /ipc for establishing cross-container communication via node.socket
#
#   docker run -v node-ipc:/ipc inputoutput/cardano-node
#   docker run -v node-ipc:/ipc inputoutput/some-node-client
############################################################################

{ pkgs
, commonLib
, dockerTools

# The main contents of the image.
, cardano-cli
, scripts

# Set gitrev to null, to ensure the version below is used
, gitrev ? null

# Other things to include in the image.
, bashInteractive
, buildPackages
, cacert
, coreutils
, curl
, glibcLocales
, iana-etc
, iproute
, iputils
, socat
, utillinux
, writeScriptBin
, runtimeShell
, lib
, exe
, script
, repoName ? "inputoutput/${exe}"
}:

let

  # Layer of tools which aren't going to change much between versions.
  baseImage = dockerTools.buildImage {
    name = "${repoName}-env";
    contents = [
      cardano-cli       # Provide cardano-cli capability
      bashInteractive   # Provide the BASH shell
      cacert            # X.509 certificates of public CA's
      coreutils         # Basic utilities expected in GNU OS's
      curl              # CLI tool for transferring files via URLs
      glibcLocales      # Locale information for the GNU C Library
      iana-etc          # IANA protocol and port number assignments
      iproute           # Utilities for controlling TCP/IP networking
      iputils           # Useful utilities for Linux networking
      socat             # Utility for bidirectional data transfer
      utillinux         # System utilities for Linux
    ];
    # set up /tmp (override with TMPDIR variable)
    extraCommands = ''
      mkdir -m 0777 tmp
    '';
  };
  # Image with all iohk-nix network configs or utilizes a configuration volume mount
  # To choose a network, use `-e NETWORK testnet`
    clusterStatements = lib.concatStringsSep "\n" (lib.mapAttrsToList (env: scripts: ''
      elif [[ "$NETWORK" == "${env}" ]]; then
        exec ${scripts.${script}}
    '') scripts);
  nodeDockerImage = let
    entry-point = writeScriptBin "entry-point" ''
      #!${runtimeShell}
      if [[ -z "$NETWORK" ]]; then
        exec ${pkgs.${exe}}/bin/${exe} $@
      ${clusterStatements}
      else
        echo "Managed configuration for network "$NETWORK" does not exist"
      fi
    '';
  in dockerTools.buildImage {
    name = "${repoName}";
    fromImage = baseImage;
    tag = "${gitrev}";
    created = "now";   # Set creation date to build time. Breaks reproducibility
    contents = [ entry-point ];
    config = {
      EntryPoint = [ "${entry-point}/bin/entry-point" ];
    };
  };

in nodeDockerImage
