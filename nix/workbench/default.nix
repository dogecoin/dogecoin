{ lib
, stdenv
, pkgs
, git
, graphviz
, jq
, moreutils
, makeWrapper
, runCommand
, customConfig
, cardano-cli
, cardano-topology

, useCabalRun
}:

with lib; with customConfig.localCluster;

let
  nixWbMode =
    if useCabalRun
    then "cabal-exes+nix-wb"
    else "nix-exes+nix-wb";

  workbench' = tools:
    stdenv.mkDerivation {
      pname = "workbench";

      version = "0.1";

      src = ./.;

      buildInputs = [ jq makeWrapper ];

      buildPhase = ''
        patchShebangs .
      '';

      postFixup = ''
        wrapProgram "$out/bin/wb" --argv0 wb --add-flags "--set-mode ${nixWbMode}" \
        --prefix PATH ":" ${pkgs.lib.makeBinPath tools}
      '';

      installPhase = ''
        mkdir -p         $out/bin
        cp -a wb profiles *.sh *.jq $out/bin
      '';

      dontStrip = true;
    };

  workbench = workbench'
    [ git graphviz
      jq
      moreutils

      cardano-cli
      cardano-topology
    ];

  runWorkbench =
    name: command:
    runCommand name {} ''
      ${workbench}/bin/wb ${command} > $out
    '';

  runWorkbenchJqOnly =
    name: command:
    runCommand name {} ''
      ${workbench' [jq]}/bin/wb ${command} > $out
    '';

  runJq =
    name: args: query:
    runCommand name {} ''
      args=(${args})
      ${jq}/bin/jq '${query}' "''${args[@]}" > $out
    '';

  exeCabalOp = op: exe:
    toString [ "cabal" op "${exe}" "--"];

  checkoutWbMode =
    if useCabalRun
    then "cabal-exes+checkout-wb"
    else "nix-exes+checkout-wb";

  shellHook = ''
    export WORKBENCH_BACKEND=${./.}/supervisor.sh

      ${optionalString workbenchDevMode
    ''
    echo 'workbench:  dev mode enabled, calling wb directly from checkout (instead of using Nix store)' >&2

    WORKBENCH_CARDANO_NODE_REPO_ROOT=$(git rev-parse --show-toplevel)
    WORKBENCH_EXTRA_FLAGS=

    function wb() {
      $WORKBENCH_CARDANO_NODE_REPO_ROOT/nix/workbench/wb --set-mode ${checkoutWbMode} $WORKBENCH_EXTRA_FLAGS "$@"
    }

    export WORKBENCH_CARDANO_NODE_REPO_ROOT WORKBENCH_EXTRA_FLAGS
    export -f wb

    ''}

    ${optionalString useCabalRun
    ''
    echo 'workbench:  cabal-inside-nix-shell mode enabled, calling cardano-* via 'cabal run' (instead of using Nix store)' >&2

    function cardano-cli() {
      ${exeCabalOp "exec" "cardano-cli"} "$@"
    }

    function cardano-node() {
      ${exeCabalOp "exec" "cardano-node"} "$@"
    }

    function cardano-topology() {
      ${exeCabalOp "exec" "cardano-topology"} "$@"
    }

    export -f cardano-cli cardano-node cardano-topology

    ''}

    function workbench-prebuild-executables() {
      ${optionalString useCabalRun
        ''
      git log -n1 --alternate-refs --pretty=format:"%Cblue%h %Cred%cr %Cgreen%D %Cblue%s%Creset"
      echo -n "workbench:  prebuilding executables (because of useCabalRun):"
      for exe in cardano-cli cardano-node cardano-topology
      do echo -n " $exe"
         cabal -v0 build exe:$exe >/dev/null || return 1
      done
      echo
        ''}
      true
    }
    export -f workbench-prebuild-executables

    '';

  generateProfiles =
    { pkgs

    ## The backend is an attrset of AWS/supervisord-specific methods and parameters.
    , backend

    ## Environment arguments:
    ##   - either affect semantics on all backends equally,
    ##   - or have no semantic effect
    , envArgs
    }:
    rec {
      profile-names-json =
        runWorkbenchJqOnly "profile-names.json" "profiles list";

      profile-names =
        __fromJSON (__readFile profile-names-json);

      environment =
        ## IMPORTANT:  keep in sync with envArgs in 'supervisord-cluster/default.nix/envArgs'.
        with envArgs; rec {
          inherit cardanoLib stateDir;

          JSON = runWorkbench "environment.json"
          ''env compute-config \
            --cache-dir "${cacheDir}" \
            --base-port ${toString basePort} \
            ${optionalString staggerPorts "--stagger-ports"} \
          '';
          value = __fromJSON (__readFile JSON);
        };

      mkProfile =
        profileName:
        pkgs.callPackage ./profiles
          { inherit
              pkgs
              runWorkbenchJqOnly runJq workbench
              backend
              environment
              profileName;
          };

      profiles = genAttrs profile-names mkProfile;

      profilesJSON =
        runWorkbench "all-profiles.json" "profiles generate-all";
    };

  initialiseProfileRunDirShellScript =
    profile: runDir:
      __concatStringsSep "\n"
      (flip mapAttrsToList profile.node-services
        (name: svc:
          ''
          cp -f ${svc.serviceConfig.JSON} ${runDir}/${name}/service-config.json
          cp -f ${svc.nodeConfig.JSON}    ${runDir}/${name}/config.json
          cp -f ${svc.topology.JSON}      ${runDir}/${name}/topology.json
          cp -f ${svc.startupScript}      ${runDir}/${name}/start.sh
          ''
        ));
in
{
  inherit workbench runWorkbench runJq;

  inherit generateProfiles initialiseProfileRunDirShellScript shellHook;
}
