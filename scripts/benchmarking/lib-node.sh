#!/usr/bin/env bash
# shellcheck disable=SC2034,SC2154,SC2039,SC1007,SC2207,SC2145
## We need to come up with a design on how to handle --help compositionally.
DESC="

Cardano node runner.

"
USAGE="

  Node flags:

    --config-name NAME   Profile in configuration/defaults to take config from
    --topology-name NAME Profile in configuration/defaults to take topology from
    --state NAME         Suffix for node database & local comms socket
    --delegate-id ID     Delegate number
    --port PORTNO        Listen on this port

    --profile MODE       Enable profiling:
       time space space-module space-closure space-type space-retainer space-bio

    --no-profile         Suppress profiling in .hp/.prof/.eventlog
    --no-stats           Suppress runtime stats in .stats
    --redirect-stdout FILE
                         Redirect stdout to the specified file
    --no-stdout          Redirect stdout to /dev/null
    --profile-suffix SUF
                         Extra suffix to add to the profile name

"

## Defaults

default_node_config='byron-mainnet'
default_node_port=7776

## Running node setups
#
#  This allows running a node with cherry-picked:
#
#   - configuration
#   - topology
#   - node db/socket
#
## run_node
##   :: String ConfigId
##   -> String TopologyId
##   -> String StateId
##   -> Int Port
##   -> String ExtraNodeCLIArgs
##   -> IO ()
run_node() {
        if test -n "${verbose}"
        then run_node_verbose "$@"
        else run_node_quiet   "$@"
        fi
}

run_node_verbose() {
        _run_node run_verbose "$@"
}

run_node_quiet() {
        _run_node run_quiet   "$@"
}

_run_node() {
        local LODE_RUNNER="$1"; shift

        dprint "_run_node args: $*"

        local config_id=${default_node_config}
        local topo_id= state_id=
        local port=${default_node_port}
        local delegate_id=
        local profile=${COMMON_NODE_PROFILING}
        local profile_suffix=
        local no_stats=
        local redirect_stdout=
        local tag=
        while test -n "$1"
        do case "$1" in
           --config-name )      config_id=$2;   shift;;
           --topology-name )    topo_id=$2;     shift;;
           --state )            state_id=$2;    shift;;
           --delegate-id )      delegate_id=$2; shift;;
           --port )             port=$2;        shift;;
           --redirect-stdout )  redirect_stdout=$2; shift;;
           --no-stdout )        redirect_stdout='/dev/null'; shift;;
           ## Sadly, due to arg splitting, some runner args
           ## have to be handled here..
           --no-profile )       profile=;;
           --no-stats )         no_stats=t;;
           --profile )          profile=$2;     shift;;
           --profile-suffix )   profile_suffix=$2; shift;;
           --tag )              tag=$2;         shift;;
           * ) break;; esac; shift; done

        local RUNNER_ARGS=(
        )
        if test -n "${profile}"; then RUNNER_ARGS+=(
          --profile "${profile}"
        ); fi
        if test -n "${profile_suffix}"; then RUNNER_ARGS+=(
          --profile-suffix "${profile_suffix}"
        ); fi
        if test -n "${no_stats}"; then RUNNER_ARGS+=(
          --no-stats
        ); fi
        if test -n "${tag}"; then RUNNER_ARGS+=(
          --tag "${tag}"
        ); fi

        dprint "config_id=${config_id}"
        dprint "topo_id=${topo_id}"
        dprint "state_id=${state_id}"
        dprint "delegate_id=${delegate_id}"
        dprint "port=${port}"
        dprint "tag=${tag}"
        dprint "run_node binary extra args:  $*"

        local topo_id="${topo_id:-$config_id}"
        local state_id="${state_id:-$config_id}"
        local db_root="${__COMMON_SRCROOT}/db"
        local sock_root="${__COMMON_SRCROOT}/socket"

        local NODE_ARGS=(
          --database-path    "${db_root}/${state_id}/"
          --socket-path      "${sock_root}/${state_id}-socket"
          --port             "${port}"
          "$@"
        )

        if test ! -d "${db_root}"
        then dprint "Creating DB root: '${db_root}'"
             mkdir -p "${db_root}"
        fi
        if test ! -d "${sock_root}"
        then dprint "Creating socket root: '${sock_root}'"
             mkdir -p "${sock_root}"
        fi

        if test "${config_id}" != 'custom'; then
        NODE_ARGS+=(
          --config   "${configuration}/defaults/${config_id}/configuration.yaml"); fi
        if test "${topo_id}"   != 'custom'; then
        NODE_ARGS+=(
          --topology "${configuration}/defaults/${topo_id}/topology.json"); fi

        if test -n "${delegate_id}"; then
        NODE_ARGS+=(
          --signing-key            $(printf "${genesis_root}/delegate-keys.%03d.key" "${delegate_id}")
          --delegation-certificate $(printf "${genesis_root}/delegation-cert.%03d.json" "${delegate_id}")); fi

        dprint "_run_node exec: ${LODE_RUNNER} ${RUNNER_ARGS[*]} 'cardano-node' $* ${NODE_ARGS[@]}"

        local final_stdout_redirect=${redirect_stdout:+>"${redirect_stdout/}"}

        eval "${LODE_RUNNER}" "${RUNNER_ARGS[@]}" \
           'cardano-node' 'cardano-node' run "${NODE_ARGS[@]}" ${final_stdout_redirect}
}
