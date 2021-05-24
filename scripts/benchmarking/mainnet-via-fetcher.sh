#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2034

app_usage() {
        cat <<EOF

Run this like:

   scripts/mainnet-via-fetcher.sh --epochs 3 --nix --profile

This works in two phases, prefetch and benchmark:

   Phase 1:  preload the prefetcher's ChainDB, up to desired limit.

   Phase 2:  run the second node as the benchmark, up to desired slot limit,
             while feeding it blocks from the preloaded local prefetcher,
             while also preventing said prefetcher from syncing mainnet further.

App options (going BEFORE common options OR IGNORED):

  Benchmark setup options:

    --skip-prefetch             skip the prefetch phase
    --prebench-pause            request the user to hit 'Enter' between phases
    --server-wait SECONDS       delay the benchmarked (client) side to start,
                                  for this long -- to avoid profile disturbance.
                                  Defaults to ${server_wait} seconds.
    --no-server-wait            same as --server-wait 0
    --node-stdout FILE          redirect stdout of benchmarked node to a file
    --node-no-stdout            redirect stdout of benchmarked node to /dev/null

  Benchmark schedule options:

    --node-config CONFIG        config to benchmark, per configuration/defaults.
                                  Default: 'mainnet-via-fetcher'
                                  'both' means two runs: default + 'mainnet-silent'

    --epochs / --slots          at which point to stop
    --repeats COUNT             repeat the chosen benchmark runs, default=1

  Common options (going AFTER common options or LATTER ARE IGNORED):

    --profile MODE              MODE profiling of the benchmarked node:
       time space space-module space-closure space-type space-retainer space-bio
    --nix / --cabal / --stack   pick your poison
    --help                      see for more common options

EOF
}

epoch_limit=3
mainnet_k=2160
mainnet_epoch_slots=$((10 * mainnet_k))
slot_limit=
skip_prefetch=
prebench_pause=
benchmark_configs=('mainnet-via-fetcher')
server_wait=$((epoch_limit*18+10))
repeats=1
redirect_stdout=
node_port=3002

while test -n "$1"
do case "$1" in
           --node-config )
                   case $2 in
                   'both' )    benchmark_configs=('mainnet-via-fetcher'
                                                  'mainnet-silent'); shift;;
                   * )         test -d "configuration/defaults/$2" ||
                                     error "--node-config $2 points to absent configuration";
                               benchmark_configs=("$2"); shift;;
                   esac;;
           --node-port )       node_port=$2; shift;;
           --node-stdout )     redirect_stdout="$2"; shift;;
           --node-no-stdout | --silent | --mute )
                               redirect_stdout='/dev/null';;
           ## The server node takes a busy while to validate its ChainDB.
           --server-wait )     server_wait=$2; shift;;
           --no-server-wait | --no-wait )
                               server_wait=0;;
           --epochs )          epoch_limit=$2; shift;;
           --slots )           slot_limit=$2; shift;;
           --prebench-pause )  prebench_pause=t;;
           --skip-prefetch )   skip_prefetch=t;;
           --skip-benchmark )  benchmark_configs=();;
           --repeats )         repeats=$2; shift;;
           --app-help )        app_usage; exit 1;;
           ## Remaining options handled in 'lib/common.sh'
           * )                 break;; esac; shift; done

. "$(dirname "$0")"/common.sh
. "$(dirname "$0")"/lib-cli.sh
. "$(dirname "$0")"/lib-node.sh

REMAINING_TOP_ARGS=("$@")

__LOCAL_SERVER_BASH_PID=
shutdown() {
        if test -n "${__LOCAL_SERVER_BASH_PID}"
        then local node_pid="$(pstree -Ap ${__LOCAL_SERVER_BASH_PID} |
                               head -n1 |
                               sed 's/^bash([0-9]*)---cardano-node(\([0-9]*\)).*$/\1/')"
             dprint "cleaning up server node:  bash ${__LOCAL_SERVER_BASH_PID} -> node pid ${node_pid}.."
             kill "${node_pid}" 2>/dev/null || true
             pstree -p "${__LOCAL_SERVER_BASH_PID}"
        fi
}
## Terminate children, whatever they may be..
trap shutdown EXIT

check_port_availability() {
        local port="$1"
        test -z "$(netstat -pltn 2>/dev/null | grep ":${port} ")"
}

phase_prefetch()
{
        oprint "prefetching up to ${limit_desc} worth of mainnet to local ChainDB.."
        local RUN_NODE_ARGS=(
                --no-profile
                --no-stats
                --config-name     'mainnet-silent'
                --topology-name   'byron-mainnet'
                --state           'mainnet'
                ## non-run_node args follow:
                "${REMAINING_TOP_ARGS[@]}"
                --shutdown-on-slot-synced "${slot_limit}"
        )
        if run_node_quiet "${RUN_NODE_ARGS[@]}"
        then oprint "local ChainDB preloaded with mainnet chain up to ${limit_desc}"
        else
                 echo -e '\n' >&2
                 pgrep -fal 'cardano-node'
                 echo -e '\nMainnet prefetch failed, perhaps some unexpected "cardano-node" processes are running (see above)?\nIf so, you may consider:  pkill cardano-node' >&2
                 exit 1
        fi
}

## Launch the server side of the local chainsync benchmark.
start_preloaded_server_node()
{
        local SERVER_RUN_NODE_ARGS=(
                --no-profile
                --no-stats
                --config-name     'mainnet-silent'
                --topology-name   'excommunicated'
                --state           'mainnet'
                --port            "${node_port}"
                "${REMAINING_TOP_ARGS[@]}"
        )
        oprint "starting local ChainSync server.."
        run_node_quiet "${SERVER_RUN_NODE_ARGS[@]}"
}

git_state() {
        echo "$(git symbolic-ref --short HEAD)" / "$(git rev-parse HEAD | cut -c-8)"
}

output_tags=()
print_benchmark_schedule() {
        oprint "benchmark schedule:  $(git_state), ${repeats} runs of configs:  ${benchmark_configs[*]}"
}

## Run a chainsync benchmark for:
##  1. the specified configuration
##  2. using the 'mainnet-via-fetcher' topology
benchmark_config()
{
        local benchmarked_config="$1"
        local iteration="$2"
        local profile_suffix="${slot_limit}slots.${benchmarked_config}.run${iteration}"

        rm -rf "db/${benchmarked_config}"

        BENCHMARKED_RUN_NODE_ARGS=(
                --config-name     "${benchmarked_config}"
                --topology-name   'mainnet-via-fetcher'
                --state           "${benchmarked_config}"
                --profile-suffix  "${profile_suffix}"
                ${redirect_stdout:+--redirect-stdout "${redirect_stdout}"}
                "${REMAINING_TOP_ARGS[@]}"
                ## non-run_node args follow:
                --shutdown-on-slot-synced "${slot_limit}"
        )

        oprint "starting local ChainSync benchmark:"
        oprint "   branch/commit:   $(git_state)"
        oprint "   work amount:     ${limit_desc}"
        oprint "   node config:     ${benchmarked_config}"
        oprint "   iteration #:     ${iteration}"
        oprint "   output tag:      ${profile_suffix}"
        output_tags+=("${profile_suffix}")
        run_node_quiet "${BENCHMARKED_RUN_NODE_ARGS[@]}"
}

###
### Main
###

check_port_availability "${node_port}" || {
        fprint "port ${node_port} busy"
        fprint "perhaps another 'cardano-node' is running?  netstat -pltn | grep ':${node_port} '"
        exit 1
}

if test -n "${REMAINING_TOP_ARGS[*]}"
then oprint "WARNING:"
     oprint "WARNING:  unhandled top-level args:  ${REMAINING_TOP_ARGS[*]}"
     oprint "WARNING:  they will be passed to run_node, which can break things"
     oprint "WARNING:                                   ^^^^^^^^^^^^^^^^^^^^^^"; fi

if test -z "${slot_limit}"
then slot_limit=$((epoch_limit * mainnet_epoch_slots))
     limit_desc=" epoch ${epoch_limit} (${slot_limit} slots)"
else limit_desc=${slot_limit}' slots'
fi

print_benchmark_schedule

#
#  Phase 1:  preload the prefetcher's ChainDB, up to desired limit.
#
if test -z "${skip_prefetch}"
then time -p phase_prefetch
else prebuild 'cardano-node' 'cardano-node'; fi

test -n "${prebench_pause}" &&
  read -rp "Mainnet prefetch complete. Press Enter to continue..." foo

start_preloaded_server_node &
__LOCAL_SERVER_BASH_PID=$!
dprint "primed cleanup for PID ${__LOCAL_SERVER_BASH_PID}"

sleep 0.3
if test "${server_wait}" != 0
then oprint "waiting for ${server_wait} seconds, to let the server process ChainDB.."
     sleep "${server_wait}"; fi

#
#  Phase 2:  run the second node as the benchmark, up to desired slot limit,
#            while feeding it blocks from the local prefetcher,
#            while also preventing said prefetcher from syncing mainnet further.
#
for i in $(seq 1 "${repeats}")
do for bench_conf in "${benchmark_configs[@]}"
   do time -p benchmark_config "${bench_conf}" "${i}"; done; done

print_benchmark_schedule
oprint "benchmark schedule complete"
if test -n "${output_tags[*]}"
then oprint "output tags:  ${output_tags[*]}"; fi
