#!/usr/bin/env bash
# shellcheck disable=SC2154,SC2046

__COMMON_SRCROOT="$(realpath $(dirname "$0")/..)"
###
### INTERNAL PLUMBING, DO NOT RUN DIRECTLY
###

conf="$1"
topo="$2"
delegate="$3"

run_node_args() {
        local id=$1

        RUN_NODE_ARGS=(
          --state           "node-${id}"
          --port            "300${id}"
          --config-name     'custom'
        )

        if test "${delegate}" = 'indexed'
        then delegate=${id}
        fi
        if test -n "${delegate}"
        then RUN_NODE_ARGS+=(
          --delegate-id "${delegate}"
        ); fi

        if test "${topo}" = 'indexed'
        then RUN_NODE_ARGS+=(
          --topology-name   "custom"
          --topology "configuration/defaults/${conf}/topology-node-${id}.json"
             )
        else
             RUN_NODE_ARGS+=(
          --topology-name   "${topo}"
             )
        fi

        RUN_NODE_ARGS+=(
          --config "configuration/defaults/${conf}/config-${id}.yaml"
        )
        echo -n "${RUN_NODE_ARGS[@]}"
}

tmux split-window -h
tmux split-window -v

for i in 0 1 2
do tmux select-pane -t ${i}
   tmux send-keys \
     "__COMMON_SRCROOT=${__COMMON_SRCROOT};
      DEFAULT_DEBUG=${DEFAULT_DEBUG};
      DEFAULT_VERBOSE=${DEFAULT_VERBOSE};
      DEFAULT_TRACE=${DEFAULT_TRACE};

      . ${__COMMON_SRCROOT}/scripts/common.sh;
      . ${__COMMON_SRCROOT}/scripts/lib-cli.sh;
      . ${__COMMON_SRCROOT}/scripts/lib-node.sh;

      run_node $(run_node_args ${i})" \
     C-m
done
tmux select-pane -t 0
