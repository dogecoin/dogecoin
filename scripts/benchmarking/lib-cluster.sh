#!/usr/bin/env bash
# shellcheck disable=SC2154

TMUX_CONFIG="$(mktemp -t --suffix=tmuXXXXXXXX)"
run_3node_cluster() {
        command -v tmux >/dev/null 2>&1 || { echo >&2 "ERROR: This script requires tmux, which is missing. Please install it."; exit 1; }
        dprint "run_3node_cluster: $*"
        local config="$1" topo="${2:-indexed}" delegate="${3:-indexed}"

        setup_genesis_for_config "${config}"
        prebuild 'cardano-node' 'cardano-node'

        tmux \
          -f "${TMUX_CONFIG}" \
          new-session \
          -E \
          -s "3node-cluster-${config}" \
          -n 'Main' \
          "${scripts}/shelley-testnet.tmux0.sh '${config}' '${topo}' '${delegate}'"
}
