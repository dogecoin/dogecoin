#!/usr/bin/env bash
# shellcheck disable=SC1090

. "$(dirname "$0")"/common.sh
. "$(dirname "$0")"/lib-cli.sh

setup_genesis_for_config 'liveview'

test -z "$1" -o ! -f "$1" -o ! -r "$1" && {
        cat >&1 <<EOF
Usage:  $(basename "$0") COMMON-OPTIONS.. TX-FILE
EOF
        exit 1
}

TX="$1"
shift

ARGS=(
        submit-tx
        --tx           "$TX"
        --config       "${configuration_root}/config-0.yaml"
        --socket-path  "socket/node-0-socket"
)
run cardano-cli cardano-cli "${ARGS[@]}" "$@"
