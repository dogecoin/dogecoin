#!/usr/bin/env bash
# shellcheck disable=SC1090

app_usage() {
        cat <<EOF

Run this like:

   scripts/mainnet.sh --nix --profile time --shutdown-on-slot-synced 21600

This syncs the mainnet chain.

  Common options (going BEFORE app options or LATTER ARE IGNORED):

    --profile MODE        enable profiling of the benchmarked node, where MODE is:
       time space space-module space-closure space-type space-retainer space-bio 
    --nix / --cabal / --stack  
                          pick your poison
    --help                see for more common options

  App options (going AFTER common options OR IGNORED):

    --shutdown-on-slot-synced SLOT
                          shut the node down on reaching given mainnet slot

    ..or any other 'cardano-node' option.

EOF
}

. "$(dirname "$0")"/common.sh
. "$(dirname "$0")"/lib-cli.sh
. "$(dirname "$0")"/lib-node.sh

while test -n "$1"
do case "$1" in
           --app-help ) app_usage; exit 1;;
           * ) break;; esac; shift; done

run_node --config-name 'byron-mainnet' \
  "$@"
