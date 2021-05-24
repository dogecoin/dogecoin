#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2034

DEFAULT_VERBOSE=t
. "$(dirname "$0")"/common.sh
. "$(dirname "$0")"/lib-cli.sh
. "$(dirname "$0")"/lib-node.sh
. "$(dirname "$0")"/lib-cluster.sh

# requires a local dns server which resolves
# * local6.iohk.io to ::1
# * local.iohk.io  to 127.0.0.1
# You can use `unbound` with this configuration put in `/etc/unbound/unbound.conf`
# ```
# server:
#   verbosity: 1
#   local-data: "local.iohk.io A 127.0.0.1"
#   local-data: "local6.iohk.io AAAA ::1"
# ```

run_3node_cluster 'simpleview' 'simple-dns'
