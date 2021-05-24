#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2154,SC2155,SC2034,SC2039,SC2240,SC2124

. "$(dirname "$0")"/common.sh
. "$(dirname "$0")"/lib-cli.sh

prebuild 'cardano-cli' 'cardano-cli'

setup_genesis_for_config 'liveview'

TX=${1?scripts.issue-genesis-utxo-expenditure.Error: no tx name specified}

CONFIG="configuration/defaults/liveview/config-0.yaml"
from_addr="2cWKMJemoBain3UWCzSY5wZvcf8uQ2MAaSy8hedrwpqsbYCm4QnBgPn3cEH7KF3X7DKoZ"
from_key="${configuration_root}/genesis/delegate-keys.000.key"
default_to_key="${configuration_root}/genesis/delegate-keys.001.key"

## ADA to be spent
default_lovelace="863000000000000"
case $# in
        1 ) proto_magic=$(jq '.protocolConsts | .protocolMagic' "${genesis_file}")
            to_addr=$(get_default_key_address "${proto_magic}" "${default_to_key}")
            lovelace=${default_lovelace};;
        3 ) to_addr="$2"
            lovelace="$3";;
        * ) cat >&2 <<EOF
Usage:  $(basename "$0") COMMON-OPTIONS.. TX-FILE [TO-ADDR] [LOVELACE]
EOF
            exit 1;; esac

issue_genesis_utxo_expenditure \
  "${CONFIG}" "${TX}" "${from_key}" "${from_addr}" "${lovelace}" "${to_addr}"
