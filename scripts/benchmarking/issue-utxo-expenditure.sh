#!/usr/bin/env bash
# shellcheck disable=SC1090,SC2154,SC2155,SC2034,SC2039,SC2240,SC2124

. "$(dirname "$0")"/common.sh
. "$(dirname "$0")"/lib-cli.sh

prebuild 'cardano-cli' 'cardano-cli'

setup_genesis_for_config 'liveview'

CONFIG='configuration/defaults/liveview/config-0.yaml'
default_from_key="${genesis_root}/delegate-keys.001.key"
default_to_key="${genesis_root}/delegate-keys.002.key"

## rob the bank
default_lovelace="862000000000000"

case $# in
        2 ) tx="$1"
            txin="$2"
            outindex="0"
            from_key="${default_from_key}"
            to_key="${default_to_key}"
            lovelace=${default_lovelace};;
        6 ) tx="$1"
            txin="$2"
            outindex="$3"
            from_key="$4"
            to_key="$5"
            lovelace="$6";;
        * ) cat >&2 <<EOF
Usage:  $(basename "$0") TX-FILE IN-TXID IN-INDEX FROM-KEY-FILE TO-KEY-FILE LOVELACE
EOF
            exit 1;; esac

proto_magic=$(jq '.protocolConsts | .protocolMagic' "${genesis_file}")
to_addr=$(get_default_key_address "${proto_magic}" "${to_key}")

issue_utxo_expenditure \
  "${CONFIG}" "${tx}" "${from_key}" "${txin}" "${outindex}" "${lovelace}" "${to_addr}"
