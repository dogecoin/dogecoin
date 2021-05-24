#!/usr/bin/env bash

# This script connects a node to mainnet

ROOT="$(realpath "$(dirname "$0")/../..")"
configuration="${ROOT}/configuration/cardano"

data_dir=mainnetsingle
mkdir -p "${data_dir}"
db_dir="${data_dir}/db/node"
mkdir -p "${db_dir}"
socket_dir="${data_dir}/socket"
mkdir -p "${socket_dir}"

# Launch a node
cabal run exe:cardano-node -- run \
  --config "${configuration}/mainnet-config.json" \
  --topology "${configuration}/mainnet-topology.json" \
  --database-path "${db_dir}" \
  --socket-path "${socket_dir}/node-1-socket" \
  --host-addr "127.0.0.1" \
  --port "3001"



function cleanup()
{
  for child in $(jobs -p); do
    echo kill "$child" && kill "$child"
  done
}

trap cleanup EXIT
