#!/usr/bin/env bash

ROOT="$(realpath "$(dirname "$0")/../..")"

configuration="${ROOT}/configuration"

OS=$(uname -s) DATE=
case $OS in
  Darwin )       DATE="gdate";;
  * )            DATE="date";;
esac

data_dir="$(mktemp).d"
mkdir -p "${data_dir}"

# Generate genesis keys
ARGS=(
  --genesis-output-dir           "${data_dir}/genesis"
  --start-time                   "$(${DATE} -d "now + 1 minute" +%s)"
  --protocol-parameters-file     "${ROOT}/scripts/protocol-params.json"
  --k                            2160
  --protocol-magic               459045235
  --n-poor-addresses             128
  --n-delegate-addresses         7
  --total-balance                8000000000000000
  --avvm-entry-count             128
  --avvm-entry-balance           10000000000000
  --delegate-share               0.9
  --real-pbft
  --secret-seed                  2718281828
)
cabal run exe:cardano-cli -- byron genesis genesis "${ARGS[@]}"

# Compute genesis hash
cabal run exe:cardano-cli -- byron genesis print-genesis-hash --genesis-json "${data_dir}/genesis/genesis.json" | tail -1 > "${data_dir}"/genesis/GENHASH

# Ensure the node is built
cabal run --no-stats cardano-node cardano-node --help >/dev/null || true

for i in 0 1 2; do
  # Use copy default configuration and topolgy to configuration directory for a particular node instance
  cp -af "${configuration}/defaults/simpleview/config-$i.yaml" "${data_dir}"
  cp -af "${configuration}/defaults/simpleview/topology-node-$i.json" "${data_dir}"
  db_dir="${data_dir}/db/node-$i"
  socket_dir="${data_dir}/socket"

  mkdir -p "${db_dir}"
  mkdir -p "${socket_dir}"

  # Launch a node instead for the testnet
  cabal run exe:cardano-node -- run \
    --database-path "${db_dir}" \
    --socket-path "${socket_dir}/node-$i-socket" \
    --port "300$i" \
    --config "${data_dir}/config-$i.yaml" \
    --topology "${data_dir}/topology-node-$i.json" \
    --signing-key "${data_dir}/genesis/delegate-keys.00$i.key" \
    --delegation-certificate "${data_dir}/genesis/delegation-cert.00$i.json" &
done

function cleanup()
{
  for child in $(jobs -p); do
    echo kill "$child" && kill "$child"
  done
}

cat

trap cleanup EXIT