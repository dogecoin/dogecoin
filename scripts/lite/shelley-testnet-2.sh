#!/usr/bin/env bash

set -e

: "${ROOT:=$(mktemp).d}"

echo "Working directory: $ROOT"

BFT_NODES="node-bft1 node-bft2"
BFT_NODES_N="1 2"
NUM_BFT_NODES=2

POOL_NODES="node-pool1"

ALL_NODES="${BFT_NODES} ${POOL_NODES}"

if ! mkdir "${ROOT}"; then
  echo "The ${ROOT} directory already exists, please move or remove it"
  exit
fi

cardano_cli="$(cabal exec -- which cardano-cli)"
cardano_node="$(cabal exec -- which cardano-node)"

# copy and tweak the configuration
cp configuration/defaults/byron-mainnet/configuration.yaml ${ROOT}/
sed -i ${ROOT}/configuration.yaml \
    -e 's/^Protocol: RealPBFT/Protocol: TPraos/' \
    -e 's/^minSeverity: Info/minSeverity: Debug/' \
    -e 's/^TraceBlockchainTime: False/TraceBlockchainTime: True/'

# Set up our template
$cardano_cli genesis create --testnet-magic 42 --genesis-dir ${ROOT}

# Then edit the genesis.spec.json ...

SUPPLY=1000000000

# We're going to use really quick epochs (300 seconds), by using short slots 0.2s
# and K=10, but we'll keep long KES periods so we don't have to bother
# cycling KES keys
sed -i ${ROOT}/genesis.spec.json \
    -e 's/"slotLength": 1/"slotLength": 0.2/' \
    -e 's/"activeSlotsCoeff": 5.0e-2/"activeSlotsCoeff": 0.1/' \
    -e 's/"securityParam": 2160/"securityParam": 10/' \
    -e 's/"epochLength": 432000/"epochLength": 1500/' \
    -e 's/"maxLovelaceSupply": 0/"maxLovelaceSupply": 1000000000/' \
    -e 's/"decentralisationParam": 1/"decentralisationParam": 0.7/'

# Now generate for real:

$cardano_cli genesis create \
    --testnet-magic 42 \
    --genesis-dir ${ROOT}/ \
    --gen-genesis-keys ${NUM_BFT_NODES} \
    --gen-utxo-keys 1

pushd ${ROOT}

echo "====================================================================="
echo "Generated genesis keys and genesis files:"
echo
ls -1 *
echo "====================================================================="
echo "Generated genesis.json:"
echo
cat genesis.json
echo
echo "====================================================================="

mkdir ${ALL_NODES}

# Make the pool operator cold keys
# This was done already for the BFT nodes as part of the genesis creation

for NODE in ${POOL_NODES}; do
  $cardano_cli node key-gen \
    --cold-verification-key-file                 ${NODE}/operator.vkey \
    --cold-signing-key-file                      ${NODE}/operator.skey \
    --operational-certificate-issue-counter-file ${NODE}/operator.counter

  $cardano_cli node key-gen-VRF \
    --verification-key-file ${NODE}/vrf.vkey \
    --signing-key-file      ${NODE}/vrf.skey

  # Set permissions for the vrf private key file: read for owner only
  chmod gou-rwx "${NODE}/vrf.skey"
  chmod u+r "${NODE}/vrf.skey"
done

# Symlink the BFT operator keys from the genesis delegates, for uniformity

for N in ${BFT_NODES_N}; do
  ln -s ../delegate-keys/delegate${N}.skey node-bft${N}/operator.skey
  ln -s ../delegate-keys/delegate${N}.vkey node-bft${N}/operator.vkey
  ln -s ../delegate-keys/delegate${N}.counter node-bft${N}/operator.counter
  ln -s ../delegate-keys/delegate${N}.vrf.vkey node-bft${N}/vrf.vkey
  ln -s ../delegate-keys/delegate${N}.vrf.skey node-bft${N}/vrf.skey
done

# Make hot keys and for all nodes
for NODE in ${ALL_NODES}; do

  $cardano_cli node key-gen-KES \
      --verification-key-file ${NODE}/kes.vkey \
      --signing-key-file      ${NODE}/kes.skey

  $cardano_cli node issue-op-cert \
      --kes-period 0 \
      --kes-verification-key-file                  ${NODE}/kes.vkey \
      --cold-signing-key-file                      ${NODE}/operator.skey \
      --operational-certificate-issue-counter-file ${NODE}/operator.counter \
      --out-file                                   ${NODE}/node.cert

done

# Make topology files
#TODO generalise this over the N BFT nodes and pool nodes
(cat <<TOPOLOGY_FILE
{
   "Producers": [
     {
       "addr": "127.0.0.1",
       "port": 3002,
       "valency": 1
     }
   , {
       "addr": "127.0.0.1",
       "port": 3003,
       "valency": 1
     }
   ]
 }
TOPOLOGY_FILE
) > node-bft1/topology.json
echo 3001 > node-bft1/port

(cat <<TOPOLOGY_FILE
{
   "Producers": [
     {
       "addr": "127.0.0.1",
       "port": 3001,
       "valency": 1
     }
   , {
       "addr": "127.0.0.1",
       "port": 3003,
       "valency": 1
     }
   ]
 }
TOPOLOGY_FILE
) > node-bft2/topology.json
echo 3002 > node-bft2/port

(cat <<TOPOLOGY_FILE
{
   "Producers": [
     {
       "addr": "127.0.0.1",
       "port": 3001,
       "valency": 1
     }
   , {
       "addr": "127.0.0.1",
       "port": 3002,
       "valency": 1
     }
   ]
 }
TOPOLOGY_FILE
) > node-pool1/topology.json
echo 3003 > node-pool1/port

echo "Generated node operator keys (cold, hot) and operational certs:"
echo
ls -1 ${ALL_NODES}
echo "====================================================================="

# Make some payment and stake addresses
# user1..n:       will own all the funds in the system, we'll set this up from
#                 initial utxo the
# pool-owner1..n: will be the owner of the pools and we'll use their reward
#                 account for pool rewards

USER_ADDRS="user1"
POOL_ADDRS="pool-owner1"

ADDRS="${USER_ADDRS} ${POOL_ADDRS}"

mkdir addresses

for ADDR in ${ADDRS}; do
  # Payment address keys
  $cardano_cli address key-gen \
      --verification-key-file addresses/${ADDR}.vkey \
      --signing-key-file      addresses/${ADDR}.skey

  # Stake address keys
  $cardano_cli stake-address key-gen \
      --verification-key-file addresses/${ADDR}-stake.vkey \
      --signing-key-file      addresses/${ADDR}-stake.skey

  # Payment addresses
  $cardano_cli address build \
      --payment-verification-key-file addresses/${ADDR}.vkey \
      --stake-verification-key-file addresses/${ADDR}-stake.vkey \
      --testnet-magic 42 \
      --out-file addresses/${ADDR}.addr

  # Stake addresses
  $cardano_cli stake-address build \
      --stake-verification-key-file addresses/${ADDR}-stake.vkey \
      --testnet-magic 42 \
      --out-file addresses/${ADDR}-stake.addr

  # Stake addresses registration certs
  $cardano_cli stake-address registration-certificate \
      --stake-verification-key-file addresses/${ADDR}-stake.vkey \
      --out-file addresses/${ADDR}-stake.reg.cert
done

# user N will delegate to pool N
USER_POOL_N="1"

for N in ${USER_POOL_N}; do
  # Stake address delegation certs
  $cardano_cli stake-address delegation-certificate \
      --stake-verification-key-file addresses/user${N}-stake.vkey \
      --cold-verification-key-file  node-pool${N}/operator.vkey \
      --out-file addresses/user${N}-stake.deleg.cert

  ln -s ../addresses/pool-owner${N}-stake.vkey node-pool${N}/owner.vkey
  ln -s ../addresses/pool-owner${N}-stake.skey node-pool${N}/owner.skey
done

echo "Generated payment address keys, stake address keys,"
echo "stake address regitration certs, and stake address delegatation certs"
echo
ls -1 addresses/
echo "====================================================================="

# Next is to make the stake pool registration cert

for NODE in ${POOL_NODES}; do
  $cardano_cli stake-pool registration-certificate \
    --testnet-magic 42 \
    --pool-pledge 0 --pool-cost 0 --pool-margin 0 \
    --cold-verification-key-file             ${NODE}/operator.vkey \
    --vrf-verification-key-file              ${NODE}/vrf.vkey \
    --reward-account-verification-key-file   ${NODE}/owner.vkey \
    --pool-owner-stake-verification-key-file ${NODE}/owner.vkey \
    --out-file                               ${NODE}/registration.cert
done

echo "Generated stake pool registration certs:"
ls -1 node-*/registration.cert
echo "====================================================================="

# Now we'll construct one whopper of a transaction that does everything
# just to show off that we can, and to make the script shorter

# We'll transfer all the funds to the user1, which delegates to pool1
# We'll register certs to:
#  1. register the pool-owner1 stake address
#  2. register the stake pool 1
#  3. register the user1 stake address
#  4. delegate from the user1 stake address to the stake pool

$cardano_cli transaction build-raw \
    --invalid-hereafter 1000 \
    --fee 0 \
    --tx-in $($cardano_cli genesis initial-txin \
                --testnet-magic 42 \
                --verification-key-file utxo-keys/utxo1.vkey) \
    --tx-out $(cat addresses/user1.addr)+${SUPPLY} \
    --certificate-file addresses/pool-owner1-stake.reg.cert \
    --certificate-file node-pool1/registration.cert \
    --certificate-file addresses/user1-stake.reg.cert \
    --certificate-file addresses/user1-stake.deleg.cert \
    --out-file tx1.txbody

# So we'll need to sign this with a bunch of keys:
# 1. the initial utxo spending key, for the funds
# 2. the user1 stake address key, due to the delegatation cert
# 3. the pool1 owner key, due to the pool registration cert
# 3. the pool1 operator key, due to the pool registration cert

$cardano_cli transaction sign \
    --signing-key-file utxo-keys/utxo1.skey \
    --signing-key-file addresses/user1-stake.skey \
    --signing-key-file node-pool1/owner.skey \
    --signing-key-file node-pool1/operator.skey \
    --testnet-magic 42 \
    --tx-body-file  tx1.txbody \
    --out-file      tx1.tx

echo "Generated a signed 'do it all' transaction:"
ls -1 tx1.tx
echo "====================================================================="

popd

i=0
for NODE in ${ALL_NODES}; do
  esc=$(printf '\033')
  node_tag="$(echo "\033[31m[node-$i]\e[0m")"
  i="$(($i + 1))"
  color="$((31+$i))"

  $cardano_node run \
    --config                          ${ROOT}/configuration.yaml \
    --topology                        ${ROOT}/${NODE}/topology.json \
    --database-path                   ${ROOT}/${NODE}/db \
    --socket-path                     ${ROOT}/${NODE}/node.sock \
    --shelley-kes-key                 ${ROOT}/${NODE}/kes.skey \
    --shelley-vrf-key                 ${ROOT}/${NODE}/vrf.skey \
    --shelley-operational-certificate ${ROOT}/${NODE}/node.cert \
    --port                            $(cat ${ROOT}/${NODE}/port) \
    | sed "s|^|${esc}[${color}m[node-$NODE]${esc}[0m |g" &
done

function cleanup()
{
  for child in $(jobs -p); do
    echo kill "$child" && kill "$child"
  done
}

cat

trap cleanup EXIT
