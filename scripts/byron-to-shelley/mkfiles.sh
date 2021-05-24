#!/usr/bin/env bash

set -e
#set -x

# This script sets up a cluster that starts out in Byron, and can transition to Shelley.
#
# The script generates all the files needed for the setup, and prints commands
# to be run manually (to start the nodes, post transactions, etc.).
#
# There are three ways of triggering the transition to Shelley:
# 1. Trigger transition at protocol version 2.0.0 (as on mainnet)
#    The system starts at 0.0.0, and we can only increase it by 1 in the major
#    version, so this does require to
#    a) post an update proposal and votes to transition to 1.0.0
#    b) wait for the protocol to change (end of the epoch, or end of the last
#      epoch if it's posted near the end of the epoch)
#    c) change configuration.yaml to have 'LastKnownBlockVersion-Major: 2',
#      and restart the nodes
#    d) post an update proposal and votes to transition to 2.0.0
#    This is what will happen on the mainnet, so it's vital to test this, but
#    it does contain some manual steps.
# 2. Trigger transition at protocol version 2.0.0
#    For testing purposes, we can also modify the system to do the transition to
#    Shelley at protocol version 1.0.0, by uncommenting the line containing
#    'TestShelleyHardForkAtVersion' below. Then, we just need to execute step a)
#    above in order to trigger the transition.
#    This is still close to the procedure on the mainnet, and requires less
#    manual steps.
# 3. Schedule transition in the configuration
#    To do this, uncomment the line containing 'TestShelleyHardForkAtEpoch'
#    below. It's good for a quick test, and does not rely on posting update
#    proposals to the chain.
#    This is quite convenient, but it does not test that we can do the
#    transition by posting update proposals to the network.
#
# TODO: The script allows transitioning to Shelley, but not yet to register a
# pool and delegate, so all blocks will still be produced by the BFT nodes.
# We will need CLI support for Byron witnesses in Shelley transactions to do
# that.

ROOT=example

BFT_NODES="node-bft1 node-bft2"
BFT_NODES_N="1 2"
NUM_BFT_NODES=2

POOL_NODES="node-pool1"

ALL_NODES="${BFT_NODES} ${POOL_NODES}"

NUM_UTXO_KEYS=1
MAX_SUPPLY=1000000000
INIT_SUPPLY=1000000000
FUNDS_PER_GENESIS_ADDRESS=$((${INIT_SUPPLY} / ${NUM_BFT_NODES}))
FUNDS_PER_BYRON_ADDRESS=$((${FUNDS_PER_GENESIS_ADDRESS} * 9 / 10))
# We need to allow for a fee to transfer the funds out of the genesis.

NETWORK_MAGIC=42
SECURITY_PARAM=10

OS=$(uname -s) DATE=
case $OS in
  Darwin )       DATE="gdate";;
  * )            DATE="date";;
esac

START_TIME="$(${DATE} -d "now + 30 seconds" +%s)"

if ! mkdir "${ROOT}"; then
  echo "The ${ROOT} directory already exists, please move or remove it"
  exit
fi

# copy and tweak the configuration
cp configuration/defaults/byron-mainnet/configuration.yaml ${ROOT}/
sed -i ${ROOT}/configuration.yaml \
    -e 's/Protocol: RealPBFT/Protocol: Cardano/' \
    -e '/Protocol/ aPBftSignatureThreshold: 0.6' \
    -e 's/minSeverity: Info/minSeverity: Debug/' \
    -e 's|GenesisFile: genesis.json|ByronGenesisFile: byron/genesis.json|' \
    -e '/ByronGenesisFile/ aShelleyGenesisFile: shelley/genesis.json' \
    -e 's/RequiresNoMagic/RequiresMagic/' \
    -e 's/LastKnownBlockVersion-Major: 0/LastKnownBlockVersion-Major: 1/' \
    -e 's/LastKnownBlockVersion-Minor: 2/LastKnownBlockVersion-Minor: 0/'
# Options for making it easier to trigger the transition to Shelley
# If neither of those are used, we have to
# - post an update proposal + votes to go to protocol version 1
# - after that's activated, change the configuration to have
#   'LastKnownBlockVersion-Major: 2', and restart the nodes
# - post another proposal + vote to go to protocol version 2

#uncomment this for an automatic transition after the first epoch
echo "TestShelleyHardForkAtEpoch: 1" >> ${ROOT}/configuration.yaml
#uncomment this to trigger the hardfork with protocol version 1
#echo "TestShelleyHardForkAtVersion: 1"  >> ${ROOT}/configuration.yaml


pushd ${ROOT}

# create the node directories
for NODE in ${ALL_NODES}; do

  mkdir ${NODE} ${NODE}/byron ${NODE}/shelley

done

# Make topology files
#TODO generalise this over the N BFT nodes and pool nodes
cat > node-bft1/topology.json <<EOF
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
EOF
echo 3001 > node-bft1/port

cat > node-bft2/topology.json <<EOF
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
EOF
echo 3002 > node-bft2/port

cat > node-pool1/topology.json <<EOF
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
EOF
echo 3003 > node-pool1/port


cat > byron.genesis.spec.json <<EOF
{
  "heavyDelThd":     "300000000000",
  "maxBlockSize":    "2000000",
  "maxTxSize":       "4096",
  "maxHeaderSize":   "2000000",
  "maxProposalSize": "700",
  "mpcThd": "20000000000000",
  "scriptVersion": 0,
  "slotDuration": "2000",
  "softforkRule": {
    "initThd": "900000000000000",
    "minThd": "600000000000000",
    "thdDecrement": "50000000000000"
  },
  "txFeePolicy": {
    "multiplier": "43946000000",
    "summand": "155381000000000"
  },
  "unlockStakeEpoch": "18446744073709551615",
  "updateImplicit": "10000",
  "updateProposalThd": "100000000000000",
  "updateVoteThd": "1000000000000"
}
EOF

cardano-cli byron genesis genesis \
  --protocol-magic ${NETWORK_MAGIC} \
  --start-time ${START_TIME} \
  --k ${SECURITY_PARAM} \
  --n-poor-addresses 0 \
  --n-delegate-addresses ${NUM_BFT_NODES} \
  --total-balance ${INIT_SUPPLY} \
  --delegate-share 1 \
  --avvm-entry-count 0 \
  --avvm-entry-balance 0 \
  --protocol-parameters-file byron.genesis.spec.json \
  --genesis-output-dir byron
mv byron.genesis.spec.json byron/genesis.spec.json


# Symlink the BFT operator keys from the genesis delegates, for uniformity
for N in ${BFT_NODES_N}; do

  ln -s ../../byron/delegate-keys.00$((${N} - 1)).key     node-bft${N}/byron/delegate.key
  ln -s ../../byron/delegation-cert.00$((${N} - 1)).json  node-bft${N}/byron/delegate.cert

done

# Create keys and addresses to withdraw the initial UTxO into
for N in ${BFT_NODES_N}; do

  cardano-cli keygen \
    --secret byron/payment-keys.00$((${N} - 1)).key \

  cardano-cli signing-key-address \
    --testnet-magic 42 \
    --secret byron/payment-keys.00$((${N} - 1)).key > byron/address-00$((${N} - 1))

  # Write Genesis addresses to files

  cardano-cli signing-key-address \
    --testnet-magic 42 \
    --secret byron/genesis-keys.00$((${N} - 1)).key > byron/genesis-address-00$((${N} - 1))

done

# Create Byron address that moves funds out of the genesis UTxO into a regular
# address.

cardano-cli issue-genesis-utxo-expenditure \
            --genesis-json byron/genesis.json \
            --testnet-magic 42 \
            --tx tx0.tx \
            --wallet-key byron/delegate-keys.000.key \
            --rich-addr-from $(head -n 1 byron/genesis-address-000) \
            --txout "(\"$(head -n 1 byron/address-000)\", $FUNDS_PER_BYRON_ADDRESS)"

# Update Proposal and votes
cardano-cli byron governance create-update-proposal \
            --filepath update-proposal \
            --testnet-magic 42 \
            --signing-key byron/delegate-keys.000.key \
            --protocol-version-major 1 \
            --protocol-version-minor 0 \
            --protocol-version-alt 0 \
            --application-name "cardano-sl" \
            --software-version-num 1 \
            --system-tag "linux" \
            --installer-hash 0

for N in ${BFT_NODES_N}; do
    cardano-cli byron governance create-proposal-vote \
                --proposal-filepath update-proposal \
                --testnet-magic 42 \
                --signing-key byron/delegate-keys.00$((${N} - 1)).key \
                --vote-yes \
                --output-filepath update-vote.00$((${N} - 1))
done

cardano-cli byron governance create-update-proposal \
            --filepath update-proposal-1 \
            --testnet-magic 42 \
            --signing-key byron/delegate-keys.000.key \
            --protocol-version-major 2 \
            --protocol-version-minor 0 \
            --protocol-version-alt 0 \
            --application-name "cardano-sl" \
            --software-version-num 1 \
            --system-tag "linux" \
            --installer-hash 0

for N in ${BFT_NODES_N}; do
    cardano-cli byron governance create-proposal-vote \
                --proposal-filepath update-proposal-1 \
                --testnet-magic 42 \
                --signing-key byron/delegate-keys.00$((${N} - 1)).key \
                --vote-yes \
                --output-filepath update-vote-1.00$((${N} - 1))
done

echo "====================================================================="
echo "Generated genesis keys and genesis files:"
echo
ls -1 byron/*
echo "====================================================================="


# Set up our template
mkdir shelley
cardano-cli genesis create --testnet-magic 42 --genesis-dir shelley

# Then edit the genesis.spec.json ...

# We're going to use really quick epochs (300 seconds), by using short slots 0.2s
# and K=10, but we'll keep long KES periods so we don't have to bother
# cycling KES keys
sed -i shelley/genesis.spec.json \
    -e 's/"slotLength": 1/"slotLength": 0.2/' \
    -e 's/"activeSlotsCoeff": 5.0e-2/"activeSlotsCoeff": 0.1/' \
    -e 's/"securityParam": 2160/"securityParam": 10/' \
    -e 's/"epochLength": 432000/"epochLength": 1500/' \
    -e 's/"maxLovelaceSupply": 0/"maxLovelaceSupply": 1000000000/' \
    -e 's/"decentralisationParam": 1/"decentralisationParam": 0.7/'

# Now generate for real:

cardano-cli genesis create \
    --testnet-magic 42 \
    --genesis-dir shelley/ \
    --gen-genesis-keys ${NUM_BFT_NODES} \
    --gen-utxo-keys 1

echo "====================================================================="
echo "Generated genesis keys and genesis files:"
echo
ls -1 shelley/*
echo "====================================================================="

echo "Generated genesis.json:"
echo
cat shelley/genesis.json
echo
echo "====================================================================="

echo "To start the nodes, in separate terminals use:"
echo
for NODE in ${ALL_NODES}; do

  echo "cardano-node run \\"
  echo "  --config                          ${ROOT}/configuration.yaml \\"
  echo "  --topology                        ${ROOT}/${NODE}/topology.json \\"
  echo "  --database-path                   ${ROOT}/${NODE}/db \\"
  echo "  --socket-path                     ${ROOT}/${NODE}/node.sock \\"
if [ -f ${NODE}/byron/delegate.key ]; then
  echo "  --signing-key                     ${ROOT}/${NODE}/byron/delegate.key \\"
  echo "  --delegation-certificate          ${ROOT}/${NODE}/byron/delegate.cert \\"
fi
  echo "  --shelley-kes-key                 ${ROOT}/${NODE}/shelley/kes.skey \\"
  echo "  --shelley-vrf-key                 ${ROOT}/${NODE}/shelley/vrf.skey \\"
  echo "  --shelley-operational-certificate ${ROOT}/${NODE}/shelley/node.cert \\"
  echo "  --port                            $(cat ${NODE}/port)"

done

# Make the pool operator cold keys
# This was done already for the BFT nodes as part of the genesis creation

for NODE in ${POOL_NODES}; do

  cardano-cli node key-gen \
      --cold-verification-key-file                 ${NODE}/shelley/operator.vkey \
      --cold-signing-key-file                      ${NODE}/shelley/operator.skey \
      --operational-certificate-issue-counter-file ${NODE}/shelley/operator.counter

  cardano-cli node key-gen-VRF \
      --verification-key-file ${NODE}/shelley/vrf.vkey \
      --signing-key-file      ${NODE}/shelley/vrf.skey

done

# Symlink the BFT operator keys from the genesis delegates, for uniformity

for N in ${BFT_NODES_N}; do

  ln -s ../../shelley/delegate-keys/delegate${N}.skey node-bft${N}/shelley/operator.skey
  ln -s ../../shelley/delegate-keys/delegate${N}.vkey node-bft${N}/shelley/operator.vkey
  ln -s ../../shelley/delegate-keys/delegate${N}.counter node-bft${N}/shelley/operator.counter
  ln -s ../../shelley/delegate-keys/delegate${N}.vrf.vkey node-bft${N}/shelley/vrf.vkey
  ln -s ../../shelley/delegate-keys/delegate${N}.vrf.skey node-bft${N}/shelley/vrf.skey

done


# Make hot keys and for all nodes

for NODE in ${ALL_NODES}; do

  cardano-cli node key-gen-KES \
      --verification-key-file ${NODE}/shelley/kes.vkey \
      --signing-key-file      ${NODE}/shelley/kes.skey

  cardano-cli node issue-op-cert \
      --kes-period 0 \
      --kes-verification-key-file                  ${NODE}/shelley/kes.vkey \
      --cold-signing-key-file                      ${NODE}/shelley/operator.skey \
      --operational-certificate-issue-counter-file ${NODE}/shelley/operator.counter \
      --out-file                                   ${NODE}/shelley/node.cert

done

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
  cardano-cli address key-gen \
      --verification-key-file addresses/${ADDR}.vkey \
      --signing-key-file      addresses/${ADDR}.skey

  # Stake address keys
  cardano-cli stake-address key-gen \
      --verification-key-file addresses/${ADDR}-stake.vkey \
      --signing-key-file      addresses/${ADDR}-stake.skey

  # Payment addresses
  cardano-cli address build \
      --payment-verification-key-file addresses/${ADDR}.vkey \
      --stake-verification-key-file addresses/${ADDR}-stake.vkey \
      --testnet-magic 42 \
      --out-file addresses/${ADDR}.addr

  # Stake addresses
  cardano-cli stake-address build \
      --stake-verification-key-file addresses/${ADDR}-stake.vkey \
      --testnet-magic 42 \
      --out-file addresses/${ADDR}-stake.addr

  # Stake addresses registration certs
  cardano-cli stake-address registration-certificate \
      --stake-verification-key-file addresses/${ADDR}-stake.vkey \
      --out-file addresses/${ADDR}-stake.reg.cert

done

# user N will delegate to pool N
USER_POOL_N="1"

for N in ${USER_POOL_N}; do

  # Stake address delegation certs
  cardano-cli stake-address delegation-certificate \
      --stake-verification-key-file addresses/user${N}-stake.vkey \
      --cold-verification-key-file  node-pool${N}/shelley/operator.vkey \
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

  cardano-cli stake-pool registration-certificate \
    --testnet-magic 42 \
    --pool-pledge 0 --pool-cost 0 --pool-margin 0 \
    --cold-verification-key-file             ${NODE}/shelley/operator.vkey \
    --vrf-verification-key-file              ${NODE}/shelley/vrf.vkey \
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

cardano-cli transaction build-raw \
    --invalid-hereafter 1000 \
    --fee 0 \
    --tx-in $(cardano-cli genesis initial-txin \
                --testnet-magic 42 \
                --verification-key-file shelley/utxo-keys/utxo1.vkey) \
    --tx-out $(cat addresses/user1.addr)+${MAX_SUPPLY} \
    --certificate-file addresses/pool-owner1-stake.reg.cert \
    --certificate-file node-pool1/registration.cert \
    --certificate-file addresses/user1-stake.reg.cert \
    --certificate-file addresses/user1-stake.deleg.cert \
    --out-file tx1.txbody


# TODO: this will become the transaction to register the pool, etc. We'll need to pick the tx-in from the actual UTxO -- since it contains the txid, we'll have to query this via cardano-cli query utxo.
# cardano-cli transaction build-raw --invalid-hereafter 1000000 --fee 0 --tx-in 67209bfcdf78f8cd86f649da75053a80fb9bb3fad68465554f9301c31b496c65#0 --tx-out $(cat example/addresses/user1.addr)+450000000 --certificate-file example/addresses/pool-owner1-stake.reg.cert     --certificate-file example/node-pool1/registration.cert     --certificate-file example/addresses/user1-stake.reg.cert     --certificate-file example/addresses/user1-stake.deleg.cert --out-file example/register-pool.txbody

# cardano-cli address convert --byron-key-file example/byron/payment-keys.000.key --signing-key-file example/byron/payment-keys.000-converted.key

# cardano-cli transaction sign --tx-body-file example/register-pool.txbody --testnet-magic 42 --signing-key-file example/byron/payment-keys.000-converted.key --signing-key-file example/shelley/utxo-keys/utxo1.skey --signing-key-file example/addresses/user1-stake.skey --signing-key-file example/node-pool1/owner.skey --signing-key-file example/node-pool1/shelley/operator.skey --out-file example/register-pool.tx

# cardano-cli transaction submit --tx-file example/register-pool.tx --testnet-magic 42

# So we'll need to sign this with a bunch of keys:
# 1. the initial utxo spending key, for the funds
# 2. the user1 stake address key, due to the delegatation cert
# 3. the pool1 owner key, due to the pool registration cert
# 3. the pool1 operator key, due to the pool registration cert

cardano-cli transaction sign \
    --signing-key-file shelley/utxo-keys/utxo1.skey \
    --signing-key-file addresses/user1-stake.skey \
    --signing-key-file node-pool1/owner.skey \
    --signing-key-file node-pool1/shelley/operator.skey \
    --testnet-magic 42 \
    --tx-body-file  tx1.txbody \
    --out-file      tx1.tx

echo "Generated a signed 'do it all' transaction:"
ls -1 tx1.tx
echo "====================================================================="

echo "So you can now do various things:"
echo " * Start the nodes"
echo " * Submit the initial 'do it all' transaction"
echo " * Query the node's ledger state"
echo
echo "To start the nodes, in separate terminals use:"
echo
for NODE in ${BFT_NODES}; do

  echo "cardano-node run \\"
  echo "  --config                          ${ROOT}/configuration.yaml \\"
  echo "  --topology                        ${ROOT}/${NODE}/topology.json \\"
  echo "  --database-path                   ${ROOT}/${NODE}/db \\"
  echo "  --socket-path                     ${ROOT}/${NODE}/node.sock \\"
  echo "  --shelley-kes-key                 ${ROOT}/${NODE}/shelley/kes.skey \\"
  echo "  --shelley-vrf-key                 ${ROOT}/${NODE}/shelley/vrf.skey \\"
  echo "  --shelley-operational-certificate ${ROOT}/${NODE}/shelley/node.cert \\"
  echo "  --port                            $(cat ${NODE}/port) \\"
  echo "  --delegation-certificate          ${ROOT}/${NODE}/byron/delegate.cert \\"
  echo "  --signing-key                     ${ROOT}/${NODE}/byron/delegate.key \\"
  echo "  | tee ${NODE}.log"

done
for NODE in ${POOL_NODES}; do

  echo "cardano-node run \\"
  echo "  --config                          ${ROOT}/configuration.yaml \\"
  echo "  --topology                        ${ROOT}/${NODE}/topology.json \\"
  echo "  --database-path                   ${ROOT}/${NODE}/db \\"
  echo "  --socket-path                     ${ROOT}/${NODE}/node.sock \\"
  echo "  --shelley-kes-key                 ${ROOT}/${NODE}/shelley/kes.skey \\"
  echo "  --shelley-vrf-key                 ${ROOT}/${NODE}/shelley/vrf.skey \\"
  echo "  --shelley-operational-certificate ${ROOT}/${NODE}/shelley/node.cert \\"
  echo "  --port                            $(cat ${NODE}/port) \\"
  echo "  | tee ${NODE}.log"

done
echo "To transfer funds out of the genesis UTxO to a regular address"
echo
echo "CARDANO_NODE_SOCKET_PATH=${ROOT}/node-bft1/node.sock \\"
echo "  cardano-cli submit-tx \\"
echo "    --testnet-magic 42 \\"
echo "    --tx ${ROOT}/tx0.tx"
echo
echo "To submit the update proposal for the transition to version 1"
echo
echo "CARDANO_NODE_SOCKET_PATH=${ROOT}/node-bft1/node.sock \\"
echo "  cardano-cli byron submit-update-proposal --testnet-magic 42 --filepath ${ROOT}/update-proposal"
echo
echo "To submit votes on the update proposal"
echo
for N in ${BFT_NODES_N}; do
    echo "CARDANO_NODE_SOCKET_PATH=${ROOT}/node-bft1/node.sock \\"
    echo "cardano-cli byron submit-proposal-vote  --testnet-magic 42 --filepath example/update-vote.00$((${N} - 1))"
done
echo
echo "To submit the update proposal for the transition to version 2"
echo "Make sure to wait until the protocol version has been set to 1.0.0"
echo "Also, for this to be  endorsed, you will need to set "
echo "'LastKnownBlockVersion-Major: 2' in configuration.yaml, and restart the nodes."
echo
echo "CARDANO_NODE_SOCKET_PATH=${ROOT}/node-bft1/node.sock \\"
echo "  cardano-cli byron submit-update-proposal --testnet-magic 42 --filepath ${ROOT}/update-proposal-1"
echo
echo "To submit votes on the update proposal"
echo
for N in ${BFT_NODES_N}; do
    echo "CARDANO_NODE_SOCKET_PATH=${ROOT}/node-bft1/node.sock \\"
    echo "cardano-cli byron submit-proposal-vote  --testnet-magic 42 --filepath example/update-vote-1.00$((${N} - 1))"
done
echo
echo "To submit the transaction that registers the pool, in the Shelley era"
echo
echo "CARDANO_NODE_SOCKET_PATH=${ROOT}/node-bft1/node.sock \\"
echo "  cardano-cli transaction submit \\"
echo "    --tx-file ${ROOT}/tx1.tx \\"
echo "    --testnet-magic 42"
echo
echo "Then wait until epoch #2 (counting from 0) starting at slot 3000"
echo "and query the stake distribution, and see if the pool node creates blocks"
echo
echo "CARDANO_NODE_SOCKET_PATH=example/node-bft1/node.sock \\"
echo "  cardano-cli query stake-distribution --testnet-magic 42"
echo

popd
