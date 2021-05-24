#!/usr/bin/env bash

set -e
# set -x

# This script creates, signs, and submits a transaction that creates some new tokens.
# It uses the output of the transaction from update-4.sh.

ROOT=example
COINS_IN_INPUT=1000000000
pushd ${ROOT}

export CARDANO_NODE_SOCKET_PATH=node-bft1/node.sock

mkdir -p ma
cardano-cli address key-gen \
            --verification-key-file ma/policy.vkey \
            --signing-key-file ma/policy.skey

KEYHASH=$(cardano-cli address key-hash --payment-verification-key-file ma/policy.vkey)

SCRIPT=ma/policy.script
rm -f $SCRIPT
echo "{" >> $SCRIPT
echo "  \"keyHash\": \"${KEYHASH}\"," >> $SCRIPT
echo "  \"type\": \"sig\"" >> $SCRIPT
echo "}" >> $SCRIPT

TXID3=$(cardano-cli transaction txid --tx-body-file tx3.txbody)

POLICYID=$(cardano-cli transaction policyid --script-file ma/policy.script)

cardano-cli transaction build-raw \
            --mary-era \
            --fee 0 \
            --tx-in $TXID3#0 \
            --tx-out="$(cat addresses/user1.addr)+$((${COINS_IN_INPUT} / 2))+5 $POLICYID.couttscoin" \
            --mint="5 $POLICYID.couttscoin" \
            --out-file tx4.txbody

cardano-cli transaction sign \
            --signing-key-file addresses/user1.skey \
            --signing-key-file ma/policy.skey \
            --script-file $SCRIPT \
            --testnet-magic 42 \
            --tx-body-file  tx4.txbody \
            --out-file      tx4.tx

cardano-cli transaction submit --tx-file  tx4.tx --testnet-magic 42



popd

