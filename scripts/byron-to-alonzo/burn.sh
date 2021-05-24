#!/usr/bin/env bash

set -e
# set -x

# This script creates, signs, and submits a transaction that burns the tokens
# that were created with mint.sh.

ROOT=example
COINS_IN_INPUT=1000000000
pushd ${ROOT}

export CARDANO_NODE_SOCKET_PATH=node-bft1/node.sock

KEYHASH=$(cardano-cli address key-hash --payment-verification-key-file ma/policy.vkey)

SCRIPT=ma/policy.script

TXID4=$(cardano-cli transaction txid --tx-body-file tx4.txbody)

POLICYID=$(cardano-cli transaction policyid --script-file ma/policy.script)

cardano-cli transaction build-raw \
            --mary-era \
            --fee 0 \
            --tx-in $TXID4#0 \
            --tx-out $(cat addresses/user1.addr)+$((${COINS_IN_INPUT} / 2)) \
            --mint="-5 $POLICYID.couttscoin" \
            --out-file tx5.txbody

cardano-cli transaction sign \
            --signing-key-file addresses/user1.skey \
            --signing-key-file ma/policy.skey \
            --script-file $SCRIPT \
            --testnet-magic 42 \
            --tx-body-file  tx5.txbody \
            --out-file      tx5.tx

cardano-cli transaction submit --tx-file  tx5.tx --testnet-magic 42



popd

