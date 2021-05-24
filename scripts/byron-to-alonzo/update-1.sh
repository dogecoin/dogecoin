#!/usr/bin/env bash

set -e
# set -x

# This script will
# - move funds out of the Byron genesis address, so that we can use them later in Shelley
# - iniate the transition to protocol version 1 (Byron, OBFT)

ROOT=example

pushd ${ROOT}

export CARDANO_NODE_SOCKET_PATH=node-bft1/node.sock

# move funds out of Byron genesis
cardano-cli submit-tx \
            --testnet-magic 42 \
            --tx tx0.tx
cardano-cli submit-tx \
            --testnet-magic 42 \
            --tx tx1.tx

# submit update proposal
cardano-cli byron submit-update-proposal \
            --testnet-magic 42 \
            --filepath update-proposal
sleep 2

# vote on proposal
cardano-cli byron submit-proposal-vote  \
            --testnet-magic 42 \
            --filepath update-vote.000
cardano-cli byron submit-proposal-vote  \
            --testnet-magic 42 \
            --filepath update-vote.001

popd
