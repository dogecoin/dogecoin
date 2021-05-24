#!/bin/sh

## Run this inside the `cardano-sl` repository:

nix-build -j auto --cores 0 -A mkGenesis --arg genesisArgs '{ systemStart = 1000000000; configurationKey = "mainnet_ci_full"; configurationKeyLaunch = "mainnet_ci"; }'
