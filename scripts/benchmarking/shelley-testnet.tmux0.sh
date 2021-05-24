#!/usr/bin/env bash

###
### INTERNAL PLUMBING, DO NOT RUN DIRECTLY
###

dprint "tmux0: $*"
config="$1"
topology="$2"
delegate="$3"

tmux select-window -t :0
tmux new-window \
  -n 'Nodes' \
  "$(dirname "$0")/shelley-testnet.tmux1.sh '${config}' '${topology}' '${delegate}'; $SHELL"
sleep 1
