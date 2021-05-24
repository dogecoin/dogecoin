# Benchmarking Scripts Overview

 This document describes development scripts in this directory.

## General notes on building the binaries

  Scripts in this directory share a common property -- they rely on the
  Haskell binaries to function -- and those binaries have to be built somehow,
  before the scripts can be used.

  There are different ways of how these executables can be built, as there
  are different workflows & audiences to serve:

  1. using Nix -- the most reliable way (as CI ensures the buildability),
     but not necessarily provides fastest iteration (although it benefits
     from the IOHK binary cache for the dependencies).
     This can be used by passing `--nix` to the scripts.  It's also the default,
     if the `dist-newstyle` directory is absent.

     Before you can use `--nix`, you'll need to set up Nix -- please see
     the `Building under Nix` section [here](https://github.com/input-output-hk/cardano-node/blob/master/doc/building-running.md#building-under-nix)

  2. using Cabal -- if you already have the development context set up
     (dependencies, GHC, etc.), then this is the fastest way to iterate.
     Pass `--cabal` to the scripts, or just create the `dist-newstyle` directory
     -- which serves as a cue, that changes the default.

  3. using Cabal and Nix, the best of both worlds (CI-cached deps & fast rebuilds):

       1. Ensure that `cabal.project` is in its pristine state.
       2. `nix-shell --cores 0 --max-jobs 4`
       3. Once the shell is ready, remove all `source-repository-package`
          entries from `cabal.project`.
       4. Pass `--cabal` to the scripts.

## Features

  Profiling of `cardano-node` (or any executable that is run using the
  `scripts/lib.sh` infrastructure) can be enabled by passing `--profile TYPE`
  to any script that runs the node, where `TYPE` is one of:
  `time` `space` `space-module` `space-closure` `space-type` `space-retainer` `space-bio`.

  Note that only the `--nix` mode benefits from the automatically-supplied
  profiling builds -- the `--cabal` users have to supply properly profiled builds
  on their own.

  The profiling & RTS stats output is put into `./profile`:
    `*.prof`, `*.hp`, `*.eventlog`, `*.stats`, and is tagged in the following
    manner:
    `TIMESTAMP.COMMIT-HASH.WORKING-TREE-STATUS.MNEMONIC.*`, i.e.
    `1588593053.711c6aef.modified.protozoan-maybe.stats`.

### Index

- <span><b>genesis.sh</b></span> <br/> Generate a new genesis in the `configuration/` folder
- <span><b>issue-genesis-utxo-expenditure.sh</b></span> <br/> Write a file with a transaction
         spending a genesis UTxO entry, given a key owning it
- <span><b>issue-utxo-expenditure.sh</b></span> <br/> Write a file with a transaction spending a
         normal UTxO entry, given a key owning it
- <span><b>mainnet.sh</b></span> <br/> Run a node against Cardano Mainnet (the [configuration file](https://github.com/input-output-hk/cardano-node/blob/master/configuration/defaults/byron-mainnet/configuration.yaml) contains documentation how to change options
	 and turn on/off different features)
- <span><b>mainnet-via-fetcher.sh</b></span> <br/> Works in two phases, 1) prefetch - preload the prefetcher's
	 ChainDB, up to desired limit and 2) benchmark - run the second node as the benchmark, up to desired
	 slot limit
- <span><b>shelley-testnet-live.sh</b></span> <br/> Start a dev cluster with 3 nodes,
         with neat curses-based UI (run from tmux)
- <span><b>shelley-testnet-dns.sh</b></span> <br/> Start a dev cluster with 3 nodes (run from tmux)
- <span><b>shelley-testnet.sh</b></span> <br/> Start a dev cluster with 3 nodes, with
         basic logging (run from tmux)
- <span><b>submit-tx.sh</b></span> <br/> Submit a transaction file made by `issue-*-expenditure.sh`
                      family of scripts


# Shelley Testnet

The `./scripts/benchmarking/shelley-testnet.sh` script starts up three nodes that are
connected via TCP sockets to each other and produce blocks according to the
algorithm selected (e.g. "BFT").  The blocks are shared among the nodes and
after verification integrated into a nodes ledger.  The user can submit
transactions to a node which includes them in its local mempool, and eventually
in the next block it will create.


```

 +---------+         +---------+
 |         | <-----> |         |
 | node 0  |         | node 1  |
 |         | <-+ +-> |         |
 +---------+   | |   +---------+
               v v

            +---------+
            |         |
            | node 2  |
            |         |
            +---------+


```

## Startup testnet

Add the next two lines to your $HOME/.tmux.conf file:
```
set-window-option -g mouse on
set -g default-terminal "tmux-256color"
```


Run the demo script:

`./scripts/benchmarking/shelley-testnet-live.sh`

NB: This will automatically start a tmux session and create the necessary genesis file in `configuration/defaults/liveview/genesis`


## Run chairman

#### Purpose:

Connect with all the core nodes and store the forks from a common prefix.  If any of them is longer than the security parameter (k) it will throw an exception.

#### Usage:

1) Run `./scripts/benchmarking/shelley-testnet-live.sh`.

2) Run `./scripts/benchmarking/chairman.sh ./socket/node-0-socket ./socket/node-1-socket ./socket/node-2-socket` in a separate terminal.


## Connect to mainnet

Run `./scripts/benchmarking/mainnet.sh`

This script connects to several IOHK nodes on mainnet.

## Submit a tx to the testnet

Following the instructions above, the window of the terminal will be split into four panes.
Three of the panes showing the nodes running and a shell to enter commands for transaction submission, e.g.

Create a tx:

```
./scripts/benchmarking/issue-genesis-utxo-expenditure.sh transaction-file
```
Submit the tx:

```
./scripts/benchmarking/submit-tx.sh transaction-file
```
The `submit-tx.sh` script by default sends the transaction to node with node id 0.

NB: This will submit a tx based on the `configuration/defaults/liveview/config-0.yaml` config file.

See cardano-cli's [Transactions](../cardano-node/README.md#transactions) section for instructions on how to generate a tx.

## Startup testnet with dns

You can run:

`./scripts/benchmarking/shelley-testnet-dns.sh`

instead of `shelley-testnet.sh`.
It requires that the addresses `local.iohk.io` and `local6.iohk.io` resolve to `127.0.0.1` and `::1` respectively.

You can use [unbound](https://github.com/NLnetLabs/unbound) dns server. You can use the following `/etc/unbound/unbound.conf` file:
```
server:
  verbosity: 1
  local-data: "local.iohk.io A 127.0.0.1"
  local-data: "local6.iohk.io AAAA ::1"
```

## CI cluster

See the README file in the `chairmans-cluster` subdirectory.

## Troubleshooting

All the scripts accept `--verbose`, `--debug` and `--trace` for various
intensity of debug messages.

In particular, `--trace` is equivalent to `set -x`.

## Implementation notes

The scripts are factored, so that shared functionality is segregated in
various `scripts/lib*.sh` modules:

- `scripts/lib.sh` is about running haskell binaries in general:
   - the general machinery for `--nix` and `--cabal` builds,
   - profiling & statistics (output goes into `./profile/`),
   - debugging and tracing of the scripts themselves (`--verbose`, `--debug` and `--trace`),
   - relies on `scripts/lib-nix.sh` for the implementation of `--nix`.

- `scripts/lib-node.sh` is about running `cardano-node` specifically.

- `scripts/lib-nix.sh` handles cached running of Nix binaries.
  The problem it solves is that Nix evaluation is somewhat slow, and we don't
  want to pay that cost multiple times for each binary -- so we have to cache
  its Nix output path.

- `scripts/lib-cluster.sh` is basis for the multi-node, `tmux`-based cluster scripts.
