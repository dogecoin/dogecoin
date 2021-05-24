# cardano-cli


A CLI utility to support a variety of key material operations (genesis, migration, pretty-printing..) for different system generations.

The general synopsis is as follows:

```
   Usage: cardano-cli (Genesis related CMDs | Key related CMDs | Delegation related CMDs | Transaction related CMDs | Local node related CMDs)
```

The top-level commands are as shown below.

```bash
$ cardano-cli --help
cardano-cli - utility to support a variety of key operations (genesis
generation, migration, pretty-printing..) for different system generations.

Usage: cardano-cli (Era based commands | Byron specific commands |
                     Miscellaneous commands)

Available options:
  --version                Show the cardano-cli version
  -h,--help                Show this help text

Era based commands
  address                  Payment address commands
  stake-address            Stake address commands
  key                      Key utility commands
  transaction              Transaction commands
  node                     Node operation commands
  stake-pool               Stake pool commands
  query                    Node query commands. Will query the local node whose
                           Unix domain socket is obtained from the
                           CARDANO_NODE_SOCKET_PATH enviromnent variable.
  genesis                  Genesis block commands
  governance               Governance commands
  text-view                Commands for dealing with Shelley TextView files.
                           Transactions, addresses etc are stored on disk as
                           TextView files.

Byron specific commands
  byron                    Byron specific commands

Miscellaneous commands
  version                  Show the cardano-cli version
```

Byron-specific commands

```bash
$ cardano-cli byron --help
Usage: cardano-cli byron (key | transaction | query | genesis | governance |
                           miscellaneous)
  Byron specific commands

Available options:
  -h,--help                Show this help text

Available commands:
  key                      Byron key utility commands
  transaction              Byron transaction commands
  query                    Byron node query commands.
  genesis                  Byron genesis block commands
  governance               Byron governance commands
  miscellaneous            Byron miscellaneous commands
```

## How to build

### Cabal

Use [Cabal - Version 3.4.0.0](https://www.haskell.org/cabal/) to build and/or install this project:

```
$ cd cardano-cli
$ cabal build
$ cabal install
```

It may be necessary to specify the installation directory when installing the command using the `--installdir` option.
