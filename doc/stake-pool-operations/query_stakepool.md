# Querying a Stake Pool

Two queries are available for querying your stakepool:

* `stake-snapshot` (advanced): Get the stake snapshot information for a stake pool
* `pool-params` (advanced): Get the current and future parameters for a stake pool,
  including retirement

## Querying for stake snapshot

The stake snapshot returns information about the mark, set, go ledger snapshots for a pool, plus
the total active stake for each snapshot that can be used in a 'sigma' calculation:

```bash
$ cardano-cli query stake-snapshot \
    --stake-pool-id 00beef0a9be2f6d897ed24a613cf547bb20cd282a04edfc53d477114 \
    --mainnet
{
    "poolStakeGo": 40278547538358,
    "activeStakeGo": 22753958467474959,
    "poolStakeMark": 40424218559492,
    "activeStakeMark": 22670949084364797,
    "poolStakeSet": 39898761956772,
    "activeStakeSet": 22488877070796904
}
```

Each snapshot is taken at the end of a different era.  The `go` snapshot is the current one and
was taken two epochs earlier, `set` was taken one epoch ago, and `mark` was taken immediately
before the start of the current epoch.

This command is for debugging purposes only and may fail when used in a memory constrained
environment due to the size of the ledger state.

# Querying for pool pparameters

The pool parameters command returns three pieces of information: current parameters, future
parameters and retiring information.

They may be `null` if eg the parameters are not changing.

```bash
$ cardano-cli query pool-params \
    --stake-pool-id d785ff6a030ae9d521770c00f264a2aa423e928c85fc620b13d46eda \
    --mainnet
{
    "poolParams": {
        "publicKey": "d785ff6a030ae9d521770c00f264a2aa423e928c85fc620b13d46eda",
        "cost": 340000000,
        "metadata": {
            "hash": "b150b12a1301c4b1510ac8b9f53f7571cabb43455f6fd244cd8fd97504b1c869",
            "url": "https://adalite.io/ADLT4-metadata.json"
        },
        "owners": [
            "463a9695c9222183ee6e1523478722bebcb332fa3769f1d8ef40c7d0",
            "5049c1dac0e597ee902f27a74a167cf135ae7c1717b0d3a417cd6c67"
        ],
        "vrf": "0a21e37b1917ce37a897eb2a8dc6715973a18d0586f7ab4962e3975561151348",
        "pledge": 30000000000,
        "margin": 3.0e-2,
        "rewardAccount": {
            "network": "Mainnet",
            "credential": {
                "key hash": "b1bc146a5fb0683c4e3836712d115b98619048bc307cc059b6adc76e"
            }
        },
        "relays": [
            {
                "single host address": {
                    "IPv6": null,
                    "port": 3003,
                    "IPv4": "54.228.75.154"
                }
            },
            {
                "single host address": {
                    "IPv6": null,
                    "port": 3001,
                    "IPv4": "54.228.75.154"
                }
            },
            {
                "single host address": {
                    "IPv6": null,
                    "port": 3003,
                    "IPv4": "34.249.11.89"
                }
            },
            {
                "single host address": {
                    "IPv6": null,
                    "port": 3001,
                    "IPv4": "34.249.11.89"
                }
            }
        ]
    },
    "futurePoolParams": null,
    "retiring": null
}
```

The main advantage of these commands over using `query ledger-state` is that they avoid the need
to dump the full ledger state (which is both time consuming and memory intensive - meaning they
reduce the total system demands for SPOs), and will make it easier to support CNCLI and other
tools.

They also use existing internal operations (such as the ledger pool stake and active stake
calculations), meaning that the information is guaranteed to be identical to that which the
ledger is using (and without having to write scripts to extract/correlate the information).

This command is for debugging purposes only.
