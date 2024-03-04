Dogecoin Core version 1.14.6 is now available from:

  <https://github.com/dogecoin/dogecoin/releases/tag/v1.14.6/>

This is a new minor version release, including important security updates and
changes to network efficiency. All Dogecoin Core users - miners, services,
relay operators and wallet users - are strongly recommended to upgrade.

Please report bugs using the issue tracker at github:

  <https://github.com/dogecoin/dogecoin/issues>

To receive notifications about updates, subscribe to the release mailing list:

  <https://sourceforge.net/projects/dogecoin/lists/dogecoin-releases>

Releases are also announced on reddit:

  <https://www.reddit.com/r/dogecoindev/>

Compatibility
==============

Dogecoin Core is extensively tested on Ubuntu Server LTS, Intel-based macOS
and Windows 10.

Dogecoin Core should also work on most other Unix-like systems but is not
frequently tested on them.

Notable changes
===============

Important Security Updates
--------------------------

This release contains fixes that harden node and network security. These fixes
are important for every node operator and wallet user.

### Alert system removal

The P2P alert system has been removed and alert messages are no longer
processed. This was planned for the next major release, but as bugs were found
in the implementation, the full deprecation of this feature has been executed
early, because it makes no sense to overhaul a system that is unused and
already marked for removal in the near future.

### Harden the transaction download mechanism

Significantly reduce the impact of peers that withhold transaction information
(accidentally or otherwise) in the announcement stage, before the transaction
is mined, by enforcing strict controls, limits and timeouts on all transaction
announcements and giving preference to outgoing connections when deciding which
peer to request transaction information from.

This improves the resilience of the entire network and improves reliability of
transaction relay throughout.

### Other security-related fixes

- Harden the protocol implementation to reject buggy or malformed messages.
  This helps protect the network against broken custom clients.
- Limit and tightly manage memory usage in events of high network traffic or
  when connected to extremely slow peers. This protects nodes on lower end
  hardware to not run out of memory in the face of increased network activity.

Fee Recommendation
------------------

This release changes the recommended dust limit for all participants on the
Dogecoin network from 1 DOGE to 0.01 DOGE. The full recommendation can be found
[in the documentation](fee-recommendation.md).

This change has been implemented in the wallet as the default value of
`-discardthreshold`, resulting in the following default logic:

1. The wallet will attempt to not create any outputs smaller than 0.03 DOGE as
   change if possible.
2. If after signing there is more than 0.01 DOGE left in the change output, the
   wallet will keep the output as-is.
3. If the change output drops under 0.01 DOGE, the wallet will discard the
   change and instead give it to miners as fee.
4. Any output under 0.01 DOGE will not be accepted as valid by default.

Breaking changes
----------------

### Configuration of a wallet backup directory

Adds a new command line parameter `-backupdir` that allows operators to specify
where to store wallet backups and dumps. When not set, the node will add a
directory under the data directory called "backups".

**This is a breaking change.**

The behavior of the `dumpwallet` and `backupwallet` RPC methods has changed, as
these will ignore any path given other than the filename, and will always store
the filename in the directory defined with `-backupdir` or the above-mentioned
default location.

These methods will also refuse to overwrite any existing files and will produce
error messages to that effect.

### Maximum number of addnode records

To protect against users accidentally submitting an insane amount of address
records using the `addnode` RPC call, the maximum number of "addnode" records
has been limited to 800, which is 100x the maximum number of manually added
nodes that the software connects to (8).

Once this maximum is reached, the software will refuse to add any more
addresses and return an error.

Users can remove existing addnode records from the software by running:

```
dogecoin-cli addnode <address> remove
```

Features
--------

### Improve stuck transaction detection

A new RPC method has been added to help wallet operators identify if any
transactions are stuck (have not been mined and are not in the mempool)
and enable resolution. This helps in cases where node-local fee or dust
policies change, to prevent the node from sending out old transactions that
got lost.

The method can be called with:

```
dogecoin-cli liststucktransactions
```

### Add pruning configuration to the UI

Adds pruning configuration to the UI, to make it easier to save diskspace for
users of graphical (wallet) installations. Before, this could only be done
through manual editing of the configuration file.

The new setting allows a user to decrease the total disk space used for keeping
full blocks the node down to 3GB, by removing blocks that are no longer needed
for full incremental validation of the blockchain.

**Caveats**

Note that when running a pruned node, the following applies:

* The node will no longer relay blocks and transactions.
* If the node needs to recover after a hardware failure, it might need to
  re-download the entire chain.
* Features that need the entire block chain, such as rescanning the chain
  for wallet transactions, or importing old keys, will not function.
* Reverting this setting will make the node re-download the entire chain.  

### Manage the number of connections without restart

A new RPC method `setmaxconnections` has been introduced that enables wallet
and node operators to change the maximum number of connections their node
allows without needing to restart (and with that, lose all connections)
allowing for more fine-grained control of the node's network capacities.

The node will allow reducing the number of connections until a minimum of 28,
and will automatically adapt to a maximum that the host OS allows.

To change the maximum number of connections, run:

```
dogecoin-cli setmaxconnections <desired_amount>
```

### Improve the traffic graph in the UI

Significantly improves the graph in the UI's traffic monitor by not throwing
away data every time the graph scale got resampled, but instead keeping track
of the traffic we see in the background and then rendering it on demand.

### Add a rescan RPC method

A new RPC method, `rescan`, has been added to allow node operators to rescan
the chain for wallet transactions from a certain height. This can help speed
up the rescan process for keys that are known to be generated after a certain
date or block height, as older blocks do not necessarily require to be scanned
in that scenario. This may also be helpful when adding large quantities of
watchonly addresses.

NOTE: rescanning the blockchain takes a lot of time.

To use the rescan functionality, use:

```
dogecoin-cli rescan <start_block>
```

It will inform of the state before and after when the process finishes:

```
{
  "before": {
    "balance": 69.00000000,
    "txcount": 1
  },
  "after": {
    "balance": 420.00000000,
    "txcount": 3
  },
  "blocks_scanned": 31337,
  "time_elapsed": 23
}
```

### New REST endpoint: blockhashbyheight

A new endpoint has been added to the REST server to find the current best known
block at a certain height, without having to do RPC calls. This is useful for
service implementations that need to track or query blocks.

The new endpoint can be queried with:

```
GET /rest/blockhashbyheight/<height>
```

Translation Updates
-------------------

Significant updates have been provided to the following languages:

* Bulgarian
* Dutch
* French
* Italian
* Korean
* Persian
* Portuguese (pt_PT)
* Tamil

Policies and tooling have been updated to enable translators better in their
efforts to make Dogecoin more accessible to people that do not speak English.

RPC API Changes
---------------

* The `verifychain` RPC command no longer accepts negative values
* Added `liststucktransactions`
* Added `setmaxconnections`
* Added `rescan`
* `addnode` will now return error `-32` when the address provided is too long
* `getpeerinfo` provides 2 new fields per peer, `addr_processed` and
  `addr_rate_limited`, that track `addr` message processing
* `dumpwallet` and `backupwallet` now write their output to the configurable
  backup directory and do not overwrite existing files

Dependency Updates
------------------

* Updated the default dependency zlib to `1.2.12`.
* Updated the default dependency libevent to `2.1.12-stable`.
* Update the LevelDB subtree to use the same version as Bitcoin `22.0`, from
  the bitcoin-core maintained fork.

Minor Changes
=============

* Added ARMv8 and ARMv82 crypto extensions as an experimental feature. This
  significantly improves the speed at which 64-bit ARM hardware can validate
  blocks and transactions.
* Fixed the graphical wallet to properly display in High-DPI resolutions.
* Improved privacy by preventing leaks of information about our wallet when
  setting the locktime field (automatically.)
* Added scrypt to the benchmark code.
* Change the granularity of data downloaded in the network tab to no longer
  display gigabytes and terrabytes, as this obscures too much detail. Instead,
  the largest unit is now megabytes.
* Fixed a bug in the getutxos REST endpoint to also show data when not
  including the mempool.
* Improved the tips displayed in the UI to be more concise and give better
  advice.
* Removed information from the graphical wallet that was attempting to indicate
  stuck transactions based on peer-to-peer traffic, but this does not work
  since 1.14.2, and can be misleading (showing false positives and negatives.)
* Improved and updated the [bip list](bips.md) documentation that explains the
  protocol features implemented by Dogecoin Core.
* Greatly improved the `gitian-build` script to ease doing deterministic builds
  for everyone, and the [documentation](gitian-building.md) of that process.

Credits
=======

Credit goes to all awesome contributors to this release, alphabetically:

* AbcSxyZ
* alamshafil
* Ayushman Singh Chauhan
* Bhaskar Kashyap
* ch0k1
* Chao Liu
* chromatic
* Chuanyi
* Chun Kuan Lee
* Dakoda Greaves
* DogecoinItalia
* dogespacewizard
* Ed Tubbs
* Evan Klitzke
* fanquake
* fdov
* Gregory Maxwell
* Hennadii Stepanov
* huedaya
* iamagf
* inevitable360
* Jade Hamel
* Jadi
* Jeremy Rubin
* Jimkov
* just-an-dev
* justAndrea1111
* katesalazar
* Kaz Wesley
* MVShishkov
* Maniixer
* MarcoFalke
* Martyornot
* Matheus Bach
* Matt Corallo
* Micael Malta
* Michi Lumin
* Misha Behersky
* MishaBoar
* Mohammad Ali Haghshenas
* Motahhar Mokf
* Patrick Lodder
* Pieter Wuille
* practicalswift
* RnoHach
* Roman Zeyde
* Russell Yanofsky
* scarletletters
* sinetek
* Staartvos
* Steve Chung
* thisiskeanyvy
* vertiond
* VJ
* Wladimir J. van der Laan
* ZeeAmini
