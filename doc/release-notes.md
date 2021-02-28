Dogecoin Core version 1.14.3 is now available from:

  <https://github.com/dogecoin/dogecoin/releases/tag/v1.14.3/>

This is a new minor version release, including various bugfixes and performance improvements. It is a recommended
update for all users.

Please report bugs using the issue tracker at github:

  <https://github.com/dogecoin/dogecoin/issues>

To receive security and update notifications, please watch reddit or Twitter:

  * https://www.reddit.com/r/dogecoin/
  * @Dogecoin on Twitter for high priority announcements
  * @dogecoin\_devs on Twitter for updates on development work

The developers also maintain personal Twitter accounts:

  * @langer\_hans
  * @JRossNicoll

Lastly the founders are on Twitter, although as of the time of writing not involved in
maintaining Dogecoin:

  * @ummjackson
  * @BillyM2k

Compatibility
==============

Dogecoin Core is extensively tested on Ubuntu Server LTS, Mac OS X and Windows 10.

Microsoft ended support for Windows XP on [April 8th, 2014](https://www.microsoft.com/en-us/WindowsForBusiness/end-of-xp-support),
No attempt is made to prevent installing or running the software on Windows XP, you
can still do so at your own risk but be aware that there are known instabilities and issues.
Please do not report issues about Windows XP to the issue tracker.

Dogecoin Core should also work on most other Unix-like systems but is not
frequently tested on them.

Notable changes
===============

Reduce CPU usage during sync
----------------------------

When loading block headers to send to a peer, the block was revalidated by calculating its proof of work. This is expensive and led to a bottleneck in the sync process where nodes were CPU rather than IO bound in sending blocks to ther peers.

All block headers are already checked when they are accepted, and they will be checked again on the receiving node.

Reduce default mempool expiry time
----------------------------------

Reduces DEFAULT_MEMPOOL_EXPIRY from 336 hours to 24 hours. Motivation is that while blocks are empty, un-relayable tx are stuck in mempools for a long time and effectively locking utxo for 2 weeks until they can be respent, if no RBF opt-in was performed (most wallet implementations do not do RBF opt-in.)

As the expectation is that block space will not be fully utilized for the foreseeable future, and therefore, as long as this is the case, no valid transaction should ever live in the mempool for more than a couple of minutes.

This default setting can be overridden with the -mempoolexpiry parameter by individual node operators to a value (expressed in hours) that makes the most sense for the use cases the node serves.

Increase block download timeouts
--------------------------------

Block download timeouts are expressed as a multiple of block interval, and as such Dogecoin block download times were relatively aggressive, leading to a high number of timeouts. Increased the timeouts to be more flexible to real world conditions.

Add size_on_disk, prune_target_size, automatic_pruning to getblockchaininfo
---------------------------------------------------------------------------

* Fix pruneheight help text.
* Move fPruneMode block to match output ordering with help text.
* Add functional tests for new fields in getblockchaininfo.

Add query options to listunspent RPC call
-----------------------------------------

* Return unspents greater or equal than a specific amount in DOGE: minimumAmount (default = 0).
* Return unspents lower or equal than a specific amount in DOGE: maximumAmount (default=unlimited).
* Return unspents with a total number lower or equal than a specific number: maximumCount (default=0=unlimited).
* Return unspents which total is greater or equal than a specific amount in DOGE: minimumSumAmount (default=unlimited).

Minor changes
=============

* Set BIP65 softfork heights in chainparams.cpp.
* Update package links for OSX cross compilation.
* Change IPC prefix from `bitcoin:` to `dogecoin:`.
* Locale independent sorting.
* Corrections to Italian translation.
* Refresh main and test network checkpoints and seeds.
* Do not print an error on connection timeouts through proxy.
* Numerous fixes to automated tests.
* Numerous fixes to documentation.

Credits
=======

Thanks to everyone who directly contributed to this release:

- Anthony Chen
- Bertrand Jacquin
- BT
- Daniel Edgecumbe
- Demon
- Dennis Field
- fluteds
- Ikko Ashimine
- John-Gee
- Jonathan
- Kent
- leuqarte
- Luis-Johannes Schubert
- Marco
- marcuswin
- Max Keller
- Patrick Lodder
- Pedro Branco
- Primo
- Reiner Herrmann
- Ross Nicoll
- Shibe
- tnaka
- Vertian
