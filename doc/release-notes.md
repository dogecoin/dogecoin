Dogecoin Core version 1.14.3 is now available from:

  <https://github.com/dogecoin/dogecoin/releases/tag/v1.14.3/>

This is a new minor version release, including various bugfixes and
performance improvements.

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
=============

Dogecoin Core is extensively tested on Ubuntu Server LTS, Mac OS X and Windows 10.

Microsoft ended support for Windows XP on [April 8th, 2014](https://www.microsoft.com/en-us/WindowsForBusiness/end-of-xp-support),
No attempt is made to prevent installing or running the software on Windows XP, you
can still do so at your own risk but be aware that there are known instabilities and issues.
Please do not report issues about Windows XP to the issue tracker.

Dogecoin Core should also work on most other Unix-like systems but is not
frequently tested on them.

Notable changes
===============

Denial-of-Service vulnerability CVE-2018-17144
----------------------------------------------

A denial-of-service vulnerability exploitable by miners has been discovered in
Bitcoin Core versions 0.14.0 up to 0.16.2. It is recommended to upgrade any of
the vulnerable versions to 0.14.3, 0.15.2 or 0.16.3 as soon as possible.

Known Bugs
==========

Since 1.14.0 the approximate transaction fee shown in Dogecoin-Qt when using coin
control and smart fee estimation does not reflect any change in target from the
smart fee slider. It will only present an approximate fee calculated using the
default target. The fee calculated using the correct target is still applied to
the transaction and shown in the final send confirmation dialog.

1.14.3 Change log
=================

Detailed release notes follow. This overview includes changes that affect
behavior, not code moves, refactors and string updates. For convenience in locating
the code changes and accompanying discussion, both the pull request and
git merge commit are mentioned.

### Consensus
- #14247 `52965fb` Fix crash bug with duplicate inputs within a transaction (TheBlueMatt, sdaftuar)
 
### RPC and other APIs

- #10445 `87a21d5` Fix: make CCoinsViewDbCursor::Seek work for missing keys (Pieter Wuille, Gregory Maxwell)
- #9853 Return correct error codes in setban(), fundrawtransaction(), removeprunedfunds(), bumpfee(), blockchain.cpp (John Newbery)


### P2P protocol and network code

- #10234 `d289b56` [net] listbanned RPC and QT should show correct banned subnets (John Newbery)

### Build system


### Miscellaneous

- #10451 `3612219` contrib/init/bitcoind.openrcconf: Don't disable wallet by default (Luke Dashjr)
- #10250 `e23cef0` Fix some empty vector references (Pieter Wuille)
- #10196 `d28d583` PrioritiseTransaction updates the mempool tx counter (Suhas Daftuar)
- #9497 `e207342` Fix CCheckQueue IsIdle (potential) race condition and remove dangerous constructors. (Jeremy Rubin)

### GUI

- #9481 `7abe7bb` Give fallback fee a reasonable indent (Luke Dashjr)
- #9481 `3e4d7bf` Qt/Send: Figure a decent warning colour from theme (Luke Dashjr)
- #9481 `e207342` Show more significant warning if we fall back to the default fee (Jonas Schnelli)

### Wallet

- #10308 `28b8b8b` Securely erase potentially sensitive keys/values (tjps)
- #10265 `ff13f59` Make sure pindex is non-null before possibly referencing in LogPrintf call. (Karl-Johan Alm)

GUI Changes
-----------

 - After resetting the options by clicking the `Reset Options` button 
   in the options dialog or with the `-resetguioptions` startup option, 
   the user will be prompted to choose the data directory again. This 
   is to ensure that custom data directories will be kept after the 
   option reset which clears the custom data directory set via the choose 
   datadir dialog.

 - Multiple peers can now be selected in the list of peers in the debug 
   window. This allows for users to ban or disconnect multiple peers 
   simultaneously instead of banning them one at a time.

 - An indicator has been added to the bottom right hand corner of the main
   window to indicate whether the wallet being used is a HD wallet. This
   icon will be grayed out with an X on top of it if the wallet is not a
   HD wallet.

Low-level RPC changes
----------------------

 - `importprunedfunds` only accepts two required arguments. Some versions accept
   an optional third arg, which was always ignored. Make sure to never pass more
   than two arguments.

 - The first boolean argument to `getaddednodeinfo` has been removed. This is 
   an incompatible change.

 - RPC command `getmininginfo` loses the "testnet" field in favor of the more
   generic "chain" (which has been present for years).

 - A new RPC command `preciousblock` has been added which marks a block as
   precious. A precious block will be treated as if it were received earlier
   than a competing block.

 - A new RPC command `importmulti` has been added which receives an array of 
   JSON objects representing the intention of importing a public key, a 
   private key, an address and script/p2sh

 - Use of `getrawtransaction` for retrieving confirmed transactions with unspent
   outputs has been deprecated. For now this will still work, but in the future
   it may change to only be able to retrieve information about transactions in
   the mempool or if `txindex` is enabled.

 - A new RPC command `getmemoryinfo` has been added which will return information
   about the memory usage of Dogecoin Core. This was added in conjunction with
   optimizations to memory management. See [Pull #8753](https://github.com/bitcoin/bitcoin/pull/8753)
   for more information.

 - A new RPC command `bumpfee` has been added which allows replacing an
   unconfirmed wallet transaction that signaled RBF (see the `-walletrbf`
   startup option above) with a new transaction that pays a higher fee, and
   should be more likely to get confirmed quickly.

HTTP REST Changes
-----------------

 - UTXO set query (`GET /rest/getutxos/<checkmempool>/<txid>-<n>/<txid>-<n>
   /.../<txid>-<n>.<bin|hex|json>`) responses were changed to return status 
   code `HTTP_BAD_REQUEST` (400) instead of `HTTP_INTERNAL_SERVER_ERROR` (500)
   when requests contain invalid parameters.

Fee Estimation Changes
----------------------

- Fee estimation for a confirmation target of 1 block has been
  disabled. The fee slider will no longer be able to choose a target of 1 block.
  This is only a minor behavior change as there was often insufficient
  data for this target anyway. `estimatefee 1` will now always return -1 and
  `estimatesmartfee 1` will start searching at a target of 2.

- The default target for fee estimation is changed to 6 blocks in both the GUI
  (previously 25) and for RPC calls (previously 2).

Removal of Priority Estimation
------------------------------

- Estimation of "priority" needed for a transaction to be included within a target
  number of blocks has been removed.  The RPC calls are deprecated and will either
  return -1 or 1e24 appropriately. The format for `fee_estimates.dat` has also
  changed to no longer save these priority estimates. It will automatically be
  converted to the new format which is not readable by prior versions of the
  software.

- Support for "priority" (coin age) transaction sorting for mining is
  considered deprecated in Core and will be removed in the next major version.
  This is not to be confused with the `prioritisetransaction` RPC which will remain
  supported by Core for adding fee deltas to transactions.

P2P connection management
--------------------------

- Peers manually added through the `-addnode` option or `addnode` RPC now have their own
  limit of eight connections which does not compete with other inbound or outbound
  connection usage and is not subject to the limitation imposed by the `-maxconnections`
  option.

- New connections to manually added peers are performed more quickly.

Introduction of assumed-valid blocks
-------------------------------------

- A significant portion of the initial block download time is spent verifying
  scripts/signatures.  Although the verification must pass to ensure the security
  of the system, no other result from this verification is needed: If the node
  knew the history of a given block were valid it could skip checking scripts
  for its ancestors.

- A new configuration option 'assumevalid' is provided to express this knowledge
  to the software.  Unlike the 'checkpoints' in the past this setting does not
  force the use of a particular chain: chains that are consistent with it are
  processed quicker, but other chains are still accepted if they'd otherwise
  be chosen as best. Also unlike 'checkpoints' the user can configure which
  block history is assumed true, this means that even outdated software can
  sync more quickly if the setting is updated by the user.

- Because the validity of a chain history is a simple objective fact it is much
  easier to review this setting.  As a result the software ships with a default
  value adjusted to match the current chain shortly before release.  The use
  of this default value can be disabled by setting -assumevalid=0

Fundrawtransaction change address reuse
----------------------------------------

- Before 1.14, `fundrawtransaction` was by default wallet stateless. In
  almost all cases `fundrawtransaction` does add a change-output to the
  outputs of the funded transaction. Before 1.14, the used keypool key was
  never marked as change-address key and directly returned to the keypool
  (leading to address reuse).  Before 1.14, calling `getnewaddress`
  directly after `fundrawtransaction` did generate the same address as
  the change-output address.

- Since 0.14, fundrawtransaction does reserve the change-output-key from
  the keypool by default (optional by setting  `reserveChangeKey`, default =
  `true`)

- Users should also consider using `getrawchangeaddress()` in conjunction
  with `fundrawtransaction`'s `changeAddress` option.

Unused mempool memory used by coincache
----------------------------------------

- Before 1.14, memory reserved for mempool (using the `-maxmempool` option)
  went unused during initial block download, or IBD. In 0.14, the UTXO DB cache
  (controlled with the `-dbcache` option) borrows memory from the mempool
  when there is extra memory available. This may result in an increase in
  memory usage during IBD for those previously relying on only the `-dbcache`
  option to limit memory during that time.

Credits
=======

Thanks to everyone who directly contributed to this release:

- Cory Fields
- CryptAxe
- fanquake
- Jeremy Rubin
- John Newbery
- Jonas Schnelli
- Gregory Maxwell
- Karl-Johan Alm
- Luke Dashjr
- MarcoFalke
- Matt Corallo
- Max Keller
- Mikerah
- Patrick Lodder
- Pieter Wuille
- practicalswift
- Ross Nicoll
- Suhas Daftuar
- Thomas Snider
- Tjps
- Wladimir J. van der Laan
