Dogecoin Core version 1.14.2 is now available from:

  <https://github.com/dogecoin/dogecoin/releases/tag/v1.14.2/>

This is a new major version release, including new features, various bugfixes
and performance improvements, as well as updated translations.

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

Low-level RPC changes
---------------------

- Error codes have been updated to be more accurate for the following error cases:
  - `getblock` now returns RPC_MISC_ERROR if the block can't be found on disk (for
  example if the block has been pruned). Previously returned RPC_INTERNAL_ERROR.
  - `pruneblockchain` now returns RPC_MISC_ERROR if the blocks cannot be pruned
  because the node is not in pruned mode. Previously returned RPC_METHOD_NOT_FOUND.
  - `pruneblockchain` now returns RPC_INVALID_PARAMETER if the blocks cannot be pruned
  because the supplied timestamp is too late. Previously returned RPC_INTERNAL_ERROR.
  - `pruneblockchain` now returns RPC_MISC_ERROR if the blocks cannot be pruned
  because the blockchain is too short. Previously returned RPC_INTERNAL_ERROR.
  - `setban` now returns RPC_CLIENT_INVALID_IP_OR_SUBNET if the supplied IP address
  or subnet is invalid. Previously returned RPC_CLIENT_NODE_ALREADY_ADDED.
  - `setban` now returns RPC_CLIENT_INVALID_IP_OR_SUBNET if the user tries to unban
  a node that has not previously been banned. Previously returned RPC_MISC_ERROR.
  - `removeprunedfunds` now returns RPC_WALLET_ERROR if bitcoind is unable to remove
  the transaction. Previously returned RPC_INTERNAL_ERROR.
  - `removeprunedfunds` now returns RPC_INVALID_PARAMETER if the transaction does not
  exist in the wallet. Previously returned RPC_INTERNAL_ERROR.
  - `fundrawtransaction` now returns RPC_INVALID_ADDRESS_OR_KEY if an invalid change
  address is provided. Previously returned RPC_INVALID_PARAMETER.
  - `fundrawtransaction` now returns RPC_WALLET_ERROR if bitcoind is unable to create
  the transaction. The error message provides further details. Previously returned
  RPC_INTERNAL_ERROR.
  - `bumpfee` now returns RPC_INVALID_PARAMETER if the provided transaction has
  descendants in the wallet. Previously returned RPC_MISC_ERROR.
  - `bumpfee` now returns RPC_INVALID_PARAMETER if the provided transaction has
  descendants in the mempool. Previously returned RPC_MISC_ERROR.
  - `bumpfee` now returns RPC_WALLET_ERROR if the provided transaction has
  has been mined or conflicts with a mined transaction. Previously returned
  RPC_INVALID_ADDRESS_OR_KEY.
  - `bumpfee` now returns RPC_WALLET_ERROR if the provided transaction is not
  BIP 125 replaceable. Previously returned RPC_INVALID_ADDRESS_OR_KEY.
  - `bumpfee` now returns RPC_WALLET_ERROR if the provided transaction has already
  been bumped by a different transaction. Previously returned RPC_INVALID_REQUEST.
  - `bumpfee` now returns RPC_WALLET_ERROR if the provided transaction contains
  inputs which don't belong to this wallet. Previously returned RPC_INVALID_ADDRESS_OR_KEY.
  - `bumpfee` now returns RPC_WALLET_ERROR if the provided transaction has multiple change
  outputs. Previously returned RPC_MISC_ERROR.
  - `bumpfee` now returns RPC_WALLET_ERROR if the provided transaction has no change
  output. Previously returned RPC_MISC_ERROR.
  - `bumpfee` now returns RPC_WALLET_ERROR if the fee is too high. Previously returned
  RPC_MISC_ERROR.
  - `bumpfee` now returns RPC_WALLET_ERROR if the fee is too low. Previously returned
  RPC_MISC_ERROR.
  - `bumpfee` now returns RPC_WALLET_ERROR if the change output is too small to bump the
  fee. Previously returned RPC_MISC_ERROR.

miniupnp CVE-2017-8798
----------------------------

Bundled miniupnpc was updated to 2.0.20170509. This fixes an integer signedness error
(present in MiniUPnPc v1.4.20101221 through v2.0) that allows remote attackers
(within the LAN) to cause a denial of service or possibly have unspecified
other impact.

Performance Improvements
--------------

Validation speed and network propagation performance have been greatly
improved, leading to much shorter sync and initial block download times.

- The script signature cache has been reimplemented as a "cuckoo cache",
  allowing for more signatures to be cached and faster lookups.
- Assumed-valid blocks have been introduced which allows script validation to
  be skipped for ancestors of known-good blocks, without changing the security
  model. See below for more details.
- In some cases, compact blocks are now relayed before being fully validated as
  per BIP152.
- P2P networking has been refactored with a focus on concurrency and
  throughput. Network operations are no longer bottlenecked by validation. As a
  result, block fetching is several times faster than previous releases in many
  cases.
- The UTXO cache now claims unused mempool memory. This speeds up initial block
  download as UTXO lookups are a major bottleneck there, and there is no use for
  the mempool at that stage.


Manual Pruning
--------------

Dogecoin Core introduces pruning the blockchain in 1.14. Pruning
the blockchain allows for significant storage space savings as the vast majority of
the downloaded data can be discarded after processing so very little of it remains
on the disk.

Manual block pruning can now be enabled by setting `-prune=1`. Once that is set,
the RPC command `pruneblockchain` can be used to prune the blockchain up to the
specified height or timestamp.

`getinfo` Deprecated
--------------------

The `getinfo` RPC command has been deprecated. Each field in the RPC call
has been moved to another command's output with that command also giving
additional information that `getinfo` did not provide. The following table
shows where each field has been moved to:

|`getinfo` field   | Moved to                                  |
|------------------|-------------------------------------------|
`"version"`	   | `getnetworkinfo()["version"]`
`"protocolversion"`| `getnetworkinfo()["protocolversion"]`
`"walletversion"`  | `getwalletinfo()["walletversion"]`
`"balance"`	   | `getwalletinfo()["balance"]`
`"blocks"`	   | `getblockchaininfo()["blocks"]`
`"timeoffset"`	   | `getnetworkinfo()["timeoffset"]`
`"connections"`	   | `getnetworkinfo()["connections"]`
`"proxy"`	   | `getnetworkinfo()["networks"][0]["proxy"]`
`"difficulty"`	   | `getblockchaininfo()["difficulty"]`
`"testnet"`	   | `getblockchaininfo()["chain"] == "test"`
`"keypoololdest"`  | `getwalletinfo()["keypoololdest"]`
`"keypoolsize"`	   | `getwalletinfo()["keypoolsize"]`
`"unlocked_until"` | `getwalletinfo()["unlocked_until"]`
`"paytxfee"`	   | `getwalletinfo()["paytxfee"]`
`"relayfee"`	   | `getnetworkinfo()["relayfee"]`
`"errors"`	   | `getnetworkinfo()["warnings"]`

ZMQ On Windows
--------------

Previously the ZeroMQ notification system was unavailable on Windows
due to various issues with ZMQ. These have been fixed upstream and
now ZMQ can be used on Windows. Please see [this document](https://github.com/bitcoin/bitcoin/blob/master/doc/zmq.md) for
help with using ZMQ in general.

Nested RPC Commands in Debug Console
------------------------------------

The ability to nest RPC commands has been added to the debug console. This
allows users to have the output of a command become the input to another
command without running the commands separately.

The nested RPC commands use bracket syntax (i.e. `getwalletinfo()`) and can
be nested (i.e. `getblock(getblockhash(1))`). Simple queries can be
done with square brackets where object values are accessed with either an 
array index or a non-quoted string (i.e. `listunspent()[0][txid]`). Both
commas and spaces can be used to separate parameters in both the bracket syntax
and normal RPC command syntax.

Network Activity Toggle
-----------------------

A RPC command and GUI toggle have been added to enable or disable all p2p
network activity. The network status icon in the bottom right hand corner 
is now the GUI toggle. Clicking the icon will either enable or disable all
p2p network activity. If network activity is disabled, the icon will 
be grayed out with an X on top of it.

Additionally the `setnetworkactive` RPC command has been added which does
the same thing as the GUI icon. The command takes one boolean parameter,
`true` enables networking and `false` disables it.

Out-of-sync Modal Info Layer
----------------------------

When Dogecoin Core is out-of-sync on startup, a semi-transparent information
layer will be shown over top of the normal display. This layer contains
details about the current sync progress and estimates the amount of time
remaining to finish syncing. This layer can also be hidden and subsequently
unhidden by clicking on the progress bar at the bottom of the window.

Support for JSON-RPC Named Arguments
------------------------------------

Commands sent over the JSON-RPC interface and through the `bitcoin-cli` binary
can now use named arguments. This follows the [JSON-RPC specification](http://www.jsonrpc.org/specification)
for passing parameters by-name with an object.

`bitcoin-cli` has been updated to support this by parsing `name=value` arguments
when the `-named` option is given.

Some examples:

    src/bitcoin-cli -named help command="help"
    src/bitcoin-cli -named getblockhash height=0
    src/bitcoin-cli -named getblock blockhash=000000000019d6689c085ae165831e934ff763ae46a2a6c172b3f1b60a8ce26f
    src/bitcoin-cli -named sendtoaddress address="(snip)" amount="1.0" subtractfeefromamount=true

The order of arguments doesn't matter in this case. Named arguments are also
useful to leave out arguments that should stay at their default value. The
rarely-used arguments `comment` and `comment_to` to `sendtoaddress`, for example, can
be left out. However, this is not yet implemented for many RPC calls, this is
expected to land in a later release.

The RPC server remains fully backwards compatible with positional arguments.

Opt into RBF When Sending
-------------------------

A new startup option, `-walletrbf`, has been added to allow users to have all
transactions sent opt into RBF support. The default value for this option is
currently `false`, so transactions will not opt into RBF by default. The new
`bumpfee` RPC can be used to replace transactions that opt into RBF.

Sensitive Data Is No Longer Stored In Debug Console History
-----------------------------------------------------------

This only affects users that have explicitly enabled UPnP through the GUI
setting or through the `-upnp` option, as since the last UPnP vulnerability
(in Bitcoin Core 0.10.3) it has been disabled by default.

If you use this option, it is recommended to upgrade to this version as soon as
possible.

Known Bugs
==========

Since 1.14.0 the approximate transaction fee shown in Dogecoin-Qt when using coin
control and smart fee estimation does not reflect any change in target from the
smart fee slider. It will only present an approximate fee calculated using the
default target. The fee calculated using the correct target is still applied to
the transaction and shown in the final send confirmation dialog.

1.14.0 Change log
=================

Detailed release notes follow. This overview includes changes that affect
behavior, not code moves, refactors and string updates. For convenience in locating
the code changes and accompanying discussion, both the pull request and
git merge commit are mentioned.

### RPC and other APIs
- #10410 `321419b` Fix importwallet edge case rescan bug (ryanofsky)

### P2P protocol and network code
- #10424 `37a8fc5` Populate services in GetLocalAddress (morcos)
- #10441 `9e3ad50` Only enforce expected services for half of outgoing connections (theuni)

### Build system
- #10414 `ffb0c4b` miniupnpc 2.0.20170509 (fanquake)
- #10228 `ae479bc` Regenerate bitcoin-config.h as necessary (theuni)

### Miscellaneous
- #10245 `44a17f2` Minor fix in build documentation for FreeBSD 11 (shigeya)
- #10215 `0aee4a1` Check interruptNet during dnsseed lookups (TheBlueMatt)

### GUI
- #10231 `1e936d7` Reduce a significant cs_main lock freeze (jonasschnelli)

### Wallet
- #10294 `1847642` Unset change position when there is no change (instagibbs)

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
>>>>>>> Update release notes for 1.14 (#1464)

Credits
=======

Thanks to everyone who directly contributed to this release:

- Alex Morcos
- Cory Fields
- fanquake
- Gregory Sanders
- Jonas Schnelli
- Matt Corallo
- Matthew King
- matthias
- Max Keller
- Micha
- Michael Ford
- Michael Rotarius
- Mitchell Cash
- mrbandrews
- mruddy
- Nicolas DORIER
- nomnombtc
- Patrick Lodder
- Patrick Strateman
- Pavel Janík
- Pedro Branco
- Peter Todd
- Pieter Wuille
- poole\_party
- practicalswift
- R E Broadley
- randy-waterhouse
- Richard Kiss
- Robert McLaughlin
- rodasmith
- Ross Nicoll
- Russell Yanofsky
- Shigeya Suzuki
- Wladimir J. van der Laan

As well as everyone that helped translating on [Transifex](https://www.transifex.com/projects/p/bitcoin/).

