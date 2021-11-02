Dogecoin Core version 1.14.5 is now available from:

  <https://github.com/dogecoin/dogecoin/releases/tag/v1.14.5/>

This is a new minor version release, including various bugfixes and performance improvements. It is a recommended
update for all users.

Please report bugs using the issue tracker at github:

  <https://github.com/dogecoin/dogecoin/issues>

To receive security and update notifications, please watch reddit or Twitter:

  * https://www.reddit.com/r/dogecoin/
  * @Dogecoin on Twitter for high priority announcements
  * @dogecoin\_devs on Twitter for updates on development work

Compatibility
==============

Dogecoin Core is extensively tested on Ubuntu Server LTS, Mac OS X and Windows 10.

Dogecoin Core should also work on most other Unix-like systems but is not
frequently tested on them.

Notable changes
===============

Fee Reductions
--------------

This release reduces the recommended fees, following reduction of the required fees
in 1.14.4. The main highlights for the revised fee schedule are:

* The user interface for selecting fees when sending Dogecoins has been updated to
  give an idea of how much is being spent, rather than a block target. As Dogecoin
  blocks are not full, typically all transactions are mined in the next block, and
  therefore the target estimation does not makes sense for Dogecoin.
* Transaction sizes are no longer rounded up to the nearest kilobyte before
  calculating fees, which significantly simplifies fee calculation logic. It is anticipated
  this will also simplify fee calculation by third party wallets which typically use
  Bitcoin-like fee calculation.
* The default transaction fee is now 0.001 DOGE per kilobyte, although note you may see
  transactions take 2-3 blocks to be confirmed while using low fees, until miners update.
* Tune defaults for replace by fee values, with default DEFAULT\_INCREMENTAL\_RELAY\_FEE now
  0.0001 DOGE/kb.
* Derive minimum change from configurable parameters `-discardthreshold`
  and `-mintxfee` as a basis.
  Specifically: `min change = discardThreshold + 2 * minTxFee(1000)`
* Introduce `-harddustlimit`, which is used for testing
  standard transactions. The existing dust limit (-dustlimit) is now
  enforcing the economic disincentive under which each output under
  the limit must add additional fee to be accepted to the mempool.
* Introduce `-discardthreshold`, a wallet-specific, configurable dust
  limit that enables gradual implementation of the dust limit. Each
  transaction created with the wallet will adhere to this threshold
  rather than the dust limits used for relay, so that the wallet stays
  usable while the network changes (lowers) its dust limits.
* Add dustlimit info to `getnetworkinfo` RPC command, as it is now configurable.

BDB Updated to 5.3
------------------

The Berkley DB version used by Dogecoin Core has been updated to 5.3 (from 5.1)
as 5.3 is now standard on many Linux distributions. In testing 5.1 and 5.3
files appear readily interchangeable, although we would recommend not
attempting to open wallets from Dogecoin Core 1.14.5 in previous versions of
Dogecoin Core, as a precautionary measure.

Key Derivation
--------------

The BIP32 hierarchical deterministic key derivation path contained the wrong
chain ID. Previously the chain ID 0 was used, it's now correctly set to 3 as
per [SLIP44](https://github.com/satoshilabs/slips/blob/master/slip-0044.md).

This has a relatively minimal impact currently, however in future versions
where expect more use of hierarchical deterministic keys, this is important to
define consistently. In particular it is important that the key derivation
paths used by Dogecoin Core and hardware wallets are consistent, so that
extended keys from one can be used with the other. This will also simplify
future compatibility with projects such as HWI, which could enable Dogecoin
Core to use hardware wallets.

Version UI
----------

Dogecoin version is now displayed in the bottom-left of the window, rather than
in the Dogecoin logo in the wallet window. This means it is updated
automatically, rather than requiring manual work editing the image.

createauxblock/submitauxblock
-----------------------------

The `createauxblock` and `submitauxblock` commands have been introduced,
mimicking the same commands from Namecoin 0.17+. These progress towards
splitting the consensus layer from the wallet (`getauxblock` generates an
address to mine to, while the new `createauxblock` command takes in an address
to mine to, and therefore does not require the wallet). It also enables mining
pools to use multiple wallet addresses if desired.

By default `createauxblock` provides difficulty target in a field called
`target`, however this can be tweaked for Namecoin-like API ("_target") by
setting the `-rpcnamecoinapi` startup argument.

Minor Changes
=============

* Fix compilation on FreeBSD, which was failing to compile the Scrypt code.
* Refresh FreeBSD docs, see `doc/build-freebsd.md`.
* Update to OpenSSL 1.0.2u.
* Refresh translation files to simplify volunteer contributions to translations.
* Remove export of glibc 2.17 secure_getenv to fix glibc minimum required.
* Add xkbcommon 0.8.4 to fix keyboard compatibility issues.
* Address compatibility issues with GCC-7 and glibc-2.27.
* Remove legacy patches which are no longer needed due to test environments being updated to more recent Ubuntu releases.
* Security harden systemd unit file `contrib/init/dogecoind.local.service`.
* Make Freetype library version independent from build system libaries.
* Update Univalue library to 1.0.4.
* Correct block download timeout for regtest, where it was too low and causing issues with tests.
* Experimental build for SHA algorithms with AVX2 support, to improve SHA performance.

Credits
=======

* AbcSxyZ
* Bertrand Jacquin
* Carl Dong
* cg
* chromatic
* Chun Kuan Lee
* Cory Fields
* Dakoda Greaves
* Daksh Sharma
* Dan Raviv
* dogespacewizard
* Ed Tubbs
* Elvis Begović
* fanquake
* Hennadii Stepanov
* KabDeveloper
* leezhen
* Micael Malta
* Michi Lumin
* Patrick Lodder
* Ross Nicoll
* Ryan Crosby
* Suhas Daftuar
* W. J. van der Laan
