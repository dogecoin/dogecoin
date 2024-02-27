Dogecoin Core version 1.14.7 is now available from:

  <https://github.com/dogecoin/dogecoin/releases/tag/v1.14.7/>

This is a new minor version release, including enchancements to several RPC
methods and important security updates for Dogecoin-Qt. Dogecoin-Qt users on
any platform are strongly recommended to upgrade.

Please report bugs using the issue tracker at GitHub:

  <https://github.com/dogecoin/dogecoin/issues>

To receive notifications about updates, subscribe to the release mailing list:

  <https://sourceforge.net/projects/dogecoin/lists/dogecoin-releases>


Compatibility
==============

Dogecoin Core is extensively tested on Ubuntu Server LTS, macOS and Windows.
Minimum OS compatibility can be found [in the INSTALL guide](../INSTALL.md).

Notable changes
================

Important Security Updates
--------------------------

This release contains fixes for Dogecoin Qt across all platforms that increase
security for end users.

### Disable BIP-70 payment server by default

To mitigate future potential risk inside Dogecoin Qt, support for BIP-70 payment
requests and related BIP-71 and BIP-72 payment URIs and MIME types have been
disabled by default, because this functionality interacts with remote websites
in an automated manner and could be used to amplify any future vulnerabilities
inside Dogecoin Qt. By default, incoming BIP-70 payment requests and files or
URLs that use the functionality are rejected and an explanation is shown to the
user.

BIP-21 payment requests remain fully supported.

The functionality can be re-enabled by using `enable-bip70=1` either in your
dogecoin.conf or as an argument to Dogecoin-Qt if absolutely needed. Please use
extreme caution when exercising this option.

This is step 1 in full deprecation of BIP-70, BIP-71 and BIP-72 inside Dogecoin
Core. Future releases may completely remove this feature.

*Implemented with #3412*

### Other security-related fixes

* Reduce the build scope of the Qt dependency to only include those features we
  use, to not package potential vulnerabilities. Also stop using libX11 features
  outside of libxcb (#3358)
* Backport patches for all Qt versions until 5.15.12 + community patches to the
  pinned Qt 5.7.1, because we cannot update that due to backward compatibility
  guarantees (#3415)
* Added security guidance to INSTALL.md and build guides, to remind those that
  self-compile to update their system libraries. (#3384)
* Updated the `rpcuser.py` script to improve the password and salt generation
  methods it uses (#3388). If you've used this script in the past, replacing
  current authentication in `dogecoin.conf` with new credentials generated with
  the new version of this script is recommended.


Maintain RPC fee estimation facilities
---------------------------------------

Fee estimation was explicitly not ported or supported on Dogecoin Core since
1.14.0, but as the non-functional data from `estimatefee` and `estimatesmartfee`
were anyway used in the field, services using these RPC methods were unable to
provide fee estimates to their users. Therefore this feature has now been ported
to support Dogecoin parametrization.

The RPC methods `estimatefee` and `estimatesmartfee` are now under active
maintenance and will be further enhanced in the future.

NOTE: Because the minimum and maximum tracked fees and spacing of fee buckets
has changed, the `fee_estimates.dat` file from earlier versions **will be
discarded** after restarting this version of the software. This means that after
the first restart, the estimates will reset, but become usable again after the
node has processed a few blocks, and will improve in precision over up to 12
hours after the first restart. Any subsequent restarts will use the estimates
from the file as usual.

***If you have in the past ran a custom compiled version of 1.14.7 or created
your own higher version, you must manually delete the `fee_estimates.dat` file
from your datadir for the new parametrization to take full effect.***

*Implemented with #3389 and #3433*


Add fully verbose transaction decoding to `getblock`
-----------------------------------------------------

Updates the `getblock` RPC to use verbosity levels 0-2 besides the existing
boolean. This allows operators to decode a full block in one go when using
level `2`, instead of having to query `getrawtransaction` for each individual
txid returned with the boolean `true`.

Mapping between boolean and integer parameter functionality:

| Verbosity                       | boolean | level (int) |
| :------------------------------ | ------: | ----------: |
| Hex encoded block               |   false |           0 |
| Block with txids                |    true |           1 |
| Block with decoded transactions |       - |           2 |

For more information see:

```
dogecoin-cli help getblock
```

*Implemented with #3299, #3306 and #3307*


Features
--------

### Add the `getblockstats` RPC method

The new `getblockstats` RPC method computes statistics for a given block hash if
the block is available on the node (pruned nodes can only run statistics over
blocks that aren't pruned.)

The user can select which stats are returned to save processing time for stats
that aren't wanted. For more information, use:

```
dogecoin-cli help getblockstats
```

*Implemented with #3297*


### Add a `height` parameter to `-walletnotify`

Adds `%i` in the command to include the height of the block containing the
transaction. If the transaction is not in any block, the height is `0`.

*Implemented with #3257 and #3382*


Translation Updates
-------------------

Updates have been provided to the following languages:

* Chinese (#3103, #3419)
* Dutch (#3435)
* French (#3148, #3195)
* French-Canadian (#3441)
* Italian (#3428)
* Korean (#3395, #3430)
* Polish (#3431)


RPC API Changes
---------------

* Added `getblockstats` (#3297)
* Allowed `verbosity` to be expressed as an integer to `getblock` and add full
  verbosity under level `2` that serializes all transactions as JSON in the
  reply (#3299, #3306, #3307)
* Added a `height` parameter to `importpubkey` (#3102) and `importaddress` (#3235)
  which allows the user to specify from which chain height to rescan for wallet
  transactions.
* Added `getmocktime` (regtest-only) (#3322)
* Added documentation for RPC method maturity in [rpc-maturity.md](rpc-maturity.md)
  (#3443)


Dependency Updates
------------------

* Updated the depends, CI and build system to Ubuntu Focal (#3143, #3144,
  #3145, #3222)
* Updated expat to version 2.5.0 (#3271)
* Updated the secp256k1 subtree to 44c2452 (#3082, #3142)
* Updated zlib to version 1.3 (#3345)
* Updated protobuf to version 3.6.1 (#3357)
* Updated fontconfig to version 2.12.6 (#3364)
* Updated the experimental intel-ipsec-mb dependency to version 1.2, adds
  AVX2 support for Windows x86_64 (#3071, #3146, #3214)

NOTE: on systems that cannot use pinned dependencies from `depends/`, please
be aware that due to a bug in `boost-1.83`, building may lead to failure.
More information about the bug can be found at the `boostorg/signals` GitHub
repository at https://github.com/boostorg/signals2/issues/68.

Bug Fixes
==========

* Fixed a bug where `-maxtxfee` was not being respected outside of the wallet,
  which caused problems with the `sendrawtransaction` RPC call. (#3088)
* Fixed the example `dogecoind.service` file (#3066)
* Fixed a bug where misnamed signal handlers were causing excessive warnings
  in Dogecoin Qt logs (#3063)
* Fixed a bug where extremely long wallet labels could cause Qt popups to flow
  over screen limits, causing the user to have no means of accepting or
  rejecting the transaction (#3224)
* Fixed an interaction error where running Qt tests interacted with the mainnet
  datadir (#3286)
* Fixed a bug that caused qt4 compilation to fail (#3427)


Minor Changes
=============

* Enforced explicit enabling of experimental features by introducing a build
  flag (`--enable-experimental`) and enforce these at compile time (#3136).
  See [experiments.md](experiments.md) for more information about which
  experiments are available to self-compile.
* Re-enabled Scrypt SSE2 routines and encapsulated it as an experimental feature
  to help speeding up PoW verification on x86_64 architecture. (#2773)
* Packaged the Noto Sans font to make sure all languages can be rendered (#2676)
* Updated `assumevalid` to help new nodes save CPU time by setting it to block
  `e7d4577405223918491477db725a393bcfc349d8ee63b0a4fde23cbfbfd81dea`, at height
  5,050,000 on mainnet (#3416)
* Enhanced the paper wallet print function to default to the User's paper format
  instead of A4 (#3239)
* Updated build guides for MacOS (#2686), Windows (#3340, #3411), Fedora (#3434)
* Introduced modern tooling for inspecting and constraining our binaries (#3204)
* Refactored out C-style NUL-terminated strings in interfaces (#3157)
* Enhanced the CI to run unit tests in armhf and aarch64 linux builds (#3025)
* Added a translation in Vietnamese (#3060), and fixed missing information in
  the Chinese README translations (#3070)
* Updated the python `ltc-scrypt` module to a maintained fork (#3080), which
  can be found at https://github.com/dogecoin/ltc-scrypt
* Backported a pure Python implementation of RIPEMD160 from Bitcoin Core (#3081)
* Removed historical OpenSSL comparison tests to increase compatibility with
  OpenSSL 3+ (#3079)
* Ported extended tests for `rpcbind` (#3228) and `feefilter` (#3241)


Credits
=======

Credit goes to all awesome contributors to this release, alphabetically:

* Ajay Chowdhury
* alamshafil
* Anish M
* Anthony Chen
* Bertrand Jacquin
* bobdos
* Brett T. Warden
* Carl Dong
* chromatic
* cijsk
* Cory Fields
* daanksy
* Dakoda Greaves
* danielw86dev
* Dylan Ferris
* Ed Tubbs
* fanquake
* Fierce Skit
* fultondoge
* georgeartem
* hwanyoungChoi
* ilsubyega
* Jalin Wang
* jimjimmiejames
* John-Inubook
* junderw
* kalvdans
* mangekyousharingan
* MarsDoge
* Martin
* Martyornot
* Michi Lumin
* mintodev
* MishaBoar
* NinVoido
* omahs
* oshawa-connection
* Patrick Lodder
* Pieter Wuille
* pmb
* practicalswift
* randomwalk266
* Schmeckl3s
* serious-gemini
* Shubham Mathur
* Skylar Loomis
* sunerok
* thisiskeanyvy
* Thành Nhân
* tosufever
* victorsk2019
