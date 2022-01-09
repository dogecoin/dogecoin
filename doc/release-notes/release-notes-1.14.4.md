Dogecoin Core version 1.14.4 is now available from:

  <https://github.com/dogecoin/dogecoin/releases/tag/v1.14.4/>

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

Enabling Future Fee Reductions
-------------------------------

This release preparing the network for a reduction of the recommended fees by
reducing the default fee requirement 1000x for transaction relay and 100x for
mining. At the same time it increases freedom for miner, wallet and node
operators to agree on fees regardless of defaults coded into the Dogecoin Core
software by solidifying fine-grained controls for operators to deviate from
built-in defaults.

This realizes the first part of a two-stage update to lower the fee
recommendation.

The main highlights for these enhancements are:

* Transaction sizes are no longer rounded up to the nearest kilobyte when
  deciding if a transaction can be accepted from another node and in applying
  fee-filter requests from peers, when relaying transactions.
* The default setting shipped with dogecoin core for relay fee has been reduced
  to 0.001 DOGE (was: 1 DOGE). This can be changed by operators using the
  `-mintxrelayfee=<amount>` option.
* Spam management has been delegated to miners, where currently a default fee
  of 0.01 DOGE has been set as a recommended default, to prevent spam on the
  blockchain. Miners can change this setting to their liking using the
  `-blockmintxfee` option.
* The relay dust limit has been reduced 100x to 0.01 DOGE and is now
  configurable via the `-dustlimit` option.

For this release, the recommended fees and dust limits, as implemented in the
wallet, remain at 1 DOGE per kilobyte, inclusive of the rounding up to the
nearest kilobyte, as miners and the relay network will upgrade gradually,
requiring time for transactions with lower fees to be able to be relayed and
mined. Not doing this would result in all transactions being rejected by old
nodes. A subsequent release will finalize the second stage and lower the
recommended fees implemented in the wallet by default. Wallet operators can
however, at their own judgement and convenience, change the fees paid from
their wallets with the `-paytxfee=<amount per kb>` option.

Synchronization Improvements
----------------------------

This release removes a bug in the network layer where a 1.14 node would open
many parallel requests for headers to its peers, increasing the total data
transferred during initial block download up to 50 times the required data, per
peer, unnecessarily. As a result, synchronization has time has been reduced by
around 2.5 times.

Additionally, when a node is in initial synchronization and a peer takes too
long to respond to a new header request, it is now aggressively disconnected,
to free connection slots for faster peers and not add more stress to already
overloaded peers.

Security enhancements
---------------------

* Proactively disconnect peers sending block headers which would build on an
  invalid chain.
* Improve handling and logging of invalid blocks and their descendants
* Fix a bug that was preventing nodes to load a fixed peer list in case DNS
  services are unreachable.

GUI Improvements
----------------

* Add menu option to import a private key, "Import Private Key" from the "File"
  menu.
* Improve displayed result when commands in the debug console return null.
* Fix text overflow on printed keys and address fields in the paper wallet
  generator.
* Add column to peers table showing bytes sent/received, accessible via
  "Debug Window" from the "Help" menu.
* Add GUI for adding peers manually, accessible from the peers table of the
  Debug Window.

RPC Improvements
----------------

`getpeerinfo` now includes `feefilter` value for each peer, to be able to diagnose transaction relay issues.

Minor Changes
=============

* Corrections to French Canadian, Chinese, German, Indonesian, Korean, Polish and Portuguese translations.
* Correct a bug that resulted in negative progress per hour being displayed during sync.
* Regtest network can now generate AuxPoW blocks.
* Add Snap packaging support.
* Modify Scrypt code so it's compatible with Alpine Linux's musl library.
* Update libevent to 2.1.11
* Update ZMQ to 4.3.4
* Add build instructions for NixOS.
* Fix a rare crash bug on shutdown due to ActivateBestChain() being called when there is no best chain.
* Fix port numbers in `contrib/seeds/generate-seeds.py`.

Credits
=======

* AbcSxyZ
* Ahmed Castro
* Alan Rudolf
* cg
* chey
* chromatic
* Cory Fields
* creekhu
* Dakoda Greaves
* David Millard
* Demon
* Dídac Coll Pujals
* Escanor Liones
* fanquake
* Florian Schade
* fmhc
* Gabriel Gosselin Roberge
* Gabriel Pérez
* geekwisdom
* Ikko Ashimine
* Jeroen Ooms
* Jerry Park
* Joakim Taule Kartveit
* katzenmalen
* Khakim Hudaya
* kregerl
* lee5378
* lynklody
* Malta Micael
* Matheus Tavares
* Matt Domko
* Maximilian Keller
* MD Islam
* Mich De L'Orme
* Michi Lumin
* motz0815
* nformant
* Patrick Lodder
* Piotr Zajączkowski
* p-j01
* rht
* Ross Nicoll
* sabotagebeats
* Shafil Alam
* stefanwouldgo
* Will
* xt3r
* Zach Latta
