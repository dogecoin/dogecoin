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

The developers also maintain personal Twitter accounts:

  * @langer\_hans
  * @JRossNicoll
  * @michilumin

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

Enable Reduced Fees
-------------------

1.14.4 is more permissive about fees on transactions, as part of a two-stage update to the recommended fees.
The main highlights for this are:

* Transaction sizes are no longer rounded up to the nearest kilobyte when deciding if a transaction can be accepted from another node.
* The default fee used when deciding if a transaction should be accepted from a node is 0.001 DOGE (reduced from 1 DOGE).
* Dust limit is now configurable via the `-dustlimit` option.

These will not be immediately user visible, as the recommended fees do not change, in order to retain backwards compatibilty while the network upgrades. We will release 1.14.5 to reduce the recommended fees, shortly, or you can run Dogecoin Core with `-paytxfee=0.001` to reduce your default fees manually.

Synchronization Improvements
----------------------------

* Disconnect peers which do not respond to requests for headers in a timely manner, to optimise synchronization rate.
* Dogecoin Core will no longer request further headers from a peer it is already downloading headers from. This stops Dogecoin Core from requesting the same headers more than once, and reduces network bandwidth wasted.
* Proactively reject block headers which would build on an invalid chain.

GUI Improvements
----------------

* Add menu option to import a private key, "Import Private Key" from the "File" menu.
* Improve displayed result when commands in the debug console return null.
* Fix text overflow in the paper wallet generator.
* Add column to peers table showing bytes sent/received, accessible via "Debug Window" from the "Help" menu.
* Add GUI for adding peers manually, accessible from the peers table of the Debug Window.

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
* Correctly set fixed seeds on startup.
* Add build instructions for NixOS.
* Fix a rare crash bug on shutdown due to ActivateBestChain() being called when there is no best chain.
* Fix port numbers in `contrib/seeds/generate-seeds.py`.

Credits
=======

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
