Dogecoin Core version 1.14.8 is now available from:

  <https://github.com/dogecoin/dogecoin/releases/tag/v1.14.8/>

This is a new minor version release. Dogecoin users on any platform are
strongly recommended to upgrade.

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

Important Updates
-----------------

Several important updates have been added to Dogecoin Core, including fixes
backported from Bitcoin Core's upstream.

### Reduce Peer INV Object Counts

Previously, Dogecoin Core nodes would maintain a large list of inventory for
each peer. This is excessive and unnecessary and has been reduced to a more
reasonable number.

*Implemented with #3577*

### Improve Transaction Request Tracking

A new transaction request tracking system improves the way Dogecoin Core
communicates with other peers. Notably, this improvement prioritizes
communication with outbound peers over inbound peers and makes CPU and memory
use more reliable and scalable with the number of peers.

*Implemented with #3577*

### Improve Orphan Transaction Handling

In previous versions of Dogecoin Core, orphan transactions were processed in a
complex fashion. Now they are processed more like regular getdata messages,
which allows for more efficient handling of messages arriving from other peers,
as well as other code simplifications.

*Implemented with #3575*

### Reduce Getheaders Traffic for INV Messages

Dogecoin Core now sends a getheaders message for one block when announced in an
INV message, and it sends only a single such message rather than multiple per
INV message. This reduces traffic and simplifies the logic of the code.

*Implemented with #3568*

### Improve Reporting of Difficulty Adjustment Errors

Previously, difficulty adjustment violations were not reported for chains that
branched off before the last checkpoint. This has been changed by moving the
checkpoint check after the difficulty check.

*Implemented with #3576*

### Security Policy Documented

The [SECURITY.md](SECURITY.md) file describes how to report a potential
vulnerability or security issue in Dogecoin Core.

*Implemented with #3611*


Dependency Updates
------------------

* Updated fontconfig to version 2.12.6 (#3590)
* Updated freetype to version 2.11.0 (#3590)
* Updated expat to version 2.6.2 (#3590)
* Allow compiling with Boost 1.80 and newer (#3588)


Minor Changes
=============

* Dogecoin Core now supports compilation with C++14 and C++17, if you select
  the appropriate configuration flags (#3494, ... )
* With the C++ version updates, the code now uses more built-in C++ features instead
  of Boost equivalents, where possible (various commits)
* The CI configuration was updated to be compatible with newer versions of Python (#3582)
* Rely on the well-tested OpenSSL secure random functions, but allow the use of std::shuffle (#3521)


Credits
=======

Credit goes to all awesome contributors to this release, alphabetically:

* Amiti Uttarwar
* Anthony Chen
* chromatic
* daanksy
* Dakoda Greaves
* Ed Tubbs
* Hennadii Stepanov
* John Newbery
* Michi Lumin
* Patrick Lodder
* Pieter Wuille
* practicalswift
* Russell Yanofsky
* Twinky-kms
* Wladimir J. van der Laan
