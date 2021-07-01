Dogecoin Core 0.14.2
=====================

Setup
---------------------
[Dogecoin Core](http://dogecoin.com/) is the reference Dogecoin client and it builds the backbone of the network. However, it downloads and stores the entire history of Bitcoin transactions (which is currently several GBs); depending on the speed of your computer and network connection, the synchronization process can take anywhere from a few hours to a day or more.

Running
---------------------
The following are some helpful notes on how to run Dogecoin on your native platform.

### Unix

Unpack the files into a directory and run:

- `bin/dogecoin-qt` (GUI) or
- `bin/dogecoind` (headless)

### Windows

Unpack the files into a directory, and then run dogecoin-qt.exe.

### OS X

Drag Dogecoin-Core to your applications folder, and then run Dogecoin-Core.

### Need Help?

* See the documentation at the [Bitcoin Wiki](https://en.bitcoin.it/wiki/Main_Page)
for help and more information.
* Ask for help on [#dogecoin](http://webchat.freenode.net?channels=dogecoin) on Freenode. If you don't have an IRC client use [webchat here](http://webchat.freenode.net?channels=dogecoin).
* Ask for help on the [BitcoinTalk](https://bitcointalk.org/) forums, in the [Dogecoin thread](https://bitcointalk.org/index.php?topic=361813.0).

Building
---------------------
The following are developer notes on how to build Dogecoin on your native platform. They are not complete guides, but include notes on the necessary libraries, compile flags, etc.

- [OS X Build Notes](build-osx.md)
- [Unix Build Notes](build-unix.md)
- [Windows Build Notes](build-windows.md)
- [OpenBSD Build Notes](build-openbsd.md)
- [Gitian Building Guide](gitian-building.md)

Development
---------------------
The Dogecoin repo's [root README](/README.md) contains relevant information on the development process and automated testing.

- [Developer Notes](developer-notes.md)
- [Release Notes](release-notes.md)
- [Release Process](release-process.md)
- [Translation Process](translation_process.md)
- [Translation Strings Policy](translation_strings_policy.md)
- [Travis CI](travis-ci.md)
- [Unauthenticated REST Interface](REST-interface.md)
- [Shared Libraries](shared-libraries.md)
- [BIPS](bips.md)
- [Dnsseed Policy](dnsseed-policy.md)
- [Benchmarking](benchmarking.md)

### Resources
* Discuss on the [BitcoinTalk](https://bitcointalk.org/) forums, in the [Dogecoin thread](https://bitcointalk.org/index.php?topic=361813.0).
* Discuss on [#dogecoin-dev](http://webchat.freenode.net/?channels=dogecoin-dev) on Freenode. If you don't have an IRC client use [webchat here](http://webchat.freenode.net/?channels=dogecoin-dev).

### Miscellaneous
- [Assets Attribution](assets-attribution.md)
- [Files](files.md)
- [Fuzz-testing](fuzzing.md)
- [Reduce Traffic](reduce-traffic.md)
- [Tor Support](tor.md)
- [Init Scripts (systemd/upstart/openrc)](init.md)
- [ZMQ](zmq.md)

License
---------------------
Distributed under the [MIT software license](/COPYING).
This product includes software developed by the Bitcoin developers for use in [Dogecoin Core](https://www.bitcoin.org/). 
This product includes software developed by the OpenSSL Project for use in the [OpenSSL Toolkit](https://www.openssl.org/). This product includes
cryptographic software written by Eric Young ([eay@cryptsoft.com](mailto:eay@cryptsoft.com)), and UPnP software written by Thomas Bernard.
