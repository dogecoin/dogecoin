# Unix Build Notes
1. [Install dependencies](#dependencies)
 - [Ubuntu & Debian](#ubuntu)
 - [Fedora](#Fedora)
 - [FreeBSD](#FreeBSD)
2. [Build steps](#build-steps)
3. [Build configuration](#configure-build)
 - [Enable Qt Gui](#enable-qt-gui)
 - [Incompatible BerkeleyDB](#incompatible-berkleydb)
 - [Disable Wallet](#diable-wallet)
 - [Miniupnpc](#miniupnpc)
 - [Security](#security)

## Install dependencies

You must install required dependencies to run a basic Dogecoin daemon.  
Optional dependencies may vary depending on your build specifications.

**Required dependencies :**

 Library     | Purpose          | Description
 ------------|------------------|----------------------
 libssl      | Crypto           | Random Number Generation, Elliptic Curve Cryptography
 libboost    | Utility          | Library for threading, data structures, etc
 libevent    | Networking       | OS independent asynchronous networking

**Optional dependencies:**

 Library     | Purpose          | Description
 ------------|------------------|----------------------
 miniupnpc   | UPnP Support     | Firewall-jumping support
 libdb5.1    | Berkeley DB      | Wallet storage (only needed when wallet enabled)
 qt          | GUI              | GUI toolkit (only needed when GUI enabled)
 protobuf    | Payments in GUI  | Data interchange format used for payment protocol (only needed when GUI enabled)
 libqrencode | QR codes in GUI  | Optional for generating QR codes (only needed when GUI enabled)
 univalue    | Utility          | JSON parsing and encoding (bundled version will be used unless --with-system-univalue passed to configure)
 libzmq3     | ZMQ notification | Optional, allows generating ZMQ notifications (requires ZMQ version >= 4.x)

For the versions used in the release, see [release-process.md](release-process.md) under *Fetch and build inputs*.

Memory Requirements
--------------------

C++ compilers are memory-hungry. It is recommended to have at least 1.5 GB of
memory available when compiling Dogecoin Core. On systems with less, gcc can be
tuned to conserve memory with additional CXXFLAGS:

    ./configure CXXFLAGS="--param ggc-min-expand=1 --param ggc-min-heapsize=32768"

### Ubuntu & Debian

**Required dependencies** :
```
sudo apt-get install build-essential libtool autotools-dev automake pkg-config libssl-dev libevent-dev bsdmainutils
```

**Optional dependencies** :  
```
# Qt (required for dogecoin-qt GUI)
sudo apt-get install libqt5gui5 libqt5core5a libqt5dbus5 qttools5-dev qttools5-dev-tools libprotobuf-dev protobuf-compiler libqrencode-dev

# BerkeleyDB
sudo apt-get install libdb5.1-dev libdb5.1++-dev

# Boost
sudo apt-get install libboost-system-dev libboost-filesystem-dev libboost-chrono-dev libboost-program-options-dev libboost-test-dev libboost-thread-dev

# ZMQ (provides ZMQ API 4.x)
sudo apt-get install libzmq3-dev

# Miniupnpc
sudo apt-get install libminiupnpc-dev
```

### Fedora

**Required dependencies :**
```
sudo dnf install gcc-c++ libtool make autoconf automake openssl-devel libevent-devel boost-devel libdb4-devel libdb4-cxx-devel
```
**Optional dependencies :**
```
# Qt (required for dogecoin-qt GUI)
sudo dnf install qt5-qttools-devel qt5-qtbase-devel protobuf-devel qrencode-devel

# Miniupnpc
sudo dnf install miniupnpc-devel
```

## FreeBSD

**Required dependencies :**
```
sudo dnf install gcc-c++ libtool make autoconf automake openssl-devel libevent-devel boost-devel libdb4-devel libdb4-cxx-devel
```
**Optional dependencies :**
```
# BerkeleyDB
pkg install db5
```
## Build steps

According to installed dependencies, the following steps will compile `dogecoind`, `dogecoin-cli` and `dogecoin-qt`.

```bash
./autogen.sh
./configure
make
make install # optional
```
See [Build configuration](#build-configuration) section for extra settings.

## Build configuration
You can see all available options with `./configure --help`

#### Enable Qt GUI
Create `dogecoin-qt`, the core wallet GUI.
```
./configure --with-gui
```

#### Incompatible BerkeleyDB
If you're using any other BerkeleyDB version than 4.8 :
```
./configure --with-incompatible-bdb
```
#### Disable-wallet mode
When the intention is to run only a P2P node without a wallet, Dogecoin may be compiled in
disable-wallet mode with:

```
./configure --disable-wallet
```

Mining is also possible in disable-wallet mode, but only using the `getblocktemplate` RPC
call, not `getwork`.

#### Miniupnpc

[miniupnpc](http://miniupnp.free.fr/) may be used for UPnP port mapping.  It can be downloaded from [here](
http://miniupnp.tuxfamily.org/files/).  UPnP support is compiled in and
turned off by default.  See the configure options for upnp behavior desired:

```
--without-miniupnpc      No UPnP support miniupnp not required
--disable-upnp-default   (the default) UPnP support turned off by default at runtime
--enable-upnp-default    UPnP support turned on by default at runtime
```

#### Security
To help make your Dogecoin installation more secure by making certain attacks impossible to
exploit even if a vulnerability is found, binaries are hardened by default.
This can be disabled with:

Hardening Flags:

	./configure --enable-hardening
	./configure --disable-hardening


Hardening enables the following features:

* Position Independent Executable
    Build position independent code to take advantage of Address Space Layout Randomization
    offered by some kernels. Attackers who can cause execution of code at an arbitrary memory
    location are thwarted if they don't know where anything useful is located.
    The stack and heap are randomly located by default but this allows the code section to be
    randomly located as well.

    On an AMD64 processor where a library was not compiled with -fPIC, this will cause an error
    such as: "relocation R_X86_64_32 against `......' can not be used when making a shared object;"

    To test that you have built PIE executable, install scanelf, part of paxutils, and use:

	scanelf -e ./dogecoin

    The output should contain:

     TYPE
    ET_DYN

* Non-executable Stack
    If the stack is executable, trivial stack-based buffer overflow exploits are possible if
    vulnerable buffers are found. By default, Dogecoin should be built with a non-executable stack,
    but if one of the libraries it uses asks for an executable stack or someone makes a mistake
    and uses a compiler extension which requires an executable stack, it will silently build an
    executable without the non-executable stack protection.

    To verify that the stack is non-executable after compiling, use:
    `scanelf -e ./dogecoin`

    the output should contain:
	STK/REL/PTL
	RW- R-- RW-

    The STK RW- means that the stack is readable and writeable, but not executable.




ARM Cross-compilation
-------------------
These steps can be performed on, for example, an Ubuntu VM. The depends system
will also work on other Linux distributions, however the commands for
installing the toolchain will be different.

Make sure you install the build requirements mentioned above.
Then, install the toolchain and curl:

    sudo apt-get install g++-arm-linux-gnueabihf curl

To build executables for ARM:

    cd depends
    make HOST=arm-linux-gnueabihf NO_QT=1
    cd ..
    ./configure --prefix=$PWD/depends/arm-linux-gnueabihf --enable-glibc-back-compat --enable-reduce-exports LDFLAGS=-static-libstdc++
    make

For further documentation on the depends system see [README.md](../depends/README.md) in the depends directory.
