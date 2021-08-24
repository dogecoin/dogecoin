# Unix Build of Dogecoin Core

This installation guide serves as a basis for all UNIX and UNIX-like operating
systems. All examples in this guide are illustrated using an Ubuntu/Debian OS,
and guides for different OSs can be found in separate documentation, as listed
below.

1. [Install dependencies](#install-dependencies)
    * [Using packaged dependencies](#using-packaged-dependencies)
    * [Ubuntu & Debian example](#ubuntu--debian-example)
2. [Build steps](#build-steps)
3. [Build configuration](#build-configuration)
    * [Enable Qt GUI](#enable-qt-gui)
    * [Wallet](#wallet)
    * [Disable-wallet mode](#disable-wallet-mode)
    * [Miniupnpc](#miniupnpc)
    * [Security](#security)
    * [Memory requirements](#memory-requirements)
4. [ARM Cross-compilation](#arm-cross-compilation)
5. OS-specific documentation:
    * [Archlinux](build-archlinux.md)
    * [Fedora](build-fedora.md)
    * [FreeBSD](build-freebsd.md)
    * [NixOs](build-nixos.md)
    * [OpenBSD](build-openbsd.md)

## Install dependencies

You must install required dependencies to build a basic Dogecoin daemon, optional dependencies may vary according to your requirements.

Wallet is optional to run a node, see [Wallet](#wallet) section to enable them.

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
 libdb5.3    | Berkeley DB      | Wallet storage (only needed when wallet enabled)
 qt          | GUI              | GUI toolkit (only needed when GUI enabled)
 protobuf    | Payments in GUI  | Data interchange format used for payment protocol (only needed when GUI enabled)
 libqrencode | QR codes in GUI  | Optional for generating QR codes (only needed when GUI enabled)
 univalue    | Utility          | JSON parsing and encoding (bundled version will be used unless --with-system-univalue passed to configure)
 libzmq3     | ZMQ notification | Optional, allows generating ZMQ notifications (requires ZMQ version >= 4.x)

For the versions used in the release, see [release-process.md](release-process.md) under *Fetch and build inputs*.

### Using packaged dependencies

Dogecoin Core's released binaries are built using the "depends system", which
contains exact, tested versions of all dependencies. To create binaries that are
using only fully tested dependencies, see the documentation in the
[depends directory](../depends/README.md) and the
[Release process documentation](release-process.md).

### Ubuntu & Debian example

**Required dependencies** :
```bash
sudo apt-get install build-essential libtool autotools-dev automake pkg-config libssl-dev libevent-dev bsdmainutils
sudo apt-get install libboost-system-dev libboost-filesystem-dev libboost-chrono-dev libboost-program-options-dev libboost-test-dev libboost-thread-dev
```

**Optional dependencies** :  
```bash
# Qt (required for dogecoin-qt GUI)
sudo apt-get install libqt5gui5 libqt5core5a libqt5dbus5 qttools5-dev qttools5-dev-tools libprotobuf-dev protobuf-compiler libqrencode-dev

# BerkeleyDB (version 5.3)
sudo apt install libdb5.3++-dev libdb5.3++ libdb5.3-dev

# ZMQ (provides ZMQ API 4.x)
sudo apt-get install libzmq3-dev

# Miniupnpc
sudo apt-get install libminiupnpc-dev
```

## Build steps

According to installed dependencies, the following steps will compile `dogecoind`, `dogecoin-cli` and `dogecoin-qt`.

```bash
./autogen.sh
./configure
make
make install # optional
```
See [Build configuration](#build-configuration) for extra settings.

## Build configuration

Configurations are done during the `./configure` step. Use `--help` to see all available options.

#### Enable Qt GUI
Create `dogecoin-qt`, the core wallet GUI.
```bash
./configure --with-gui
```

#### Wallet
BerkeleyDB is required for wallet functionality and use the `wallet.dat` file.

By default, **Dogecoin Core expect BerkeleyDB 5.3**.
You can use a different version by specifying `--with-incompatible-bdb` flag.

If no package is available for your distribution  in optional dependencies, you can build BerkeleyDB from source :
```bash
# Install script for BerkeleyDB 5.3

# BerkeleyDB installation directory
BDB_PREFIX=$(pwd)/bdb
mkdir $BDB_PREFIX

# Fetch the source and verify shasum
wget 'http://download.oracle.com/berkeley-db/db-5.3.28.NC.tar.gz'
echo '76a25560d9e52a198d37a31440fd07632b5f1f8f9f2b6d5438f4bc3e7c9013efdb-5.3.28.NC.tar.gz' | sha256sum -c

# Extract sources
tar -xzvf db-5.3.28.NC.tar.gz
cd db-5.3.28.NC/build_unix/

# Apply patch (see https://gist.github.com/danieldk/5700533)
sed -i  's/__atomic_compare_exchange/__atomic_compare_exchange_db/g' ../src/dbinc/atomic.h

# Note: Do a static build so that it can be embedded into the executable, instead of having to find a .so at runtime
../dist/configure --prefix=$BDB_PREFIX --enable-cxx --disable-shared --with-pic
make install
```

Then use `LDFLAGS` and `CPPFLAGS` during configuration to link the database :
```bash
./configure  LDFLAGS="-L${BDB_PREFIX}/lib/" CPPFLAGS="-I${BDB_PREFIX}/include/"
```

#### Disable-wallet mode
When the intention is to run only a P2P node without a wallet, Dogecoin may be compiled in
disable-wallet mode with:

```bash
./configure --disable-wallet
```

Mining is also possible in disable-wallet mode, but only using the `getblocktemplate` RPC
call, not `getwork`.

#### Miniupnpc

[miniupnpc](http://miniupnp.free.fr/) may be used for UPnP port mapping.  It can be downloaded from [here](
http://miniupnp.tuxfamily.org/files/).  UPnP support is compiled in and
turned off by default.  See the configure options for upnp behavior desired:

```bash
--without-miniupnpc      #No UPnP support miniupnp not required
--disable-upnp-default   #(the default) UPnP support turned off by default at runtime
--enable-upnp-default    #UPnP support turned on by default at runtime
```

#### Security
To help make your Dogecoin installation more secure by making certain attacks impossible to
exploit even if a vulnerability is found, binaries are hardened by default.
This can be disabled with:

Hardening Flags:
```bash
./configure --enable-hardening
./configure --disable-hardening
```

Hardening enables the following features:

* Position Independent Executable
    Build position independent code to take advantage of Address Space Layout Randomization
    offered by some kernels. Attackers who can cause execution of code at an arbitrary memory
    location are thwarted if they don't know where anything useful is located.
    The stack and heap are randomly located by default but this allows the code section to be
    randomly located as well.

    On an AMD64 processor where a library was not compiled with -fPIC, this will cause an error
    such as: `relocation R_X86_64_32 against '......' can not be used when making a shared object;`

    To test that you have built PIE executable, install scanelf, part of paxutils, and use:

	`scanelf -e ./dogecoin`

    The output should contain:

    `TYPE ET_DYN`

* Non-executable Stack
    If the stack is executable, trivial stack-based buffer overflow exploits are possible if
    vulnerable buffers are found. By default, Dogecoin should be built with a non-executable stack,
    but if one of the libraries it uses asks for an executable stack or someone makes a mistake
    and uses a compiler extension which requires an executable stack, it will silently build an
    executable without the non-executable stack protection.

    To verify that the stack is non-executable after compiling, use:
    `scanelf -e ./dogecoin`

    the output should contain:
    `STK/REL/PTL RW- R-- RW-`

    The `STK RW-` means that the stack is readable and writeable, but not executable.


#### Memory Requirements

C++ compilers are memory-hungry. It is recommended to have at
least 1.5 GB of memory available when compiling Dogecoin Core.
On systems with less, gcc can be tuned to conserve memory with additional CXXFLAGS:

```bash
./configure CXXFLAGS="--param ggc-min-expand=1 --param ggc-min-heapsize=32768"
```

## ARM Cross-compilation

These steps can be performed on, for example, an Ubuntu VM. The depends system
will also work on other Linux distributions, however the commands for
installing the toolchain will be different.

Make sure you install the build requirements mentioned above.
Then, install the toolchain and curl:
```bash
sudo apt-get install g++-arm-linux-gnueabihf curl
```

To build executables for ARM:
```bash
cd depends
make HOST=arm-linux-gnueabihf NO_QT=1
cd ..
./configure --prefix=$PWD/depends/arm-linux-gnueabihf --enable-glibc-back-compat --enable-reduce-exports LDFLAGS=-static-libstdc++
make
```
For further documentation on the depends system see [README.md](../depends/README.md) in the depends directory.
