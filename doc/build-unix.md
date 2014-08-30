UNIX BUILD NOTES
====================
Some notes on how to build Dogecoin in Unix. 

To Build
---------------------

	./autogen.sh
	./configure
	make

This will build dogecoin-qt as well if the dependencies are met.

Dependencies
---------------------

 Library     | Purpose          | Description
 ------------|------------------|----------------------
 libssl      | SSL Support      | Secure communications
 libdb5.1    | Berkeley DB      | Wallet storage
 libboost    | Boost            | C++ Library
 miniupnpc   | UPnP Support     | Optional firewall-jumping support
 qt          | GUI              | GUI toolkit
 protobuf    | Payments in GUI  | Data interchange format used for payment protocol
 libqrencode | QR codes in GUI  | Optional for generating QR codes
 
 Suggested versions of these libraries are as follows:
      openssl-1.0.1i
      db-5.1.29
      boost 1.55
      miniupnpc-1.9.20140701
      qt 4.6.4
      protobuf-2.5.0
      qrencode-3.4.3

[miniupnpc](http://miniupnp.free.fr/) may be used for UPnP port mapping.  It can be downloaded from [here](
http://miniupnp.tuxfamily.org/files/).  UPnP support is compiled in and
turned off by default.  See the configure options for upnp behavior desired:

	--without-miniupnpc      No UPnP support miniupnp not required
	--disable-upnp-default   (the default) UPnP support turned off by default at runtime
	--enable-upnp-default    UPnP support turned on by default at runtime

Licenses of statically linked libraries:
 Berkeley DB   New BSD license with additional requirement that linked
               software must be free open source
 Boost         MIT-like license
 miniupnpc     New (3-clause) BSD license

- For the versions used in the release, see doc/release-process.md under *Fetch and build inputs*.

System requirements
--------------------

C++ compilers are memory-hungry. It is recommended to have at least 1 GB of
memory available when compiling Dogecoin Core. With 512MB of memory or less
compilation will take much longer due to swap thrashing.

Dependency Build Instructions: Ubuntu & Debian
----------------------------------------------
Build requirements:

	sudo apt-get install build-essential pkg-config
	sudo apt-get install libtool autotools-dev autoconf automake
	sudo apt-get install libssl-dev

for Ubuntu 12.04 and later:

	sudo apt-get install libboost-all-dev libdb5.1-dev libdb5.1++-dev

for Ubuntu 13.10:
	libboost1.54 will not work,
	remove libboost1.54-all-dev and install libboost1.53-all-dev instead.

for other Debian:

	sudo apt-get install libdb5.1-dev
	sudo apt-get install libdb5.1++-dev

Optional:

	sudo apt-get install libminiupnpc-dev (see --with-miniupnpc and --enable-upnp-default)

Dependencies for the GUI: Ubuntu & Debian
-----------------------------------------

If you want to build Dogecoin-Qt, make sure that the required packages for Qt development
are installed. Either Qt 4 or Qt 5 are necessary to build the GUI.
If both Qt 4 and Qt 5 are installed, Qt 4 will be used. Pass `--with-gui=qt5` to configure to choose Qt5.
To build without GUI pass `--without-gui`.

To build with Qt 4 you need the following:

    sudo apt-get install libqt4-dev libprotobuf-dev protobuf-compiler

For Qt 5 you need the following:

    sudo apt-get install libqt5gui5 libqt5core5 libqt5dbus5 qttools5-dev qttools5-dev-tools libprotobuf-dev

libqrencode (optional) can be installed with:

    sudo apt-get install libqrencode-dev

Once these are installed, they will be found by configure and a dogecoin-qt executable will be
built by default.

Notes
-----
The release is built with GCC and then "strip dogecoind" to strip the debug
symbols, which reduces the executable size by about 90%.


miniupnpc
---------
	tar -xzvf miniupnpc-1.6.tar.gz
	cd miniupnpc-1.6
	make
	sudo su
	make install


Berkeley DB
-----------
It is recommended to use Berkeley DB 5.1. If you have to build it yourself:

```bash
BITCOIN_ROOT=$(pwd)

# Pick some path to install BDB to, here we create a directory within the dogecoin directory
BDB_PREFIX="${BITCOIN_ROOT}/db5"
mkdir -p $BDB_PREFIX

# Fetch the source and verify that it is not tampered with
wget 'http://download.oracle.com/berkeley-db/db-5.1.29.NC.tar.gz'
echo '08238e59736d1aacdd47cfb8e68684c695516c37f4fbe1b8267dde58dc3a576c  db-5.1.29.NC.tar.gz' | sha256sum -c
# -> db-5.1.29.NC.tar.gz: OK
tar -xzvf db-5.1.29.NC.tar.gz

# Build the library and install to our prefix
cd db-5.1.29.NC/build_unix/
#  Note: Do a static build so that it can be embedded into the exectuable, instead of having to find a .so at runtime
../dist/configure --enable-cxx --disable-shared --with-pic --prefix=$BDB_PREFIX
make install

# Configure Dogecoin Core to use our own-built instance of BDB
cd $BITCOIN_ROOT
./configure (other args...) LDFLAGS="-L${BDB_PREFIX}/lib/" CPPFLAGS="-I${BDB_PREFIX}/include/"
```

**Note**: You only need Berkeley DB if the wallet is enabled (see the section *Disable-Wallet mode* below).

Boost
-----
If you need to build Boost yourself:

	sudo su
	./bootstrap.sh
	./bjam install


Dependency Build Instructions: Fedora
-------------------------------------

Tested on Fedora 20:

	sudo yum install autoconf automake make gcc-c++
	sudo yum install openssl-devel
	sudo yum install miniupnpc-devel
	sudo yum install boost-devel
	sudo yum install libdb-cxx-devel
	sudo yum install libss-devel
	sudo yum install qrencode

Optional:

	sudo yum install miniupnpc-devel (see USE_UPNP compile flag)



Security
--------
To help make your dogecoin installation more secure by making certain attacks impossible to
exploit even if a vulnerability is found, binaries are hardened by default.
This can be disabled with:

Hardening Flags:

	./configure --enable-hardening
	./configure --disable-hardening


Hardening enables the following features:

* Position Independent Executable
    Build position independent code to take advantage of Address Space Layout Randomization
    offered by some kernels. An attacker who is able to cause execution of code at an arbitrary
    memory location is thwarted if he doesn't know where anything useful is located.
    The stack and heap are randomly located by default but this allows the code section to be
    randomly located as well.

    On an Amd64 processor where a library was not compiled with -fPIC, this will cause an error
    such as: "relocation R_X86_64_32 against `......' can not be used when making a shared object;"

    To test that you have built PIE executable, install scanelf, part of paxutils, and use:

    	scanelf -e ./dogecoin

    The output should contain:
     TYPE
    ET_DYN

* Non-executable Stack
    If the stack is executable then trivial stack based buffer overflow exploits are possible if
    vulnerable buffers are found. By default, dogecoin should be built with a non-executable stack
    but if one of the libraries it uses asks for an executable stack or someone makes a mistake
    and uses a compiler extension which requires an executable stack, it will silently build an
    executable without the non-executable stack protection.

    To verify that the stack is non-executable after compiling use:
    `scanelf -e ./dogecoin`

    the output should contain:
	STK/REL/PTL
	RW- R-- RW-

    The STK RW- means that the stack is readable and writeable but not executable.

Disable-wallet mode
--------------------
When the intention is to run only a P2P node without a wallet, dogecoin may be compiled in
disable-wallet mode with:

    ./configure --disable-wallet

In this case there is no dependency on Berkeley DB 5.1.

Mining is also possible in disable-wallet mode, but only using the `getblocktemplate` RPC
call not `getwork`.

