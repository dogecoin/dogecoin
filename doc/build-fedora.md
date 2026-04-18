Fedora build guide
------------------

**Last tested with:** 1.14.7-dev (as of aa53a0d7)
**Tested on:** Fedora 35-39 (x86 & ARM)

### Build requirements

```sh
sudo dnf install gcc-c++ libtool which diffutils make autoconf automake \
                 openssl-devel libevent-devel boost-devel libdb-cxx-devel
```
### Optional elements

#### MiniUPNP

Useful when your node is running behind a NAT router.

```sh
sudo dnf install miniupnpc-devel
```

#### ZeroMQ

```sh
sudo dnf install zeromq-devel
```

#### GUI requirements

To build the GUI with Qt 5 you need the following:

```sh
sudo dnf install qt5-qttools-devel qt5-qtbase-devel protobuf-devel qrencode-devel
``` 

### Build instructions

From the root of the repository, run:

```sh
./autogen.sh
./configure --without-gui
make
```

This builds `dogecoind` and `dogecoin-cli`.

To also build the Qt GUI (`dogecoin-qt`), install the GUI requirements above and run:

```sh
./autogen.sh
./configure --with-gui
make
```

Install binaries system-wide (optional):

```sh
sudo make install
```
