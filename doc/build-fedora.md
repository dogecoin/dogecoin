Fedora build guide
------------------

**Last tested with:** 1.14.6-dev (as of 22884709)
**Tested on:** Fedora 35,35,36

### Build requirements

**Fedora < 36:**

```sh
sudo dnf install gcc-c++ libtool which diffutils make autoconf automake \
                 openssl-devel libevent-devel boost-devel libdb-cxx-devel
```

**Fedora >= 36:**

```sh
sudo dnf install gcc-c++ libtool which diffutils make autoconf automake \
                 openssl1.1-devel libevent-devel boost-devel libdb-cxx-devel
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
