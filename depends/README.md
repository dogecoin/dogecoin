### Prerequisites

The depends system is maintained and tested using Ubuntu Bionic. Both generic
apt packages, and packages specific to the target architecture are required to
successfully compile all dependencies. Listed packages are tested and known to
work.

#### Generic packages

```
sudo apt-get install autoconf automake binutils-gold ca-certificates curl \
                     faketime git-core libtool pkg-config python bison
```

#### Generic linux: i686-pc-linux-gnu and x86_64-linux-gnu

```
sudo apt-get install g++-7-multilib gcc-7-multilib
```

#### ARM7 32bit: arm-linux-gnueabihf

```
sudo apt-get install g++-arm-linux-gnueabihf g++-7-arm-linux-gnueabihf \
                     gcc-7-arm-linux-gnueabihf binutils-arm-linux-gnueabihf
```

#### ARM 64bit: aarch64-linux-gnu

```
sudo apt-get install g++-aarch64-linux-gnu g++-7-aarch64-linux-gnu \
                     gcc-7-aarch64-linux-gnu binutils-aarch64-linux-gnu
```

#### Windows: i686-w64-mingw32 and x86_64-w64-mingw32

```
sudo apt-get install g++ g++-mingw-w64 mingw-w64 nsis zip
```

#### macOS (Intel): x86_64-apple-darwin11

```
sudo apt-get install g++ cmake imagemagick fonts-tuffy libz-dev libbz2-dev \
                     libcap-dev librsvg2-bin libtiff-tools python python-dev \
                     python-setuptools
```

### Usage

To build dependencies for the current arch+OS:

    make

To build for another arch/OS:

    make HOST=host-platform-triplet

For example:

    make HOST=x86_64-w64-mingw32 -j4

A prefix will be generated that's suitable for plugging into Bitcoin's
configure. In the above example, a dir named x86_64-w64-mingw32 will be
created. To use it for Bitcoin:

    ./configure --prefix=`pwd`/depends/x86_64-w64-mingw32

Common `host-platform-triplets` for cross compilation are:

- `i686-w64-mingw32` for Win32
- `x86_64-w64-mingw32` for Win64
- `x86_64-apple-darwin11` for MacOSX
- `arm-linux-gnueabihf` for Linux ARM 32 bit
- `aarch64-linux-gnu` for Linux ARM 64 bit

No other options are needed, the paths are automatically configured.

Dependency Options:
The following can be set when running make: make FOO=bar

    SOURCES_PATH: downloaded sources will be placed here
    BASE_CACHE: built packages will be placed here
    SDK_PATH: Path where sdk's can be found (used by OSX)
    FALLBACK_DOWNLOAD_PATH: If a source file can't be fetched, try here before giving up
    NO_QT: Don't download/build/cache qt and its dependencies
    NO_WALLET: Don't download/build/cache libs needed to enable the wallet
    NO_UPNP: Don't download/build/cache packages needed for enabling upnp
    DEBUG: disable some optimizations and enable more runtime checking
    HOST_ID_SALT: Optional salt to use when generating host package ids
    BUILD_ID_SALT: Optional salt to use when generating build package ids

If some packages are not built, for example `make NO_WALLET=1`, the appropriate
options will be passed to bitcoin's configure. In this case, `--disable-wallet`.

Additional targets:

    download: run 'make download' to fetch all sources without building them
    download-osx: run 'make download-osx' to fetch all sources needed for osx builds
    download-win: run 'make download-win' to fetch all sources needed for win builds
    download-linux: run 'make download-linux' to fetch all sources needed for linux builds

### Other documentation

- [description.md](description.md): General description of the depends system
- [packages.md](packages.md): Steps for adding packages
