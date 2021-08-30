Building on FreeBSD
--------------------

**Last tested with:** 1.14.5-dev (as of 18dbe32)
**Tested on:** FreeBSD 11.4

Clang is installed by default as `cc` compiler, this makes it easier to get
started than on other distros. Installing dependencies:

    pkg install autoconf automake libtool pkgconf
    pkg install boost-libs openssl libevent
    pkg install gmake

You need to use GNU make (`gmake`) instead of `make`.
(`libressl` instead of `openssl` will also work)

For the wallet (optional):

    pkg install db5

As of writing, the default hardening routines will fail on the scrypt code, so
currently, no hardened executables can be built, and the `--disable-hardening`
flag is needed for successful compilation.

Then build using:

```bash
  ./autogen.sh
  ./configure --disable-hardening MAKE="gmake" \
      CFLAGS="-I/usr/local/include" CXXFLAGS="-I/usr/local/include -I/usr/local/include/db5" \
      LDFLAGS="-L/usr/local/lib -L/usr/local/lib/db5"
  gmake
```

*Note on debugging*: The version of `gdb` installed by default is [ancient and considered harmful](https://wiki.freebsd.org/GdbRetirement).
It is not suitable for debugging a multi-threaded C++ program, not even for getting backtraces. Please install the package `gdb` and
use the versioned gdb command e.g. `gdb7111`.
