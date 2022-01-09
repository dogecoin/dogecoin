# Installing Dogecoin Core

### Pre-compiled binaries

The easiest way to install the latest version of the Dogecoin Core software is
by to download the latest precompiled binaries for your platform from the
[release page](https://github.com/dogecoin/dogecoin/releases). Currently,
binaries are released for the following platforms:

- Windows, 64-bit and 32-bit
- Linux, 64-bit and 32-bit
- MacOS, Intel 64-bit
- ARM, 64-bit and 32-bit Linux

These binaries are created and verified by multiple independent people, to
ensure honest and malware-free releases. See
[the gitian building documentation](doc/gitian-building.md) for more information
regarding that process.

### Compiling using packaged dependencies

It is possible to build your own copy of Dogecoin Core with the exact, tested,
dependencies, as used for the binary releases, by using the
[depends system](depends/description.md). Please refer to the
[depends README](depends/README.md) for instructions to build Dogecoin using
these dependencies.

### Compiling using system-provided libraries

  The following are developer notes on how to build Dogecoin on your native
  platform, using the dependencies as provided by your system's package manager.
  They are not complete guides, but include notes on the necessary libraries,
  compile flags, etc.

  - [Unix Build Notes](doc/build-unix.md)
  - [Windows Build Notes](doc/build-windows.md)
  - [macOS Build Notes](doc/Building-Dogecoin-1.14-for-Mac.md)

### Testing

Unit tests can be compiled and ran with `make check`. Further details on running
and extending unit tests can be found in [/src/test/README.md](/src/test/README.md).

There are also [regression and integration tests](/qa) written in Python, that
are run automatically on the build server. These tests can be run (if the
[test dependencies](/qa) are installed) with: `qa/pull-tester/rpc-tests.py`

### Tips and tricks

**compiling for debugging**

Run `configure` with the `--enable-debug` option, then `make`. Or run `configure` with
`CXXFLAGS="-g -ggdb -O0"` or whatever debug flags you need.

**debug.log**

If the code is behaving strangely, take a look in the debug.log file in the data directory;
error and debugging messages are written there.

The `-debug=...` command-line option controls debugging; running with just `-debug` will turn
on all categories (and give you a very large debug.log file).

The Qt code routes `qDebug()` output to debug.log under category "qt": run with `-debug=qt`
to see it.

**testnet and regtest modes**

Run with the `-testnet` option to run with "play dogecoins" on the test network, if you
are testing multi-machine code that needs to operate across the internet.

If you are testing something that can run on one machine, run with the `-regtest` option.
In regression test mode, blocks can be created on-demand; see qa/rpc-tests/ for tests
that run in `-regtest` mode.

**DEBUG_LOCKORDER**

Dogecoin Core is a multithreaded application, and deadlocks or other multithreading bugs
can be very difficult to track down. Compiling with `-DDEBUG_LOCKORDER` (`configure
CXXFLAGS="-DDEBUG_LOCKORDER -g"`) inserts run-time checks to keep track of which locks
are held, and adds warnings to the debug.log file if inconsistencies are detected.
