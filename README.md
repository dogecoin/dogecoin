# CoinGreen Core 

## License – MIT license ⚖️
CoinGreen Core is released under the terms of the MIT license. See
[COPYING](COPYING) for more information or see
[opensource.org](https://opensource.org/licenses/MIT)

## Status
[![Windows GUI build](https://github.com/maxirmx/CoinGreen/actions/workflows/windows-gui.yml/badge.svg)](https://github.com/maxirmx/CoinGreen/actions/workflows/windows-gui.yml)

## Development and contributions – omg developers
Development is ongoing, and the development team, as well as other volunteers,
can freely work in their own trees and submit pull requests when features or
bug fixes are ready.

#### Version strategy
Version numbers are following ```major.minor.patch``` semantics.

#### Branches
There are 3 types of branches in this repository:

- **master:** Stable, contains the latest version of the latest *major.minor* release.
- **maintenance:** Stable, contains the latest version of previous releases, which are still under active maintenance. Format: ```<version>-maint```
- **development:** Unstable, contains new code for planned releases. Format: ```<version>-dev```

*Master and maintenance branches are exclusively mutable by release. Planned*
*releases will always have a development branch and pull requests should be*
*submitted against those. Maintenance branches are there for **bug fixes only,***
*please submit new features against the development branch with the highest version.*

#### Contributions ✍️

Developers are strongly encouraged to write [unit tests](src/test/README.md) for new code, and to
submit new unit tests for old code. Unit tests can be compiled and run
(assuming they weren't disabled in configure) with: `make check`. Further details on running
and extending unit tests can be found in [/src/test/README.md](/src/test/README.md).

There are also [regression and integration tests](/qa) of the RPC interface, written
in Python, that are run automatically on the build server.
These tests can be run (if the [test dependencies](/qa) are installed) with: `qa/pull-tester/rpc-tests.py`

Changes should be tested by somebody other than the developer who wrote the
code. This is especially important for large or high-risk changes. It is useful
to add a test plan to the pull request description if testing the changes is
not straightforward.

### Wow plz make coingreend/coingreen-cli/coingreen-qt

  The following are developer notes on how to build CoinGreen on your native platform. They are not complete guides, but include notes on the necessary libraries, compile flags, etc.

  - [OSX Build Notes](doc/build-osx.md)
  - [Unix Build Notes](doc/build-unix.md)
  - [Windows Build Notes](doc/build-windows.md)

### Such ports

- RPC 22555
- P2P 22556

## Development tips and tricks

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

Run with the `-testnet` option to run with "play coingreens" on the test network, if you
are testing multi-machine code that needs to operate across the internet.

If you are testing something that can run on one machine, run with the `-regtest` option.
In regression test mode, blocks can be created on-demand; see qa/rpc-tests/ for tests
that run in `-regtest` mode.

**DEBUG_LOCKORDER**

CoinGreen Core is a multithreaded application, and deadlocks or other multithreading bugs
can be very difficult to track down. Compiling with `-DDEBUG_LOCKORDER` (`configure
CXXFLAGS="-DDEBUG_LOCKORDER -g"`) inserts run-time checks to keep track of which locks
are held, and adds warnings to the debug.log file if inconsistencies are detected.
