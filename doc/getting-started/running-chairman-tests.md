## Purpose

The chairman tests are a set of tests run only in CI to ensure that a static
set of nodes are able to join a cluster and make progress forging new blocks.

These tests are paramterised over operating systems:

* Linux
* MacOS
* Windows

and various run-modes:

* Byron only
* Shelley only
* Byron-Shelley

### Prerequisites
You must already be able to build everything in the `cardano-node` repository
using `cabal` (not `nix`).

See [Installing a node from source](install.md).

## Running the chairman test
The chairman test will run some testnets and perform some basic tests to ensure they
are working.

Those basic tests may differ depending on the testnet that is being tested, but may
include for example:

Asserting that each node:

* Opens the correct port
* Creates a socket file (or OS-specific equivalent) at the expected location

To see what assertions are made, please see the source code of the testnet you are
interested in.  These can be found in `cardano-node-chairman/src`.

To run the chairman tests, first build the necessary executables:

```bash
$ cabal build cardano-cli cardano-node cardano-node-chairman cardano-testnet
```

Then run the tests:

```bash
cabal test cardano-node-chairman
```

## Design

The testnet launcher and chairman test use common code under the `cardano-node-chairman/src` directory
to launch any one of multiple testnets.

This section will discuss the overall structure of this common design.

The testnet infrastructure will create a workspace directory with a random suffix to ensure that if multiple
testnets are launched, they will not interfere with each other via the filesystem.  All the configuration files,
logging output, and socket files will be written to to files somewhere in the workspace.  The
actual workspace location will be logged in the test output.

The nodes themselves (and the chairman executable in the case of the chairman tests) will be executed from
the parent directory of the workspace directory.  This is to work around operating system limitations such
as filename restrictions whilst still ensuring that the names of socket files (or in the case of Windows,
named pipes) remain unique.

The infrastructure uses hedgehog to provide annotations as an alternative to logging.  This allows the configuration
and logging to be printed close to the source code that generated it, making it easy to follow the testnet
set up and diagnose any potential issues.

A downside to doing it this way is all the output is collected whilst the testnet is being brought up, but
no output will be printed until the testnet is fully launched or a failure occurs.

For further details information see [Testing Cardano with Hedgehog](https://youtu.be/ZAN18xZGsSY), keeping in
mind the following correction, which is when the chairman test is run, one chairman process is created for the
entire testnet and connects to each and every node.
