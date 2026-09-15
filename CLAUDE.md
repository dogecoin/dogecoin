# CLAUDE.md

Guidance for Claude Code (claude.ai/code) and other AI coding agents working in this repository.

## What this project is

Dogecoin Core is the reference full node, wallet and miner for Dogecoin, forked from Bitcoin Core (currently
tracking the 0.14/0.17 era with Dogecoin-specific consensus, AuxPoW merged mining, and scrypt proof of work).

This is consensus-critical financial software. A change that alters how a block or transaction is validated can
fork the network; a change that alters how an amount is represented can misreport money. Treat both as
correctness-critical regardless of how small the diff looks, and prefer a narrow, well-tested change over a clever
one.

## Branch strategy

Per `CONTRIBUTING.md`: the default branch is intentionally a **stable release**. Active development is on
`master`, and **PRs go to `master`**. Check which branch you are on before starting.

## Build

Autotools. `./autogen.sh && ./configure && make`. Full instructions per platform are in `doc/build-*.md`; read the
one matching the target rather than guessing flags.

Useful configure flags when you only need to verify a change:

```bash
./configure --disable-wallet --with-gui=no --disable-bench   # fastest; no Berkeley DB needed
./configure --with-gui=no --with-incompatible-bdb            # wallet + functional tests, any BDB version
```

Functional tests in `qa/rpc-tests` need the wallet, so `--disable-wallet` is only enough for unit tests.

### Building on a modern macOS host

The released macOS binaries are built through `depends/` with a pinned toolchain. Building against system headers
and current Homebrew works, but needs help:

- Boost lives under `/opt/homebrew`, which the bundled autoconf macros do not search. Pass
  `--with-boost=/opt/homebrew` plus `CPPFLAGS=-I/opt/homebrew/include LDFLAGS=-L/opt/homebrew/lib`, or configure
  fails with "Need at least boost 1.60.0" even though Boost is installed.
- Recent macOS SDKs define `le32dec`/`le32enc`/`be32dec`/`be32enc` in `sys/endian.h`, which collide with the
  definitions in `src/crypto/scrypt.{h,cpp}`. Those are guarded only by `#ifndef __FreeBSD__`, so the build fails
  with "redefinition of 'le32dec'". This is a host quirk, not a bug in your change — do not "fix" it as a drive-by
  in an unrelated PR.

## Tests

Two suites, and a change of any substance should touch both:

- **Unit tests** — Boost.Test, in `src/test/`. Build produces `src/test/test_dogecoin`.
  ```bash
  ./src/test/test_dogecoin                       # all
  ./src/test/test_dogecoin --run_test=rpc_tests  # one suite
  ```
- **Functional/RPC tests** — Python, in `qa/rpc-tests/`, driving real `dogecoind` instances on regtest.
  ```bash
  cd qa/rpc-tests && ./sometest.py               # one test
  qa/pull-tester/rpc-tests.py                    # the set CI runs
  ```
  A new test must be added to `testScripts` in `qa/pull-tester/rpc-tests.py` or it will never run.

Notes that will save you time:

- `BitcoinTestFramework.setup_nodes()` here is just `start_nodes(self.num_nodes, self.options.tmpdir)`. It does
  **not** read `self.extra_args`, unlike newer Bitcoin Core. To give nodes different options, override
  `setup_nodes` and pass `extra_args=[...]` to `start_nodes` yourself.
- Overriding `setup_network` means you are responsible for connecting nodes (`connect_nodes_bi`) — without it the
  nodes never sync and you get a "Block sync ... timed out" failure.
- The RPC proxy parses JSON numbers as `decimal.Decimal` and leaves JSON strings as `str`, so the *wire type* of a
  value is observable as its Python type.
- Several older tests import `asyncore` via `test_framework/mininode.py`, which was removed in Python 3.12. Those
  tests fail at import on a modern interpreter regardless of your change; confirm against a clean tree before
  assuming you broke something.

## Money and precision

`CAmount` is an `int64_t` count of **koinu** (1 DOGE = 1e8 koinu). Do arithmetic on amounts in koinu, never in
floating point.

The part that bites: Dogecoin's supply is large enough that an amount in koinu routinely exceeds 2^53, the point
past which an IEEE-754 double can no longer represent every integer. 2^53 koinu is about 90,071,992.55 DOGE, so
any consumer that funnels an amount through a double — which includes every JSON parser that decodes numbers as
doubles — can silently be off by a koinu or two. Bitcoin never hits this because its whole supply is under 2^53
satoshi.

`ValueFromAmount` in `src/rpc/server.cpp` therefore formats amounts as exact decimal text rather than emitting a
double, and `AmountFromValue` parses with `ParseFixedPoint`, accepting both JSON numbers and JSON strings. Keep
both properties when touching that code.

## Conventions

- Follow `doc/developer-notes.md` for style; match the surrounding file, which is mostly Bitcoin Core's style.
- Commits are expected to be focused and individually reviewable; keep unrelated fixes out of a PR, even small ones.
- New RPC behaviour that changes output shape should be opt-in behind an option, since integrators depend on the
  existing shape.
