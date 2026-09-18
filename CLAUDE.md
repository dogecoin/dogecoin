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

### Building the Qt GUI

`--with-gui=no` is the fast path, but a change under `src/qt/` is only really checked by building it: `.ui` files
are compiled by `uic` and `Q_OBJECT` classes by `moc`, so a renamed widget or a slot that was declared but never
defined fails at build time and nowhere else.

On macOS, Homebrew's Qt 5 is keg-only, so `configure` only finds it through `pkg-config`:

```bash
brew install qt@5
export PATH="/opt/homebrew/opt/qt@5/bin:$PATH"
export PKG_CONFIG_PATH="/opt/homebrew/opt/qt@5/lib/pkgconfig:$PKG_CONFIG_PATH"
./configure --with-gui=qt5 --with-incompatible-bdb --with-boost=/opt/homebrew
```

Check that the configure summary says `with gui / qt = yes`. If Qt is not found configure does **not** fail — it
builds the daemon only, and the GUI change is never compiled. `protobuf` and `qrencode` are optional (payment
requests and QR codes); without them the GUI builds with those features off.

The GUI has its own test binary, `src/qt/test/test_dogecoin-qt`, whose tests are registered by hand in
`src/qt/test/test_main.cpp`. `QT_QPA_PLATFORM=minimal` runs it without a display.

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
- Adding a new test file to `src/Makefile.test.include` is not enough on its own: the generated `src/Makefile` is
  stale until `automake src/Makefile && ./config.status src/Makefile` runs, and until then the new suite silently
  does not exist. `make` reports success and the test binary simply does not contain it.
- Several older tests import `asyncore` via `test_framework/mininode.py`, which was removed in Python 3.12. Those
  tests fail at import on a modern interpreter regardless of your change; confirm against a clean tree before
  assuming you broke something. The auxpow tests need the `ltc_scrypt` module and fail the same way without it.
  Between them that is around 25 of the tests `rpc-tests.py` runs, so a large block of red on a modern host is
  expected rather than alarming — check that every failure is a `ModuleNotFoundError` at import, and treat any
  failure that is *not* one as yours.
- `rpc-tests.py --coverage` reports a method as uncovered when no test *called* it, which includes every method
  whose only test died at import. On a host missing `asyncore` it will claim `getconnectioncount` and
  `getmininginfo` are uncovered when `setmaxconnections.py` and `p2p-versionbits-warning.py` cover them. To tell a
  genuine gap from a skipped test, grep for a call site (`\.methodname(`) as well.
- A test whose nodes all run with `-prune` cannot sync them. A pruned node does not advertise `NODE_NETWORK`, so it
  is never a preferred download peer, and `sync_all()` after a `generate()` times out. `blockchain.py` is like
  this: its `setup_network` calls `sync_all()` only while the pair is already at the same height, which succeeds
  trivially. Ask everything of the node that mined, or give a test that genuinely needs to sync an unpruned node.

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

## Wallet change and fees

Two thresholds decide whether a transaction gets a change output, and they are not the same number:

- `CWallet::discardThreshold` (`-discardthreshold`, default 0.01 DOGE) is the point below which `CreateTransaction`
  **absorbs leftover value into the fee** rather than creating the output: `if (newTxOut.IsDust(discardThreshold))`
  then `nFeeRet += nChange` and there is no change output. It is validated at startup to be at least the hard dust
  limit, so it is never zero.
- `CWallet::GetMinChange()` is `discardThreshold + minTxFee.GetFeePerK() * MIN_CHANGE_FEE_MULTIPLIER`, a larger
  number that coin selection uses to decide which outputs are worth considering and to steer away from leaving
  awkwardly small change.

So a "no change output" outcome depends on `discardThreshold`, not on `GetMinChange()`, and code that wants to
guarantee no change output has to stay under the former. `src/qt/coincontroldialog.cpp` and the fee-bumping path
both read these, so a change to either one has reach beyond `wallet.cpp`.

`CreateTransaction` is a loop: it selects coins for `nValue + nFeeRet`, sizes the transaction, and goes round again
with a larger `nFeeRet` until the fee covers the size. Anything that changes which coins are selected changes the
transaction's size and therefore feeds back into the next iteration, so verify such a change through
`CreateTransaction` (`wallet.py`, `fundrawtransaction.py`) and not only through `SelectCoins*` in isolation.

`ApproximateBestSubset` is randomised, so the coin-selection unit tests repeat themselves `RUN_TESTS` times and
some are allowed to fail a fraction of the time (`RANDOM_REPEATS`). A single green run of those tests means less
than it looks; a single red one may be luck. Run them repeatedly before concluding either way.

## GUI options

A user-visible setting is five edits, and missing one fails silently rather than loudly:

1. `src/qt/optionsmodel.h` — an `OptionID` enum entry, a private member, a getter.
2. `src/qt/optionsmodel.cpp` — a default in `Init()`, a `case` in `data()`, a `case` in `setData()`.
3. `src/qt/forms/optionsdialog.ui` — the widget.
4. `src/qt/optionsdialog.cpp` — `mapper->addMapping(...)`; without it the widget neither loads nor saves.
5. `setRestartRequired(true)` in `setData()` *only* if the value is consumed once at startup. An option read at
   the point of use takes effect immediately and should not ask for a restart.

Settings persist in `QSettings` under a string key, not under the enum value, so inserting an entry in the middle
of `OptionID` is safe. Put defaults in `Init()` behind `if (!settings.contains(...))` so an existing user's choice
survives an upgrade, and choose the default that preserves current behaviour.

`OptionsModel::Reset()` is not a clean slate for tests: it also touches the login-item registration and leaves
`fReset` behind. A test wanting known state should remove the specific keys it cares about in `init()`/`cleanup()`
and construct `OptionsModel` normally.

User-visible strings go through `tr()` or live in the `.ui`; both are extracted for translation. Do not hard-code
a string in a `.ui` when its wording depends on another setting — set it from code and update it on that
setting's change signal, or translators get a sentence that is only true half the time.

## Block versions and AuxPoW

`nVersion` is not a plain integer here. `CPureBlockHeader` splits it three ways:

| bits  | meaning                                                       |
|-------|---------------------------------------------------------------|
| 31-16 | merge-mining chain ID — `0x0062` on every network             |
| 8     | `VERSION_AUXPOW`, set when the block carries an auxpow         |
| 7-0   | base version, what BIP34/66/65 compare against                 |

A mainnet block is therefore `0x00620004`, and `GetBaseVersion()` is `nVersion % 256`. Comparing `nVersion`
directly against 2/3/4 the way upstream Bitcoin Core does is wrong here; use `GetBaseVersion()`. `GetChainId()` is
`nVersion >> 16` and is compared for equality against `nAuxpowChainId`, not masked.

This layout collides head-on with BIP9, which puts `001` in the top three bits and hands bits 0-28 to soft-fork
deployments. Read as AuxPoW, a BIP9 version has chain ID `0x2000` and base version `0`, so a block carrying one is
rejected outright. Consequences worth knowing before touching anything version-related:

- `ComputeBlockVersion()` is deliberately commented out in `CreateNewBlock()` behind a FIXME. It is not an
  oversight and uncommenting it produces blocks this chain rejects.
- Versionbits signalling always counts zero, because a well-formed Dogecoin block never matches
  `VERSIONBITS_TOP_BITS`. Soft-forks cannot currently be deployed by BIP9 here.
- Bit 8 can never be used by a deployment: it is the auxpow flag, so every signalling block would claim an auxpow
  it does not carry.

Issue #1340 tracks reconciling the two. It is parked, and the fork it describes is a maintainer decision.

## Conventions

- Follow `doc/developer-notes.md` for style; match the surrounding file, which is mostly Bitcoin Core's style.
- Commits are expected to be focused and individually reviewable; keep unrelated fixes out of a PR, even small ones.
- New RPC behaviour that changes output shape should be opt-in behind an option, since integrators depend on the
  existing shape.
