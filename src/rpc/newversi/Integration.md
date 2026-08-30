# Integrating the index DB + `getindexstats` / `getindexrecord` RPCs

Files in this delivery:

```
db/schema.sql        SQLite schema + seeded demo data (blocks, tx, cross-chain refs)
db/install.sql        (same as schema.sql — some tooling expects this name; symlink or copy)
db/install.py         Python installer (stdlib sqlite3, no deps)
db/install.js         Node.js installer (better-sqlite3)
db/install.sh         Bash installer (sqlite3 CLI)
src/rpc/indexdb.cpp   New RPC command file: getindexstats, getindexrecord
```

All four installers are equivalent — pick whichever matches your tooling.
Each is idempotent (`INSERT OR IGNORE`, `CREATE TABLE IF NOT EXISTS`) so
re-running is safe.

## 1. Initialize the database

```bash
# any one of:
python3 db/install.py --db ~/.dogecoin/dogeindex.sqlite3
node db/install.js --db ~/.dogecoin/dogeindex.sqlite3
./db/install.sh ~/.dogecoin/dogeindex.sqlite3
```

Point it at `<datadir>/dogeindex.sqlite3` to match the default path
`indexdb.cpp` looks for (overridable with `-indexdb=<path>`).

## 2. Wire the RPC command into the build

**`src/rpc/register.h`** — add the declaration and call it from
`RegisterAllCoreRPCCommands`:

```diff
 /** Register raw transaction RPC commands */
 void RegisterRawTransactionRPCCommands(CRPCTable &tableRPC);
+/** Register local index database RPC commands */
+void RegisterIndexRPCCommands(CRPCTable &tableRPC);

 static inline void RegisterAllCoreRPCCommands(CRPCTable &t)
 {
     RegisterBlockchainRPCCommands(t);
     RegisterNetRPCCommands(t);
     RegisterMiscRPCCommands(t);
     RegisterMiningRPCCommands(t);
     RegisterAuxPoWRPCCommands(t);
     RegisterRawTransactionRPCCommands(t);
+    RegisterIndexRPCCommands(t);
 }
```

**`src/Makefile.am`** — add `rpc/indexdb.cpp` alongside the other RPC
sources (around line 213-220):

```diff
   rpc/blockchain.cpp \
   rpc/mining.cpp \
   rpc/misc.cpp \
   rpc/net.cpp \
+  rpc/indexdb.cpp \
   rpc/rawtransaction.cpp \
   rpc/server.cpp \
```

**Link SQLite3.** This codebase doesn't currently depend on libsqlite3, so
add detection to `configure.ac` (near the other `PKG_CHECK_MODULES` /
`AC_CHECK_LIB` calls) and add the resulting flags to `libbitcoin_server_a`
(or equivalent) in `src/Makefile.am`:

```
PKG_CHECK_MODULES([SQLITE3], [sqlite3 >= 3.7.17])
```

then reference `$(SQLITE3_CFLAGS)` / `$(SQLITE3_LIBS)` in the target that
builds `rpc/indexdb.cpp`. On Debian/Ubuntu build hosts the dev package is
`libsqlite3-dev`.

## 3. Try it

```bash
dogecoind -indexdb=$HOME/.dogecoin/dogeindex.sqlite3 -daemon
dogecoin-cli getindexstats
dogecoin-cli getindexrecord a1b2c3d4e5f60718293a4b5c6d7e8f90a1b2c3d4e5f60718293a4b5c6d7e8f9
```

## Notes / scope

- `indexdb.cpp` is **read-only** against the SQLite file — it never writes.
  Population is the installers' job (seed data today; swap in a real
  block-by-block ingester later without touching the RPC surface).
- `external_chain_refs` is schema-ready for Pi Network, Stellar/Soroban,
  Solana, and Ethereum reference rows, so a bridge/watcher process can tag a
  dogecoin txid against an event on any of those chains without a schema
  change.
- Not tested against an actual compiled `dogecoind` in this session (no
  Dogecoin build toolchain in this sandbox) — the SQL and both installer
  scripts (Python/Node) were run and verified against the schema; the C++
  follows this repo's existing `rpc/misc.cpp` conventions exactly (same
  `CRPCCommand` table shape, same help-text format, same registration
  pattern) so it should drop in cleanly, but compile it against your tree
  before merging.
