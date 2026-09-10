Compact block filters (BIP 157/158)
-----------------------------------

This release adds support for compact block filters, allowing light clients to
determine whether a block is relevant to them without downloading the block and
without disclosing the addresses they are interested in to the serving node.

Two new options control the feature, both off by default:

- `-blockfilterindex=<type>` maintains an index of compact filters by block.
  Supplying no `<type>`, or `-blockfilterindex=1`, enables indexes for all known
  filter types (currently `basic`). The index is built in the background and
  becomes available once it has caught up with the chain.
- `-peerblockfilters` serves those filters to peers over the P2P network per
  BIP 157. It requires `-blockfilterindex`; the node refuses to start if it is
  set without one. When enabled the node advertises the `NODE_COMPACT_FILTERS`
  service bit (`1 << 6`) and answers the `getcfilters`, `getcfheaders` and
  `getcfcheckpt` messages with `cfilter`, `cfheaders` and `cfcheckpt`.

Both options are incompatible with pruning; the node refuses to start if either
is combined with `-prune`.

The index stores its data under `indexes/blockfilter/<type>/`: a LevelDB
database in `db/`, and the filters themselves in `fltr?????.dat` flat files.
The flat files hold the bulk of the data. See `doc/files.md`.

New RPCs:

- `getblockfilter` returns the filter and filter header for a given block.
- `getindexinfo` reports the name, sync state and current height of each
  running index.

UTXO set migration
------------------

**This release changes the on-disk chainstate format and is not
backwards-compatible.** The chainstate database moves from one record per
transaction to one record per unspent transaction output.

- **A one-time migration runs automatically on first start.** The node detects a
  legacy chainstate and upgrades it in place before completing startup. The time
  taken is proportional to the size of the UTXO set. No reindex or resync is
  required.
- **Undo files written by this version cannot be read by earlier versions.**
  Undo records now carry transaction metadata for all entries. Once this version
  has connected new blocks, downgrading to an earlier release requires a full
  `-reindex`. Back up your data directory before upgrading if you may need to
  roll back.
- **The block index and block files themselves are unchanged.** Only the
  chainstate and undo data are affected.

RPC changes
-----------

- `gettxoutsetinfo` no longer returns `hash_serialized`. It now returns
  `hash_serialized_2`, computed over the per-txout chainstate. The value is not
  comparable to the old `hash_serialized` for the same block; the field was
  renamed deliberately so that tooling reading the old name fails loudly rather
  than silently comparing incomparable hashes.
- `getpeerinfo` gains an `addrbind` field, reporting the local bind address of
  the connection to each peer.
- `gettxout` no longer returns the `version` field. The per-txout chainstate does
  not store a transaction version, so the value could not be reported.
- `gettxoutsetinfo` no longer returns `bytes_serialized`, removed alongside the
  `hash_serialized_2` rename above for the same reason.
- The REST endpoint `/rest/getutxos` no longer returns `txvers` in its JSON
  output. **The binary format is affected differently and more quietly:** the
  field is retained at its original width but is now always written as zero.
  Nothing shifts position, so a binary consumer will not fail to parse -- it will
  simply read `0` where it previously read a transaction version. JSON consumers
  see a missing key and notice; binary consumers see a plausible value and do
  not. Any tooling reading the binary UTXO format should be checked.
- **Wallet RPCs now wait for the wallet to catch up with the chain before
  running.** Wallet calls invoke `BlockUntilSyncedToCurrentChain()`, so a call
  made while the validation queue is draining will block until it has drained
  rather than returning a result computed against a stale chainstate. This makes
  wallet RPCs consistent with `getblockchaininfo`'s state immediately prior to
  the call, at the cost of added latency in exactly those moments -- most
  visibly just after a block arrives, or while the node is catching up. RPCs
  whose behaviour does not depend on the chainstate do not make the call. See
  `doc/developer-notes.md` for the developer-facing rule.

P2P changes
-----------

- Orphan transaction handling no longer runs under `cs_main`. The orphan pool is
  guarded by a dedicated `g_cs_orphans` mutex, and
  `PeerLogicValidation::BlockConnected` no longer acquires `cs_main`.
