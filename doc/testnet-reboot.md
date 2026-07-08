# Rebooted Dogecoin testnet (testnet4)

Dogecoin Core supports **two** public test networks. Pick one with a command-line flag; you cannot combine them in a single process.

| | Legacy testnet3 | Reboot testnet4 |
|---|-----------------|-----------------|
| **Flag** | `-testnet` | `-testnet4` |
| **Datadir** | `testnet3/` | `testnet4/` |
| **`getblockchaininfo` chain** | `test` | `testnet4` |
| **P2P magic** | `fc c1 b7 dc` | `fd d4 dc e1` |
| **P2P port** | 44556 | 44556 |
| **RPC port** | 44555 | 44555 |
| **Addresses** | `n` / `2` (legacy) | **`T`** prefix |
| **Genesis** | `bb0a7826…` | `b9f29a99…` |
| **Strict min-difficulty ([PR #3967](https://github.com/dogecoin/dogecoin/pull/3967))** | No | Yes (from block 1) |
| **Typical use** | Existing apps, historical chain | New development, fresh chain |

Both networks use the same default **P2P port (44556)** and the same **fixed seed list** from `chainparamsseeds.h`. Magic bytes differ, so nodes only peer with the chain they were started for. **Do not run both on the same machine on port 44556** unless you change `-port` on one of them.

---

## Legacy testnet3 (`-testnet`)

Unchanged behavior for existing testnet users:

```bash
dogecoind -testnet
dogecoin-cli -testnet getblockchaininfo
```

- Data: `…/testnet3/`
- Addresses from `getnewaddress` start with **`n`** (P2PKH)
- DNS seed: `testseed.jrn.me.uk`

## Reboot testnet4 (`-testnet4`)

Fresh chain with block-storm protections, aligned with DogeGo reboot testnet:

```bash
dogecoind -testnet4
dogecoin-cli -testnet4 getnewaddress
```

- Data: `…/testnet4/` (never reuse `testnet3/` blocks)
- Addresses start with **`T`**
- 10,000 DOGE block subsidy from height 1
- Digishield + strict min-difficulty from block 1
- AuxPoW from height 158100
- No DNS seeds yet; use `addnode` or run a founder node

### First node (founder)

```ini
testnet4=1
server=1
listen=1
gen=1
```

```bash
dogecoind -testnet4 -datadir=/path/to/datadir
```

### Join an existing testnet4 network

```ini
testnet4=1
addnode=FOUNDER_HOST:44556
```

Forward TCP **44556** on the founder if peers connect from the internet.

### DogeGo interoperability

DogeGo nodes with `network=testnet` use the same chain identity as Core `-testnet4` (magic, genesis, `T` addresses). Connect with `addnode` on both sides.

---

## Choosing a network

| Goal | Use |
|------|-----|
| Test against the long-running public testnet chain | `-testnet` |
| Start clean development without 30M+ legacy blocks | `-testnet4` |
| Local isolated regression tests | `-regtest` |

---

## Related docs

- [getting-started.md](getting-started.md)
- [files.md](files.md) (datadir layout)
- [PR #3967](https://github.com/dogecoin/dogecoin/pull/3967)
