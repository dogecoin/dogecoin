# ScrapCoin Mainnet Launch Guide

Complete step-by-step instructions to compile, configure, and launch ScrapCoin mainnet with wallet and mining capabilities.

## Table of Contents
1. [Prerequisites](#prerequisites)
2. [Compilation](#compilation)
3. [Initial Setup](#initial-setup)
4. [Wallet Creation](#wallet-creation)
5. [Mining](#mining)
6. [Verification](#verification)
7. [Operations](#operations)
8. [Troubleshooting](#troubleshooting)

---

## Prerequisites

### Ubuntu/Debian

```bash
sudo apt-get update
sudo apt-get install -y \
    build-essential \
    libtool \
    autotools-dev \
    automake \
    pkg-config \
    bsdmainutils \
    python3 \
    libevent-dev \
    libboost-system-dev \
    libboost-filesystem-dev \
    libboost-test-dev \
    libboost-thread-dev \
    libssl-dev \
    libdb4.8-dev \
    libdb4.8++-dev \
    libminiupnpc-dev
```

### macOS

```bash
brew install automake libtool boost openssl berkeley-db4 miniupnpc
```

### CentOS/RHEL/Fedora

```bash
sudo yum install -y \
    gcc-c++ \
    make \
    autoconf \
    automake \
    libtool \
    boost-devel \
    openssl-devel \
    db4-devel \
    db4-cxx-devel \
    libevent-devel \
    miniupnpc-devel
```

---

## Compilation

```bash
# 1. Navigate to the repository
cd /vercel/sandbox/repos/dogecoin

# 2. Ensure you're on the scrapcoin-fork branch
git checkout scrapcoin-fork

# 3. Generate the build system
./autogen.sh

# 4. Configure the build
./configure --disable-tests --disable-bench --without-miniupnpc

# Note: If you don't have Berkeley DB and don't need wallet functionality:
# ./configure --disable-tests --disable-bench --without-miniupnpc --disable-wallet

# 5. Compile (using all CPU cores)
make -j$(nproc)

# 6. The binaries are now in src/
ls -lh src/scrapcoin*
```

Expected output:
```
src/scrapcoind
src/scrapcoin-cli
src/scrapcoin-qt (if GUI enabled)
src/scrapcoin-tx
```

---

## Initial Setup

### Create Data Directory

```bash
# Create the ScrapCoin data directory
mkdir -p ~/.scrapcoin

# Verify it was created
ls -la ~/.scrapcoin
```

### Create Configuration File

```bash
# Generate a secure random RPC password
RPC_PASS=$(openssl rand -hex 16)

# Create scrapcoin.conf
cat > ~/.scrapcoin/scrapcoin.conf << EOF
# ScrapCoin Configuration File
# Created: $(date)

# RPC Settings
rpcuser=scrapcoinrpc
rpcpassword=$RPC_PASS
rpcport=43889

# Node Settings
server=1
listen=1
port=43888
txindex=1

# Optional: Enable mining (see Mining section)
# gen=1
# genproclimit=-1

# Optional: Add peers (once network is live)
# addnode=seed.scrapcoin.org:43888
# addnode=node.scrapcoin.org:43888

# Optional: Logging
# debug=net
# debug=mining
# debug=wallet
EOF

# Display the config
cat ~/.scrapcoin/scrapcoin.conf
```

**Important:** Save the `rpcpassword` somewhere secure. You'll need it for RPC calls.

---

## Wallet Creation

### Start the Daemon

```bash
# Start scrapcoind in daemon mode
src/scrapcoind -daemon

# Wait 2-3 seconds for initialization
sleep 3

# Check if the daemon is running
src/scrapcoin-cli getblockchaininfo
```

Expected output (JSON):
```json
{
  "chain": "main",
  "blocks": 0,
  "headers": 0,
  ...
}
```

### Create or Load Wallet

```bash
# List existing wallets (v1.14+)
src/scrapcoin-cli listwallets

# If wallet directory is empty, create a new wallet
src/scrapcoin-cli createwallet "wallet.dat"

# Or with encryption (recommended):
src/scrapcoin-cli createwallet "wallet.dat" true "" "" "your strong passphrase"
```

### Get a Receiving Address

```bash
# Get a new address from your wallet
ADDRESS=$(src/scrapcoin-cli getnewaddress)
echo "Your ScrapCoin address: $ADDRESS"
```

The address will start with **S** (e.g., `Sxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx`).

### Check Wallet Balance

```bash
src/scrapcoin-cli getbalance
# Output: 0.00000000 (initially)
```

---

## Mining

### Understanding Mining Rewards

- **Block reward:** 50 SCRAP per block
- **Maturity:** 30 blocks (rewards cannot be spent until 30 confirmations)
- **Target spacing:** 1 minute (average)
- **Difficulty:** Adjusts via DigiShield/AuxPoW

### Enable Mining

**Option 1: Via Configuration File** (Recommended for continuous mining)

```bash
# Edit your config
nano ~/.scrapcoin/scrapcoin.conf

# Add these lines:
gen=1
genproclimit=-1  # Use all available CPU cores

# Or limit to specific number of cores:
# genproclimit=4

# Restart the daemon
src/scrapcoin-cli stop
src/scrapcoind -daemon
```

**Option 2: Via RPC** (For on-demand mining)

```bash
# First, unlock the wallet if encrypted
src/scrapcoin-cli walletpassphrase "your_passphrase" 999999999

# Start mining (if wallet is set as default)
# Note: The 'generate' RPC is available in regtest, but for mainnet
# you need to run the daemon with gen=1 or use an external miner
```

**Option 3: External Miner** (cpuminer, cgminer, etc.)

```bash
# Install cpuminer
git clone https://github.com/pooler/cpuminer.git
cd cpuminer
./autogen.sh
./configure CFLAGS="-O3" --disable-aes-ni
make -j$(nproc)

# Run miner (connecting to your local node)
./minerd -a scrypt -o http://127.0.0.1:43889 -u scrapcoinrpc -p $RPC_PASS
```

### Generate Blocks (Regtest Mode for Testing)

For quick testing without waiting for mainnet blocks:

```bash
# Stop the mainnet daemon
src/scrapcoin-cli stop

# Start regtest mode (instant block generation)
src/scrapcoind -regtest -daemon

# Generate 101 blocks instantly (mempool requirement)
src/scrapcoin-cli -regtest generate 101

# Check balance (101 * 50 SCRAP = 5050 SCRAP, but only 71 spendable after maturity)
src/scrapcoin-cli -regtest getbalance
# Output: 5050.00000000

# Check block height
src/scrapcoin-cli -regtest getblockcount
# Output: 101
```

### Mining on Mainnet

On mainnet, blocks are mined by solving the PoW puzzle:

```bash
# With gen=1 in config, the node will automatically mine when connected to network
# Check mining status
src/scrapcoin-cli getmininginfo

# Expected output:
# {
#   "blocks": 0,
#   "currentblocksize": 0,
#   "currentblocktx": 0,
#   "difficulty": 1.00000000,
#   "generate": true,
#   "genproclimit": -1,
#   "hashespersec": 0.00000000,
#   "networkhashps": 0,
#   "pooledtx": 0
# }

# Monitor block generation
watch -n 5 src/scrapcoin-cli getblockcount

# Or check the debug log
tail -f ~/.scrapcoin/debug.log | grep -i "block"
```

---

## Verification

### Verify Genesis Block

```bash
# Get the genesis block hash
src/scrapcoin-cli getblockhash 0
# Should return: 0000083e11af168c492eb117c61c5403bdc12675cb86a3346923cc72b83c29ce

# Get genesis block details
src/scrapcoin-cli getblock 0
```

### Verify Network Parameters

```bash
# Check network info
src/scrapcoin-cli getnetworkinfo | grep -E '"version"|"subversion"|"connections"|"network"'

# Check blockchain info
src/scrapcoin-cli getblockchaininfo | grep -E '"chain"|"blocks"|"difficulty"|"verificationprogress"'
```

### Verify Address Format

```bash
# Generate multiple addresses and verify they start with 'S'
for i in {1..5}; do
    src/scrapcoin-cli getnewaddress | head -c 1
done
# All should output: S
```

### Verify Magic Bytes

Check that the node uses ScrapCoin magic bytes (0xfaceb00c):

```bash
# In the debug log, look for network messages
grep "magic" ~/.scrapcoin/debug.log
```

---

## Operations

### Check Status

```bash
# General blockchain info
src/scrapcoin-cli getblockchaininfo

# Network info
src/scrapcoin-cli getnetworkinfo

# Mining info
src/scrapcoin-cli getmininginfo

# Peer info
src/scrapcoin-cli getpeerinfo

# Mempool info
src/scrapcoin-cli getmempoolinfo
```

### Wallet Operations

```bash
# List all addresses
src/scrapcoin-cli listaddressgroupings

# List transactions
src/scrapcoin-cli listtransactions "*" 1000

# Get balance
src/scrapcoin-cli getbalance

# Get balance by address
src/scrapcoin-cli getaddressesbylabel ""

# Send SCRAP (amount in SCRAP, not scraplets)
src/scrapcoin-cli sendtoaddress "Sxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx" 10.0

# Check transaction status
src/scrapcoin-cli gettransaction "<txid>"

# List unspent outputs
src/scrapcoin-cli listunspent
```

### Block Operations

```bash
# Get current block height
src/scrapcoin-cli getblockcount

# Get block hash by height
src/scrapcoin-cli getblockhash 100

# Get block by hash
src/scrapcoin-cli getblock "<blockhash>"

# Get block by height
src/scrapcoin-cli getblock "$(src/scrapcoin-cli getblockhash 100)"
```

### Mining Control

```bash
# Stop mining (edit config or use RPC if available)
# For regtest, you can simply stop generating

# Check mining stats
src/scrapcoin-cli getmininginfo

# Get difficulty
src/scrapcoin-cli getdifficulty
```

---

## Troubleshooting

### Daemon Won't Start

```bash
# Check if port is already in use
sudo netstat -tulpn | grep 43888

# Kill any existing process
pkill scrapcoind
sleep 2

# Check data directory permissions
ls -la ~/.scrapcoin/
chmod 700 ~/.scrapcoin/

# Try starting with debug output
src/scrapcoind -printtoconsole -debug=net
```

### RPC Connection Failed

```bash
# Verify daemon is running
ps aux | grep scrapcoind

# Check RPC credentials in config
cat ~/.scrapcoin/scrapcoin.conf | grep rpc

# Test RPC connection with curl
curl --user scrapcoinrpc:$RPC_PASS \
     -d '{"jsonrpc":"1.0","id":"curl","method":"getblockchaininfo","params":[]}' \
     -H 'content-type: text/plain;' \
     http://127.0.0.1:43889/
```

### Wallet Issues

```bash
# Wallet not found
src/scrapcoin-cli listwallets
# If empty, create one:
src/scrapcoin-cli createwallet "wallet.dat"

# Wallet locked
src/scrapcoin-cli walletpassphrase "yourpass" 999999

# Rescan blockchain (if transactions missing)
src/scrapcoin-cli stop
src/scrapcoind -daemon -rescan
```

### No Blocks Being Mined

```bash
# Check mining status
src/scrapcoin-cli getmininginfo

# Ensure gen=1 in config
grep gen ~/.scrapcoin/scrapcoin.conf

# Check difficulty (may be too high initially)
src/scrapcoin-cli getdifficulty

# For solo mining on new network, difficulty starts low but may need patience
# Consider mining in regtest for testing first
```

### Sync Stuck

```bash
# Check peers
src/scrapcoin-cli getpeerinfo
# Should show at least 1 connection

# If no peers, add manually:
src/scrapcoin-cli addnode "seed.scrapcoin.org:43888" "onetry"

# Check debug log
tail -f ~/.scrapcoin/debug.log | grep -i "error\|warning\|peer"

# Reindex if needed
src/scrapcoin-cli stop
src/scrapcoind -daemon -reindex
```

### Compilation Errors

```bash
# Clean and rebuild
make clean
./configure --disable-tests --disable-bench --without-miniupnpc
make -j$(nproc)

# Check dependencies
pkg-config --exists db4 && echo "Berkeley DB found" || echo "Berkeley DB missing"
pkg-config --exists boost && echo "Boost found" || echo "Boost missing"

# If Boost version too low, check:
pkg-config --modversion boost
# Need at least 1.60.0
```

---

## Quick Reference

### Essential Commands

```bash
# Start/stop daemon
src/scrapcoind -daemon
src/scrapcoin-cli stop

# Wallet
src/scrapcoin-cli getnewaddress          # New address
src/scrapcoin-cli getbalance             # Balance
src/scrapcoin-cli listtransactions       # Transaction history
src/scrapcoin-cli sendtoaddress <addr> <amount>  # Send

# Blockchain
src/scrapcoin-cli getblockcount          # Current height
src/scrapcoin-cli getblockhash <height>  # Block hash
src/scrapcoin-cli getblock <hash/height> # Block details

# Mining (regtest)
src/scrapcoind -regtest -daemon
src/scrapcoin-cli -regtest generate <n>

# Network
src/scrapcoin-cli getpeerinfo            # Connected peers
src/scrapcoin-cli getnetworkinfo         # Network stats
```

### File Locations

- **Data directory:** `~/.scrapcoin/`
- **Config file:** `~/.scrapcoin/scrapcoin.conf`
- **Wallet file:** `~/.scrapcoin/wallet.dat` (or `wallets/` for multiwallet)
- **Blocks:** `~/.scrapcoin/blocks/`
- **Chainstate:** `~/.scrapcoin/chainstate/`
- **Debug log:** `~/.scrapcoin/debug.log`
- **Peers:** `~/.scrapcoin/peers.dat`

### Ports

- **Mainnet P2P:** 43888
- **Mainnet RPC:** 43889
- **Testnet P2P:** 43890
- **Testnet RPC:** 43891
- **Regtest P2P:** 18444
- **Regtest RPC:** 43892

---

## Success Criteria

After following this guide, you should have:

- ✅ ScrapCoin daemon running on mainnet
- ✅ Wallet with at least one address (starting with 'S')
- ✅ Ability to mine blocks (or have mined test blocks in regtest)
- ✅ Understanding of basic operations (check balance, send transactions)
- ✅ Network isolated from Dogecoin
- ✅ Independent blockchain with genesis block height 0

## Next Steps

1. **Deploy seed nodes** at seed.scrapcoin.org and node.scrapcoin.org
2. **Add checkpoints** at strategic block heights after network stabilizes
3. **Create block explorer** for public blockchain viewing
4. **Develop wallet software** (mobile, desktop, web)
5. **Establish mining pools** to decentralize mining
6. **List on exchanges** for trading
7. **Build community** and developer ecosystem

---

## Support

- **Documentation:** See `SCRAPCOIN_DELIVERABLES.md`
- **Issues:** Report on GitHub repository
- **Community:** Create Discord/Telegram/Forum for ScrapCoin

**Welcome to ScrapCoin - The Decentralized Recycling Economy!** ♻️