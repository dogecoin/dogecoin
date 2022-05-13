## Getting started

This tutorial will help you to go through the basics to use Dogecoin Core after you completed the [installation instructions](/INSTALL.md). You now have `dogecoind` or `dogecoin-qt` executables available to run a node, and `dogecoin-cli`/`dogecoin-tx` tools to help you transact DOGE.

> **Note:** For simplicity, this guide assumes that executables can be found under the `PATH` environment variable.
If needed, you can specify their location by typing `PATH=$PATH:/path/to/executables`, or prepend the full path to the command like:
> ```console
> shibetoshi:~$ /path/to/dogecoin-cli [arguments ...]
> ```

### Table of contents

1. [Starting a dogecoin node](#starting-a-dogecoin-node)
2. [Introduction to the JSON-RPC API](#introduction-to-the-json-rpc-api)
    * [Creating a wallet](#creating-a-wallet)
    * [Verifying your balance](#verifying-your-balance)
    * [Sending transactions](#sending-transactions)
    * [Inspecting blocks and transactions](#inspecting-blocks-and-transactions)
3. [Node configuration](#node-configuration)
    * [Mainnet, testnet and regtest](#mainnet-testnet-and-regtest)
    * [Data directory](#data-directory)
    * [RPC credentials](#rpc-credentials)
    * [Ports](#ports)
    * [Memory](#memory)
    * [Troubleshooting](#troubleshooting)

## Starting a Dogecoin node

To start your node, you can run an headless server using `dogecoind`:
```console
shibetoshi:~$ dogecoind -daemon
```

Or you can use the Graphical User Interface (GUI), `dogecoin-qt`:
```console
shibetoshi:~$ dogecoin-qt
```

Detailed logging is recorded in `debug.log`, located in the [data directory](#data-directory).
*Use `-help` to see all available options for each executable.*

Your node is now running and starts with a *synchronization process* that downloads the entire blockchain from other nodes. This operation will take many hours to complete, but you are now part of the Dogecoin network!

> **Note:** The rest of this guide assumes the use of an headless node. The RPC server is not exposed with `dogecoin-qt` until you activate the `-server` option as a startup argument, but inside the GUI application, you can use all the commands explored below (without `dogecoin-cli`) by going to `Help -> Debug window` and inside the popup window selecting the tab `Console`.

## Introduction to the JSON-RPC API

Dogecoin Core exposes a JSON-RPC interface that allows you to request information about the network, blockchain and individual transactions, send transactions to the networks and manage your wallet.

The Dogecoin Core installation provides the `dogecoin-cli` tool to interact with the JSON-RPC from the command line, and the interface is exposed over HTTP on port `22555`, so that other tools and libraries can interact with it.

To have an overview of the available commands, use the `help` command:

```console
#List all commands
shibetoshi:~$ dogecoin-cli help

#Get help for a specific command
shibetoshi:~$ dogecoin-cli help COMMAND
```

Some commands are different, but it's possible to use the [bitcoin RPC API documentation](https://developer.bitcoin.org/reference/rpc/).

### Creating a wallet

To receive DOGE, you need an address that is securely derived from a private key through a series of automatic, cryptographic operations. The *address* can be shared with anyone to receive DOGE, but the *private key* is  sensitive information that allows anyone that knows it to spend the DOGE on the associated address.

By default, the Dogecoin Core software will automatically create an address for you and securely store the private key in the wallet file.

You can list wallet addresses using `getaddressesbyaccount`:

```console
shibetoshi:~$ dogecoin-cli getaddressesbyaccount ""
[
  "DA2fBazU8Y4epNJ2fQRZCcWpxKZY9HrhLN"
]
```

Using `getnewaddress` will generate a new wallet address:
```console
shibetoshi:~$ dogecoin-cli getnewaddress
DNnGtXk9khadE7EKCmQzxjnehenX92PKAv
```

Private keys are stored in the `wallet.dat` file. You can use `backupwallet` to save a copy:

```console
shibetoshi:~$ dogecoin-cli backupwallet /path/of/wallet/backup
```

**Tip:** Dogecoin addresses start with the letter `D`.

You now have two wallet addresses to share with other people to receive DOGE! Consider avoiding [address reuse](https://en.bitcoin.it/wiki/Address_reuse) for anonymity and security reasons.

### Verifying your balance

The total balance of all addresses held in your wallet can be found with the `getbalance` command.

```console
#Syntax
shibetoshi:~$ dogecoin-cli getbalance "*" minconf
```

`minconf` stands for minimum confirmations.
For example, to see current balance with transaction having at least 5 confirmations:

```console
shibetoshi:~$ dogecoin-cli getbalance "*" 5
421.552000
```

### Sending transactions

Dogecoin implements the [Unspent Transaction Output (UTXO)](https://en.wikipedia.org/wiki/Unspent_transaction_output) model to track which amounts of coin belong to an address. Owning DOGE means that you know the private key(s) to addresses that are associated with unspent outputs. To spend them, you have to compose a new transaction that spends the value from currently unspent outputs to new outputs.

##### sendtoaddress

It's possible to use a single command to create, sign and send a transaction :
```console
#Syntax
shibetoshi:~$ dogecoin-cli sendtoaddress address amount

#Example
shibetoshi:~$ dogecoin-cli sendtoaddress nWSYUqtimF7B6qW4GBdczaG6jvqKutS1Nh 420
```

So much spending power !

Alternatively, four commands are needed to manually create a transaction: `listunspent`, `createrawtransaction`, `signrawtransaction` and `sendrawtransaction`.

##### listunspent

This displays a list of UTXOs associated to addresses kept in the wallet.

```console
#Syntax
shibetoshi:~$ dogecoin-cli listunspent minconf maxconf '["address", ...]'

#Example
shibetoshi:~$ dogecoin-cli listunspent 1 9999999 '["nnJDY1xCRgWQc7vBXHUPMPsEynuZW23Y3P"]'
[
  {
    "txid": "b869ed6606d52e6446dc12db02cf868ab693dd5b9f661116269536f0f8fa2433",
    "vout": 0,
    "address": "nnJDY1xCRgWQc7vBXHUPMPsEynuZW23Y3P",
    "account": "",
    "scriptPubKey": "76a914c6977da37560e1432c2e14e16952981a4c272cac88ac",
    "amount": 100.00000000,
    "confirmations": 1355,
    "spendable": true,
    "solvable": true
  }
]
```

The `minconf` and `maxconf` parameters filter the minimum and maximum number of [confirmations](https://www.pcmag.com/encyclopedia/term/bitcoin-confirmation) of the UTXO returned.

> **Note:** The example address starts with `n` instead of `D`, because it uses [testnet](#mainnet-testnet-and-regtest).

##### createrawtransaction

You can now build a new transaction using the available UTXOs from above.

```console
#Syntax
shibetoshi:~$ utxos_to_use='
  [
    {
      "txid": "id",
      "vout": n
    },
    ...
  ]'
shibetoshi:~$ dogecoin-cli createrawtransaction "$utxos_to_use" '{"address":amount, ...}'

#Example
shibetoshi:~$ utxos_to_use='
[
  {
    "txid": "b869ed6606d52e6446dc12db02cf868ab693dd5b9f661116269536f0f8fa2433",
    "vout": 0
  }
]'
shibetoshi:~$ dogecoin-cli createrawtransaction "$utxos_to_use" '{"nWSYUqtimF7B6qW4GBdczaG6jvqKutS1Nh":69, "nnJDY1xCRgWQc7vBXHUPMPsEynuZW23Y3P": 30.999}'
01000000013324faf8f03695261611669f5bdd93b68a86cf02db12dc46642ed50666ed69b80000000000ffffffff0200a5459b010000001976a91418a89ee36293f15c4db4c01173babd579243161188ac60b8c4b8000000001976a914c6977da37560e1432c2e14e16952981a4c272cac88ac00000000
```

You can combine multiple UTXO and send it to multiple recipients by extending the `utxos_to_use` and recipient JSON structures.

> **Tip:** The transaction returned is encoded in hexadecimal encoding. You can use `dogecoin-cli decoderawtransaction` or `dogecoin-tx -json` to convert the content to JSON format.

##### signrawtransaction

Before sending a transaction, it must be signed by the private key that the address was derived from. Dogecoin Core will automatically use the correct private key when spending UTXO known to the wallet.

```console
#Syntax
shibetoshi:~$ dogecoin-cli signrawtransaction encoded_transaction

#Example
shibetoshi:~$ dogecoin-cli signrawtransaction "01000000013324faf8f03695261611669f5bdd93b68a86cf02db12dc46642ed50666ed69b80000000000ffffffff0200a5459b010000001976a91418a89ee36293f15c4db4c01173babd579243161188ac60b8c4b8000000001976a914c6977da37560e1432c2e14e16952981a4c272cac88ac00000000"
{
  "hex": "01000000013324faf8f03695261611669f5bdd93b68a86cf02db12dc46642ed50666ed69b8000000006a47304402200e1bf722d4335179de170f7c762755b463b3f7b8f026f30950f701bc834f0e6e022036295fdd5e607ca41c4e0e62e59d0911b607bfabedde2424665ffae13564d0e001210388f8f226d12eccd3ba93c1454ec4498b065cea96e29b918fbdb517872ebbf581ffffffff0200a5459b010000001976a91418a89ee36293f15c4db4c01173babd579243161188ac60b8c4b8000000001976a914c6977da37560e1432c2e14e16952981a4c272cac88ac00000000",
  "complete": true
}
```

##### sendrawtransaction

Finally, broadcast the transaction to the network so that it can be included in a block by miners:

```console
#Syntax
shibetoshi:~$ dogecoin-cli sendrawtransaction signed_transaction

#Example
shibetoshi:~$ dogecoin-cli sendrawtransaction 01000000013324faf8f03695261611669f5bdd93b68a86cf02db12dc46642ed50666ed69b8000000006a47304402200e1bf722d4335179de170f7c762755b463b3f7b8f026f30950f701bc834f0e6e022036295fdd5e607ca41c4e0e62e59d0911b607bfabedde2424665ffae13564d0e001210388f8f226d12eccd3ba93c1454ec4498b065cea96e29b918fbdb517872ebbf581ffffffff0200a5459b010000001976a91418a89ee36293f15c4db4c01173babd579243161188ac60b8c4b8000000001976a914c6977da37560e1432c2e14e16952981a4c272cac88ac00000000
b4fae2a43cb35f8016a547e9658e061f1da4a043efafecc42f739d46d95dee21
```

### Inspecting blocks and transactions

Blocks and transactions are identified by unique *hashes*.
Let's find the *[coinbase transaction](https://www.javatpoint.com/coinbase-transaction)* of block 69.

> **Note:** To be able to query transactions not related to your own wallet, like in this example, you will need to enable the `-txindex` option. This options requires the Dogecoin Core software to re-index the entire blockchain, and can take up to several hours.

First, request the information about block 69:

```console
#Find block hash from his height
shibetoshi:~$ dogecoin-cli getblockhash 69
3d2def20cd0d3aca148741ef469bda11647a3040d7669c82745d03c728706a8b

#Get block data
shibetoshi:~$ dogecoin-cli getblock 3d2def20cd0d3aca148741ef469bda11647a3040d7669c82745d03c728706a8b
{
  "hash": "3d2def20cd0d3aca148741ef469bda11647a3040d7669c82745d03c728706a8b",
  "confirmations": 7816,
  "strippedsize": 190,
  "size": 190,
  "weight": 760,
  "height": 69,
  "version": 1,
  "versionHex": "00000001",
  "merkleroot": "695ce4208fa7a87ef9e99805b0910dc129058ecdceb5cef7e25f71dcdc7936db",
  "tx": [
    "695ce4208fa7a87ef9e99805b0910dc129058ecdceb5cef7e25f71dcdc7936db"
  ],
  "time": 1386475225,
  "mediantime": 1386475209,
  "nonce": 3923708672,
  "bits": "1e0ffff0",
  "difficulty": 0.000244140625,
  "chainwork": "0000000000000000000000000000000000000000000000000000000004600460",
  "previousblockhash": "ffa69e04f928b84f19d84da25fb544340e54dca6c03c33930da245719e61c5ea",
  "nextblockhash": "44bf8abbbb96d4dcfb95df563e606c37987133ea3e013b23bbddde8d7f905fdd"
}
```

The `tx` field contains a list of all transactions included in this block. Only one transaction exist in block 69, the coinbase transaction.

We can see the entire transaction by querying for its identifier:

```console
#Syntax
shibetoshi:~$ dogecoin-cli getrawtransaction txid verbose

#Example
shibetoshi:~$ dogecoin-cli getrawtransaction 695ce4208fa7a87ef9e99805b0910dc129058ecdceb5cef7e25f71dcdc7936db 1
{
  "hex": "01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff0e04d9eea3520101062f503253482fffffffff0100ac6156be23000023210340a42a5ad6c4c0cd5ae539657032e0a359bd3e0f95771f34d71691b13460a624ac00000000",
  "txid": "695ce4208fa7a87ef9e99805b0910dc129058ecdceb5cef7e25f71dcdc7936db",
  "hash": "695ce4208fa7a87ef9e99805b0910dc129058ecdceb5cef7e25f71dcdc7936db",
  "size": 109,
  "vsize": 109,
  "version": 1,
  "locktime": 0,
  "vin": [
    {
      "coinbase": "04d9eea3520101062f503253482f",
      "sequence": 4294967295
    }
  ],
  "vout": [
    {
      "value": 393004.00000000,
      "n": 0,
      "scriptPubKey": {
        "asm": "0340a42a5ad6c4c0cd5ae539657032e0a359bd3e0f95771f34d71691b13460a624 OP_CHECKSIG",
        "hex": "210340a42a5ad6c4c0cd5ae539657032e0a359bd3e0f95771f34d71691b13460a624ac",
        "reqSigs": 1,
        "type": "pubkey",
        "addresses": [
          "D5TjotnkoXekNQBCn54CBWxjEzCJMMe7WS"
        ]
      }
    }
  ],
  "blockhash": "3d2def20cd0d3aca148741ef469bda11647a3040d7669c82745d03c728706a8b",
  "confirmations": 3964556,
  "time": 1386475225,
  "blocktime": 1386475225
}
```

The `vout` structure will give you information about where the transaction output.

## Node configuration

There are many parameters that can be configured to tune your node to your liking. There are two ways to change the configuration.

Using `dogecoind -help` will display all available configuration parameters that can be added as arguments:

**Command example :**
```console
shibetoshi:~$ dogecoind -daemon -paytxfee=0.01 -sendfreetransactions=1 -maxconnections=150
```

Configuration can be persisted by creating a `dogecoin.conf` file. Create it in the directory defined with the `datadir` setting, `$HOME/.dogecoin` by default, or specify the file location with `-conf`.

**dogecoin.conf example :**
```
daemon=1
server=1
listen=1
paytxfee=0.01
sendfreetransactions=1
maxconnections=150
```
You can see a more concrete example [here](/contrib/debian/examples/dogecoin.conf).

### Mainnet, testnet and regtest

When trying out new things, for example to test your application that interacts with the Dogecoin chain, it is recommended to not use the main Dogecoin network. Multiple networks are built-in for this purpose.

**Mainnet** : The main network where real transaction operate.  
**Testnet** : The test network, with peers.  
**Regtest** : The regression test network, to test with only local peers and create blocks on-demand.

When not specifying any network, *Mainnet* is the network used by default. To enable *testnet*, use the `dogecoind -testnet`.

To enable *regtest*, use the `-regtest` option.

> **Tip:** Remember to specify the network when you want to use `dogecoin-cli`.

### Data directory

The data directory is the location where Dogecoin Core files are stored, including the wallet, log files and blocks. You can modify the location with the `-datadir` setting.

**Default location :**

Platform | Data directory path
---------|--------------------
Linux    | `$HOME/.dogecoin`
macOS    | `$HOME/Library/Application Support/Dogecoin`
Windows  | `%APPDATA%\Dogecoin`

You may need to specify `-datadir` also when using `dogecoin-cli`.

See the [full documentation on file system](files.md) for more information.

### RPC credentials

Authentication is required to interact with the RPC interface. When no credentials are provided, Dogecoin uses a [random cookie](https://bitcoin.org/en/release/v0.12.0#rpc-random-cookie-rpc-authentication) that gets generated when the software is launched. It's possible to define your own credentials using `rpcuser` and `rpcpassword` parameters.

### Ports

A node can expose 2 different ports: one port for the **Peer to Peer Network** (P2P) to communicate with other nodes, and a second port for access to the RPC API. By default, the ports are configured as follows:

| Function | mainnet | testnet | regtest |
| :------- | ------: | ------: | ------: |
| P2P      |   22556 |   44556 |   18444 |
| RPC      |   22555 |   44555 |   18332 |

To configure them use the `-port` and `-rpcport` parameters.

### Memory

Running Dogecoin Core can require a lot of memory, so in some situations it may be necessary to optimize its usage. You can find more information about reducing the memory footprint in the [related guide](reduce-memory.md).

### Troubleshooting

By default, Dogecoin Core keeps detailed logs in the `debug.log` file, located in the `datadir`. Alternatively, the `-printtoconsole` parameter displays the log interactively to the terminal instead.

To get more verbose log output, you can enable debug mode by using the `-debug=<topic>` parameter to increase logic for a specific topic, or use `-debug=all` to see detailed logs on all topics.
