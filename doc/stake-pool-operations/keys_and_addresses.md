# Creating keys and addresses

In the Shelley era of Cardano, every stakeholder can have two sets of keys and addresses:

* Payment Keys and addresses: To send and receive transactions
* Stake Keys and addresses: To control protocol participation, create a stake pool, delegate and receive rewards.

#### Payment key pair

To generate a _payment key pair_:

```
cardano-cli address key-gen \
--verification-key-file payment.vkey \
--signing-key-file payment.skey
```
This creates two files `payment.vkey` (the _public verification key_) and `payment.skey` (the _private signing key_).

#### Legacy key

To generate Byron-era _payment key:

Payment key files use the following format:
```json
{
    "type": "PaymentSigningKeyByron_ed25519_bip32",
    "description": "Payment Signing Key",
    "cborHex": "hex-here"
}
```

Where the `hex-here` is generated as `0x5880 | xprv | pub | chaincode`

#### Stake key pair
To generate a _stake key pair_ :

```
cardano-cli stake-address key-gen \
--verification-key-file stake.vkey \
--signing-key-file stake.skey
```
#### Payment address
Both verification keys (`payment.vkey` and `stake.vkey`) are used to build the address and the resulting `payment address` is associated with these keys.

```
cardano-cli address build \
--payment-verification-key-file payment.vkey \
--stake-verification-key-file stake.vkey \
--out-file payment.addr \
--mainnet
```
#### Stake address

To generate a `stake address`:

```
cardano-cli stake-address build \
--stake-verification-key-file stake.vkey \
--out-file stake.addr \
--mainnet
```
This address __CAN'T__ receive payments but will receive the rewards from participating in the protocol.


#### Query the balance of an address

> NOTE: Ensure that your node has synced to the current block height which can be checked at [explorer.cardano.org](https://explorer.cardano.org). If it is not, you may see an error referring to the Byron Era.

To query the balance of an address we need a running node and the environment variable `CARDANO_NODE_SOCKET_PATH` set to the path of the node.socket:

```
cardano-cli query utxo \
--address $(cat payment.addr) \
--mainnet
```
```
                            TxHash                                 TxIx        Amount
--------------------------------------------------------------------------------------------
```

**Note**`--mainnet` identifies the Cardano mainnet, for testnets use `--testnet-magic 1097911063` instead.
