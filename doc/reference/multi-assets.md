# Multi-asset support

From the Mary ledger upgrade and onwards, Cardano supports [multi-assets](https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/specs.shelley-ma/latest/download-by-type/doc-pdf/shelley-ma), also referred to as a *native tokens* feature. This feature extends the ledger’s accounting infrastructure (originally designed for processing ada-only transactions) to accommodate transactions using a range of assets. These assets include ada and a variety of user-defined token types, the mixture of which can be transacted in a single tx output.

## What is a multi-asset?

Multi-assets are user-defined, custom tokens. They are supported natively, which means that the ledger handles the accounting and tracking of token-related activities. This offers distinct advantages for developers as there is no need to create smart contracts to mint or burn custom tokens, removing a layer of added complexity and potential for manual errors.

An asset is uniquely identified by an *asset ID*, which is a pair of both the *policy ID* and an *asset name*:

+ *PolicyID* - the unique identifier that is associated with a minting policy (hash of the minting policy).
+ *Asset name* - an (immutable) property of an asset that is used to distinguish different assets within the same policy. Unlike the policyID, the asset name does not refer to any code or set of rules, and can be represented as common words, eg. ‘couttscoin’.

Tokens that have the same asset ID have the property of being fungible with each other, and are not fungible with tokens that have a different asset ID.

## Minting a multi-asset token

### Step 1 - create a script

First, generate the keys that you require witnesses from using the
`cardano-cli address key-gen` command. Until the ledger is hard forked to Mary, we will
use a simple script to generate the policy ID.

*Note that you will be able to use a Plutus script when it is available in the Mary era.*

Then, construct a simple script in JSON syntax as described [here](./simple-scripts.md).

For this example, we will use the following script:

```json
 {
  "keyHash": $KEYHASH,
  "type": "sig"
 }
```

where `$KEYHASH` is generated as follows:

```bash
cardano-cli address key-hash --payment-verification-key-file policy.vkey
```

Similarly, generate the policy ID from the simple script:

```bash
cardano-cli transaction policyid --script-file policy.script
```

### Step 2 - construct the transaction body

Construct the tx body and specify the multi-asset you would like to mint.

*Note that you must spend at least the minimum UTxO value. In the tx body below, we mint and spend 5 tokens of a particular multi-asset:*

```bash
cardano-cli transaction build-raw \
            --mary-era \
            --fee 0 \
            --tx-in $TXIN \
            --tx-out $ADDR + 5 $POLICYID.couttscoin\
            --mint 5 $POLICYID.yourassetname \
            --minting-script-file $SCRIPT \
            --out-file txbody
```

### Step 3 - sign the transaction

Sign the transaction with the appropriate signing keys:

```bash
cardano-cli transaction sign \
            --signing-key-file txin.skey \
            --signing-key-file policy.skey \
            --testnet-magic 42 \
            --tx-body-file  txbody \
            --out-file      tx
```

### Step 4 - submit the transaction

To submit a transaction, run:

```bash
cardano-cli transaction submit --tx-file  tx --testnet-magic 42
```

## Burning a multi-asset token

### Step 1 - create the tx body that will burn 5 couttscoins:

```bash
cardano-cli transaction build-raw \
            --mary-era \
            --fee 0 \
            --tx-in $TXIN \
            --tx-out $TXOUT\
            --mint -5 $POLICYID.couttscoin \
            --minting-script-file $SCRIPT \
            --out-file txbodyburn
```

### Step 2 - sign the transaction:

```bash
cardano-cli transaction sign \
            --signing-key-file txin.skey \
            --signing-key-file policy.skey \
            --testnet-magic 42 \
            --tx-body-file  txbodyburn \
            --out-file      txburn
```

### Step 3 - submit the transaction:

```bash
cardano-cli transaction submit --tx-file txburn --testnet-magic 42
```
