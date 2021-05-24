# Building and signing transactions

Transactions vary in complexity, depending on their intended outcomes, but all transactions share a number of attributes:

* Input - contains funds that are spent by the transaction. It is simply the output of an earlier transaction. A transaction can have multiple inputs.
* Output - determine where the funds go to. An output is given by a payment address and an amount. A transaction can have multiple outputs.
* Payment address - an address that can receive payments, This is the only type of addresses that can be specified in a transaction output.
* Payment and stake key pairs - sets of files containing a public verification key and a private signing key.
* Invalid-before - The slot that the transaction is valid from. This can only be specified in the Allegra era and onwards.
* Invalid-hereafter - represents a slot, or deadline by which a transaction must be submitted. This is an absolute slot number, rather than a relative one, which means that the `--invalid-hereafter` value should be greater than the current slot number. A transaction becomes invalid at the invalid-hereafter slot.
* Era - Transactions can differ between the eras (e.g have new features such as multi-assets) so we must specify the era we are currently in.

To create a transaction in the shelley era we need to follow this process:

* Get the protocol parameters
* Draft the transaction
* Calculate the fee
* Define the validity interval for the transaction
* Build the transaction
* Sign the transaction
* Submit the transaction

**Protocol parameters**

Query and save the parameters in **protocol.json**

    cardano-cli query protocol-parameters \
    --mainnet \
    --out-file protocol.json

**Draft the transaction**

In the draft `tx-out`, `ttl` and `fee` can be zero. Later we use the `out-file` `tx.draft` to calculate the `fee`

    cardano-cli transaction build-raw \
    --shelley-era \
    --tx-in <TxHash>#<TxIx> \
    --tx-out <Address>+<Lovelace> \
    --tx-out <Address>+0 \
    --invalid-hereafter 0
    --fee 0 \
    --out-file tx.draft

**Calculate the fee**

Use `tx.draft` as `tx-body-file`. **Witnesses** are the amount of keys that must sign the transaction.

    cardano-cli transaction calculate-min-fee \
    --tx-body-file tx.draft \
    --tx-in-count 1 \
    --tx-out-count 2 \
    --witness-count 1 \
    --byron-witness-count 0 \
    --mainnet \
    --protocol-params-file protocol.json

For example:

    > 167965

**Determine the validity interval**

When building and submitting a transaction in the shelley era you need to check the current tip of the blockchain, for example, if the tip is slot 4000, you should set the invalid-hereafter to (4000 + N slots), so that you have enough time to build and submit a transaction. Submitting a transaction with a validity interval set in the past would result in a tx error.

    cardano-cli query tip --mainnet

Look for the value of `SlotNo`

    {
        "epoch": 259,
        "hash": "dbf5104ab91a7a0b405353ad31760b52b2703098ec17185bdd7ff1800bb61aca",
        "slot": 26633911,
        "block": 5580350
    }

Therefore, if N = 200 slots

    invalid-hereafter = 26633911 + 200
    invalid-hereafter = 26634111

**Build the transaction**

This time we include all the paramenters:

    cardano-cli transaction build-raw \
    --tx-in 4e3a6e7fdcb0d0efa17bf79c13aed2b4cb9baf37fb1aa2e39553d5bd720c5c99#4 \
    --tx-out $(cat payment2.addr)+100000000 \
    --tx-out $(cat payment.addr)+999899832035 \
    --invalid-hereafter 26634111 \
    --fee 167965 \
    --out-file tx.raw

**Signing**

A transaction must prove that it has the right to spend its inputs. In the most common case, this means that a transaction must be signed by the signing keys belonging to the payment addresses of the inputs. If a transaction contains certificates, it must additionally be signed by somebody with the right to issue those certificates. For example, a stake address registration certificate must be signed by the signing key of the corresponding stake key pair.

    cardano-cli transaction sign \
    --tx-body-file tx.raw \
    --signing-key-file payment.skey \
    --mainnet \
    --out-file tx.signed

**Submit**

    cardano-cli transaction submit \
    --tx-file tx.signed \
    --mainnet