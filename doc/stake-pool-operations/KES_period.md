# Key Evolving Signature and KES period

To create an operational certificate for a block-producing node, you need a _KES key pair_.

Here "KES" stands for _**K**ey **E**volving **S**ignature_, which means that after a certain _period_, the key will _evolve_ to a new key
and discard its old version. This is useful, because it means that even if an attacker compromises the key and gets access to the signing key,
he can only use that to sign blocks _from now on_, but not blocks dating from _earlier periods_, making it impossible for the attacker to rewrite history.

A KES key can only evolve for a certain number of periods and becomes useless afterwards.
This means that before that number of periods has passed, the node operator has to generate a new KES key pair, issue a new operational node certificate with that new key pair and restart the node with the new certificate.

To find out how long one period is and for how long a key can evolve, we can look into the _genesis file_. If that file is called `mainnet-shelley-genesis.json`,
we can type

    cat mainnet-shelley-genesis.json | grep KES
    "slotsPerKESPeriod": 129600,
    "maxKESEvolutions": 62,

in this example, the key will evolve after each period of 129600 slots and that it can evolve 62 times before it needs to be renewed.

Before we can create an operational certificate for our node, we need to figure out the start of the KES validity period, i.e. which KES evolution period we are in.

We check the current tip of the blockchain:

    cardano-cli query tip --mainnet

    {
        "epoch": 259,
        "hash": "dbf5104ab91a7a0b405353ad31760b52b2703098ec17185bdd7ff1800bb61aca",
        "slot": 26633911,
        "block": 5580350
    }

In this example, we are currently in slot 26633911, and we know from the genesis file that one period lasts for 129600 slots. So we calculate the current period by

    expr 26633911 / 129600
    > 205

With this we are able to generate an operational certificate for our stake pool:

    cardano-cli node issue-op-cert \
    --kes-verification-key-file kes.vkey \
    --cold-signing-key-file cold.skey \
    --operational-certificate-issue-counter cold.counter \
    --kes-period 205 \
    --out-file node.cert
