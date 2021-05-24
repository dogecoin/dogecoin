# Cardano Node CLI Reference

The command line interface (CLI) provides a collection of tools for generating keys, constructing transactions, creating certificates, and performing other important tasks. It is organized in a hierarchy of subcommands, and each level comes with its own built-in documentation of command syntax and options.

This section provides a reference of the core `cardano-cli` commands and their associated sub commands:

*cardano-cli*
The set of `cardano-cli` commands include:
* `address`: payment address commands
* `stake-address`: stake address commands
* `transaction`: transaction commands
* `node`: node operation commands
* `stake-pool`: stake pool commands
* `query`: node query commands. This queries the local node whose Unix domain socket is obtained from the CARDANO_NODE_SOCKET_PATH environment variable.
* `genesis`: genesis block commands
* `text-view`: commands for dealing with text view files that are stored on disk such as transactions or addresses
* `governance`: governance commands

*cardano-cli address*
The `address` command contains the following sub commands:
* `key-gen`: creates a single address key pair
* `key-hash`: prints the hash of an address to stdout
* `build`: builds a payment address, with optional delegation to a stake address
* `build-script`: builds a token locking script
* `info`: prints details about the address

*cardano-cli stake-address*
The `stake-address` command contains the following sub commands:
* `key-gen`: creates a single address key pair
* `build`: builds a stake address
* `registration-certificate`: creates a registration certificate
* `delegation-certificate`: creates a stake address delegation certificate
* `deregistration-certificate`: creates a de-registration certificate

*cardano-cli transaction*
The `transaction` command contains the following sub commands:
* `build-raw`: builds a low-level transaction (uses the `--cardano-mode`, `--byron-mode`, `--shelley-mode` flags)
* `sign`: signs the transaction
* `assemble` : combines and assembles the transaction witness(es) with a transaction body to create a transaction
* `witness`: witnesses a transaction
* `submit`: submits the transaction to the local node whose Unix domain socket is obtained from the CARANO_NODE_SOCKET_PATH environment variable (uses the `--cardano-mode`, `--byron-mode`, `--shelley-mode` flags)
* `calculate-min-fee`: calculates the minimum fee for the transaction
* `txid`: retrieves the transaction ID
* `policyid`: retrieves the policy ID

*cardano-cli node*
The `node` command contains the following sub commands:
* `key-gen`: creates a key pair for a node operator’s offline key and a new certificate issue counter
* `key-gen-KES`: creates a key pair for a node KES operational key
* `key-gen-VRF`: creates a key pair for a node VRF operational key
* `key-hash-VRF`: creates a key hash for a node VRF operational key
* `new-counter`: keeps track of the number of KES evolutions for a given operational certificate hot key
* `issue-op-cert`: issues a node operational certificate

*cardano-cli stake-pool*
The `stake-pool` command contains the following sub commands:
* `registration-certificate`: creates a stake pool registration certificate
* `de-registration-certificate`: creates a stake pool de-registration certificate
* `id`: builds pool id from the offline key
* `metadata-hash`:  retrieves the metadata hash

*cardano-cli query*
The `query` command contains the following sub commands:
* `protocol-parameters` (advanced): retrieves the node’s current pool parameters (a raw dump of `Ledger.ChainDepState`). This
* `tip`: gets the node’s current tip (slot number, hash, and block number)
* `utxo`: retrieves the node’s current UTxO, filtered by address
* `ledger-state` (advanced):  dumps the current state of the node (a raw dump of `Ledger.NewEpochState`)
* `stake-address-info`: Get the current delegations and reward accounts filtered by stake address.
* `stake-distribution`: Get the node's current aggregated stake distribution
* `stake-snapshot` (advanced): Get the stake snapshot information for a stake pool
* `pool-params` (advanced): Get the current and future parameters for a stake pool

*cardano-cli governance*
The `governance` command contains the following sub commands:
* `create-mir-certificate`: creates an MIR (move instantaneous rewards) certificate
* `create-update-proposal`: creates an update proposal
* `create-genesis-key-certificate`: retrieves the genesis key certificate

*cardano-cli genesis*
The `genesis` command contains the following sub commands:
* `key-gen-genesis`: creates a genesis key pair
* `key-gen-delegate`: creates a genesis delegate key pair
* `key-gen-utxo`: creates a genesis UTxO key pair
* `key-hash`: prints the identifier, or hash, of a public key
* `get-ver-key`: derives verification key from a signing key
* `initial-addr`: gets the address for an initial UTxO based on the verification key
* `initial-txin`: gets the transaction ID for an initial UTxO based on the verification key.
* `create`: creates a genesis file from a genesis template, as well as genesis keys, delegation keys, and spending keys.
* `create-staked`: creates a staked genesis file
* `hash`: retrieves the hash value

*cardano-cli text-view*
The `text-view` command contains the following sub command:
* `decode-cbor`: prints a text view file, as decoded CBOR.
