# Transaction Metadata

Transaction metadata (tx metadata) can contain details about a specific transaction, including sender and receiver IDs, transaction comments, and tags. Adding metadata to transactions  provides transaction information by adding arbitrarily structured data onto the chain and is a useful feature in Cardano Shelley. Tx metadata is stored on-chain and is carried along with each transaction. A factor in its design is that the on-chain metadata is not stored in the ledger state and does not influence transaction validation, thereby not compromising ledger performance.

Tx metadata is a valuable feature for developers who build applications and process transactions, and for application end users. Developers embed metadata directly and submit a valid transaction with accompanying details. End users don’t interact with the tx metadata directly, but can view transaction-specific metadata using the Cardano Explorer.

For example, tx metadata can be used to:

+ certify ownership and exchange of assets specifying asset owners in different time periods, transfer details, or asset value at the time of transfer
+ certify documents and signatures, by using a public hash that proves the document’s existence

Transactions can contain metadata whose hash is part of the body of the transaction. Because the metadata hash is in the tx body, this allows for integrity checking and authentication of the metadata.

## Metadata structure

In the Cardano environment, the structure of the metadata is defined by a mapping from keys to values (key-value pairs) that combine details for multiple purposes into the same transaction.

*Metadata keys* act as a schema identifier that show what the metadata value is. Keys are unsigned integers limited in size up to 64 bits.

The *metadata values* are simple terms, consisting of integers, text strings, byte strings, lists, and maps. Values are required to be structured, which makes it easier to be inspected and managed, particularly by scripts.

There is no limit on the number of key-value pairs (except the overall transaction size limit) or on individual structured values. However, there *is* a limit on the size of text strings and byte strings within the structured values, which is implemented to mitigate the problem of unpleasant or illegal content being posted to the blockchain.

Metadata *does not require* any additional fees. It simply contributes to the size of the transaction with a one-time processing fee based on transaction size.

**Binary schema**

The binary schema is based on the Concise Binary Object Representation ([CBOR)](https://tools.ietf.org/html/rfc7049) and Concise Data Definition Language ([CDDL)](https://tools.ietf.org/html/rfc8610) notations, and is presented as follows:

```
transaction_metadatum =
    { * transaction_metadatum => transaction_metadatum }
  / [ * transaction_metadatum ]
  / int
  / bytes .size (0..64)
  / text .size (0..64)

transaction_metadatum_label = uint

transaction_metadata =
  { * transaction_metadatum_label => transaction_metadatum }
```

## How to create a transaction with metadata using the cardano-cli

To create a transaction with metadata, first ensure that you have installed the [cardano-node](https://github.com/input-output-hk/cardano-node#cardano-node-overview) and follow instructions on how to launch [cardano-CLI](https://github.com/input-output-hk/cardano-node/tree/master/cardano-cli#cardano-cli).

To create a transaction with specified metadata, run this command:

`cabal exec cardano-cli -- transaction build-raw`

Shelley transactions can be created with the following options:

+ according to a chosen schema: *no schema* or *detailed schema*
+ according to a chosen metadata format: CBOR or JSON

```
  --json-metadata-no-schema
                       	Use the "no schema" conversion from JSON to tx
                       	metadata.
  --json-metadata-detailed-schema
                       	Use the "detailed schema" conversion from JSON to tx
                       	metadata.
  --metadata-json-file FILE
                       	Filepath of the metadata file, in JSON format.
  --metadata-cbor-file FILE
                       	Filepath of the metadata, in raw CBOR format.
  ```

## Metadata schemas - mappings and formats

On-chain metadata that is carried along with transactions is encoded according to CBOR. To create a transaction, you can add the metadata with pre-encoded CBOR. Alternatively, you can add metadata in JSON format, which will be converted into the internal format. In this section, we provide examples of the two different mappings between tx metadata and JSON, which are useful for different purposes. In both schemas, the top-level JSON is an object indexed by integers, which are mapped to JSON values.

### No schema

```json
{
    "17802948329108123211": {},
    "945845007538436815": "0x4ebc4ea3b43bb0cc76bb326f17a30d8f",
    "1351859328329939190": {
        "0x0e": "0x3bdefda92265",
        "0x14ff8d": -1.3139667629422286119e19
    },
    "7505166164059511819": "rceHlUxXlWmZJcxYd",
    "7274669146951118819": -1.4814972676680046432e19,
    "1302243434517352162": ["UJB3",-1.6236436627090480302e19]
}
```
The no schema mapping allows almost any JSON value to be converted into tx metadata. This does not require a specific JSON schema for the input, however, it does not expose the full representation capability of tx metadata. In the "no schema" mapping, the idea is that almost any JSON value can be turned into tx metadata and then converted back without any loss. The approach for this mapping is to use the most compact tx metadata representation, as follows:

* JSON lists and maps are represented as CBOR lists and maps
* JSON strings are represented as CBOR strings
* JSON hex strings with \"0x\" prefix are represented as CBOR byte strings
* JSON integer numbers are represented as CBOR signed or unsigned numbers
* JSON maps with string keys that parse as numbers or hex byte strings, represented as CBOR map keys that are actually numbers or byte strings.
* JSON `Null` or `Bool` are not allowed

The string length limit depends on whether the hex string representation is used or not.

* Text string limit: 64 bytes for the UTF8
representation of the text string.
* Byte string limit: 64 bytes
for the raw byte form (**i.e. not the input hex, but after hex decoding**).

### Detailed Schema

```json
{
    "10504143639544897702": {
        "int": -1.4304053759886015514e19
    },
    "17329656595257689515": {
        "string": "yQNttsok3EQ"
    },
    "15345559452353729335": {
        "bytes": "fa1212030dd02612eccb"
    },
    "593828266493176337": {
        "list": [
            {
                "string": "HaYsLNx7"
            },
            {
                "int": -1.537136810304170744e19
            }
        ]
    },
    "17200655244803120463": {
        "map": [
            {
                "k": {
                    "map": []
                },
                "v": {
                    "string": "G"
                }
            },
            {
                "k": {
                    "string": "zNXD7qk"
                },
                "v": {
                    "list": []
                }
            }
        ]
    }
}
```

The *detailed schema* is a mapping that exposes the full representation capability of tx metadata but relies on a specific JSON schema for the input JSON.
In the *detailed schema* mapping, the representation capability of the tx metadata is exposed in the form of a JSON schema. This means that the full representation is available and can be controlled precisely. This also means that any tx metadata can be converted into JSON and back without any loss. That is, we can round-trip the tx metadata via the JSON and round-trip schema-compliant JSON via tx metadata. Note: `Null` and `Bool` JSON values are still not allowed.

Detailed Schema:

* "int": any integer
* "bytes": hexadecimal
* "string": any valid JSON string
* "list": list of objects
* "map": list of objects with key "k" and value "v" which both contain objects.

## References and other available material

Here are some materials for further reading:

+ Tx metadata motivation and use: [Design Specification for Delegation and Incentives in Cardano](https://hydra.iohk.io/build/3744897/download/1/delegation_design_spec.pdf), p 53.
+ [Tx metadata in wallet-CLI](https://github.com/input-output-hk/cardano-wallet/wiki/TxMetadata).
