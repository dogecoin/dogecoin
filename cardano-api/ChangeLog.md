# Changelog for cardano-api

## 1.27 -- April 2021

- Add initial support for the ledger state and folding over blocks to the API.
  (#2633)
- Scripts are now stored within the TxBody in the API, rather than in the
  witnesses. (#2547)

## 1.26.1 -- March 2021

- The cardano-submit-api now takes transactions encoded as CBOR rather than
  JSON. This reverts a change to existing behaviour for backwards compatibility.
  (#2491, #2512)
- Remove a backwards-compatibility workaround related to the optional query
  point (#2241 below) when querying the NodeLocalState. This had resulted in
  spurious notifications of disconnection in the logs. Note that as a
  consequence of this, instances of the CLI and other tools using the 1.26.1 API
  will fail to query node state from older versions of the node. (#2540)

## 1.26.0 -- March 2020
- Added a demo for the use of cardano-client. This is an API to allow writing
  programs to interact with the cardano node. (#2295, #2303)
- Removed code pertaining to the old IPC API (#2319)
- Add the ability to calculate the minimum deposit needed for a transaction to
  the API, given a value. (#2325)
- When querying the NodeLocalState, make the query point optional, and use the
  chain tip when not specified. (#2241)
- Various internal improvements and refactoring (#2349, #2458)

## 1.25.0 -- January 2020
- New IPC modules for easier interaction with the node, including support for
  all existing local state queries (#2230, #2238, #2263, #2277, #2286)
- API support for Byron era update proposals and votes (#2209, #2271)
- Make Cardano.Api the primary public module for the API.
- API support for serialising multi-asset PolicyId and AssetName (#2270)
- API for pretty-printing JSON output (#2103)
- Improved tests for Byron era legacy key formats (#2259)
- More precise error cases for tx outputs that are out of range (#2217)
- Host up-to-date generated API documentation via github
  https://input-output-hk.github.io/cardano-node/ (#2273, #2276, #2278)

## 1.24.2 -- December 2020

None

## 1.24.1 -- December 2020

- Fix the getTxId implementation for Byron-era transactions (#2169)

## 1.24.0 -- December 2020

- Full API support for the Allegra and Mary eras, including creating
  transactions for the new eras, and support for the special new features in
  the new eras: script extensions, tx validity intervals, auxiliary scripts,
  multi-asset tx outputs and asset minting (#2092, #2110, #2111, #2121, #2127,
  #2128, #2141, #2149)


## 1.23.0 -- November 2020

- Preliminary support for the Allegra script language extensions (#2069)
- Preliminary support for the Mary multi-asset extensions (#2083, #2085, #2093)
- Internal refactoring of the API code (#2040, #2055, #2094)

## 1.22.1 -- October 2020

None

## 1.22.0 -- October 2020

- Preliminary support for the upcoming Allegra and Mary eras (#1958, #2019)
- Additional test coverage (#1999)

## 1.21.2 -- September 2020

- Add a Ed25519-BIP32 instance of the new crypto classes (#1933, #1952)
- Adjust what is exposed via Cardano.Api.{Byron,Shelley} (#1932)

## 1.21.1 -- September 2020

None

## 1.21.0 -- September 2020
- Support for multi-signature scripts (#1788)
- Support for Byron witnesses for addresses that use attributes, which includes
  all addresses in legacy Daedalus Byron wallets (#1851, #1871)
- Introduce a Cardano.Api top level module exporting only the public parts
  and modules Cardano.Api.{Byron,Shelley} that expose the underlying library
  types for applications that need it (#1881)

## 1.20.0 -- September 2020

- Improved support for JSON to Tx metadata conversions, with two supported
  JSON schemas, suitable for different use cases (#1797)

## 1.19.1 -- September 2020

- Adjust the tx metadata JSON schema to be fully recursive (#1735)
- Audit compliance with CIP5 for common bech32 prefixes (#1781)
- Add functionality for validating tx metadata (#1432, #1677)

## 1.19.0 -- August 2020

- Support for scripts and specifically multi-sig scripts (#1623)
- Support for JSON syntax for multi-sig scripts (#1660)
- Support for converting tx metadata to/from JSON (#1606, #1682)
- Support for Bech32-encoded stake pool IDs (#1528, #1638)
- Code tidying using hlint and style tool (#1590, #1625, #1663, #1707, #1708)

## 1.18.0 -- July 2020

None

## 1.17.0 -- July 2020

- Allow genesis keys as tx witnesses (#1483)
- Allow extended genesis delegate keys to sign operational certs (#1497)
- Add support for extended keys for stake, genesis and delegate keys (#1487)

## 1.16.0 -- July 2020

- Remove the old API (#1444, #1456)
- Added raw serialisation instances for all key types (#1455)
- Added bech32 serialisation following draft CIP 5 (#1455)

## 1.15.1 -- July 2020

- Include tx metadata in transactions in the new api (#1406)
- Add support for extended ed25519 keys for payment keys (#1411)
- Improve tx submission API in the new API (#1430)

## 1.15.0 -- July 2020

- Fix the ledger state dump query (#1333, #1334)
- Support for Byron witnesses in Shelley txs in the typed API (#1339)
- Support for Bech32 serialisation in the typed API (#1382)
- Support for other additional functionality in the typed API (#1337, #1375)
- More tests for the typed API (#1360, #1369, #1378)
- Moving code around to eliminate the cardano-config package (#1289, #1380)

## 1.14.2 -- June 2020

- Fix the query that dumps the ledger state as JSON (#1333)

## 1.14.1 -- June 2020

No changes in the cardano-api. There were changes in the cardano-node.

## 1.14.0 -- June 2020

- Improvements to the strongly-typed API (#1112, #1220, #1227, #1246)

  The API is not yet stable in this release.

## 1.13.0 -- June 2020

- Initial version of an improved strongly-typed API.
  Initially focusing on creating and serialising keys.

  The API is not yet stable in this release.

## 1.11.0 -- April 2020

- Initial version of the API package. The package provides client-side
  functionality for constructing and submitting transactions.

  The API is not yet stable in this release.

- Initial transaction API with Byron support and Shelley stubs (#787)
- Shelley address key pair generation (#799)
