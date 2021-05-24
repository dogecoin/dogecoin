# Changelog for cardano-cli

## 1.27 -- April 2021

- The query tip now also returns the era (e.g. Shelley, Allegra, Alonzo).
  (#2561, #2562, #2598)
- The `address build` command now incorporates the functionality of the script
  address build command, which is now deprecated. (#2486, #2587)
- Add additional commands for creating MIR certificates to the CLI. This
  supports the ability to transfer funds to the treasury for Catalyst projects.
  (#2503)
- As a result of refactoring in preparation for the upcoming Alonzo release,
  there are a couple of breaking changes in CLI commands referring to scripts:
  - Auxiliary scripts (i.e. those included in the Tx auxiliary data, which are
    not required as transaction signers) must now be included with
    `--auxiliary-script-file` rather than with `--script-file`.
  - Scripts witnessing txins, certificates, withdrawls and minting must now be
    paired with the thing they are witnessing. E.g.
    ```
    --certificate-file  $certfile --certificate-script-file $scriptfile
    --tx-out $txout --minting-script-file $scriptfile
    --withdrawal $withdrawal --withdrawal-script-file $scriptfile
    --tx-in $txin --txin-script-file $scriptfile
    ```
  - Scripts should now be specified when creating the txbody, rather than when
    signing the transaction. (#2547)
- The transaction view command now additionally shows detailed of minted
  non-native tokens. (#2550)
- Removed support for Byron addresses using the Bech32 encoding. The only
  supported way to use Byron-era addresses is through a file, using the text
  envelope format. (#2605)
- Add a new command which computes the minimum ADA value/deposit for a
  multi-asset value. (#2612)
- Add two new query commands:
  - `query stake-snapshot` allows querying the three stake snapshots for a given
    stake pool.
  - `query pool-params` returns the current and future parameters, as well as
    the retiring information.
  (#2560)
- Updated the CLI reference documentation. (#2665)

## 1.26.1 -- March 2021
- It's no longer necessary to specify the era when making a CLI query. When not
  specified, the current era will be used as a default. (#2470)

## 1.26.0 -- March 2021
- Add three new queries to the CLI, exposing functionality already present in
  the API:
  - Protocol parameters
  - Stake distribution
  - Individual stake addresses
  (#2275, #2290)
- Fix the rendering of Byron-era `TxOut`s to be consistent with the rendering for
  Shelley-era addresses. (#2472)
- Add `cardano-cli transaction view`, which allows for pretty-printing details
  about a serialised transaction. (#2348)
- When constructing MIR certificates, the CLI now takes stake addresses rather
  than stake certificates. These are strictly more general and can be deduced
  from the certificates.
- Make the Mary era the default era in the CLI (#2415)
- Migrate the `cardano-submit-api` tool from `cardano-rest`. (#2370)
- The 'tip' query now additionally returns the epoch at the tip (#2440)
- Various internal improvements and refactoring (#2458)

## 1.25.0 -- January 2021
- Allow creating transactions with no outputs (#2223, #2226)
- Improved error messages for syntax errors in out-of-range lovelace quantities
  in transaction outputs (#2063, #2079)
- Improved reference documentation for simple scripts and their use (#2165)
- Refactoring in the Byron part of the CLI to make more extensive use of the
  Cardano API and reduce the maintenance burden (#2103, #2228)
- Remove support for changing the delegation from Genesis keys to operational
  keys in the Byron era. This feature was never used on the mainnet during the
  Byron era. (#2219)
- Clearer usage information in the CLI `--help` output (#2203)

## 1.24.2 -- December 2020

- Rename the flags `--lower-bound` and `--upper-bound` to be `--invalid-before`
  and `--invalid-hereafter` respectively, for naming consistency (#2186, #2190)
- Hide the deprecated `--ttl` flag in the `--help` output (#2189, #2190)

## 1.24.1 -- December 2020

- New command `transaction policyid` for making multi-asset policy ids (#2176)
- New command `byron transaction txid` to help scripts with getting the
  transaction id for Byron transactions made using the cli (#2169)
- New `--tx-file` flag for the command `transaction txid` to accept complete
  txs, not just tx bodies (#2169)
- Add a regression test for the "0" case of multi-asset tx out values (#2155)

## 1.24.0 -- December 2020

- CLI support for the Allegra and Mary eras, including creating transactions
  for the new eras, and support for the special new features in the new eras:
  script extensions, tx validity intervals, auxiliary scripts, multi-asset tx
  outputs and asset minting. (#2072, #2129, #2136)
- New flags for the `build-raw` command:
  + `--invalid-before` and `--invalid-hereafter` for the new Allegra-era feature
    of transaction validity intervals. The existing flag `--ttl` is equivalent to
    the new `--invalid-hereafter`, but it is now optional in the Allegra era.
  + `--script-file` for the new Allegra-era feature of being able to include
     auxiliary scripts in a transaction.
  + `--mint` for the Mary-era token minting feature.
- It is now necessary to specify the target era (e.g. `--allegra-era`) when
  creating a transaction (with `build-raw`) so that the right format and
  feature-set is used. The `--shelley-era` remains the default.
- It is necessary for now to specify the target era when using the CLI query
  commands. This may become automatic in future. The default is `--shelley-era`.
- Move all the Shelley sub-commands to the top level of the command line.
  For example `cardano-cli shelley transaction build-raw` becomes simply
  `cardano-cli transaction build-raw`. The existing names are also kept for
  compatibility. (#2076, #2145)
- Updated help text for the ledger/protocol state queries to clarify that they
  are primarily for debugging and are not stable interfaces (#2125, #2126, #2133)
- New command `genesis create-staked` to make it easier to set up Shelley-based
  testnets with stake pools and delegation set up from the genesis. (#2052)

## 1.23.0 -- November 2020

- Create VRF keys with the correct file permissions (#1948)
- New command to query the Shelley protocol (not just ledger) state (#2057)
- Skeletons of the new commands and flags for the multi-asset extensions (#2081)

## 1.22.1 -- October 2020

None

## 1.22.0 -- October 2020

- Adjust the ledger state dump to return the "extended" ledger state (#2019)
- Preliminary support for the upcoming Allegra and Mary eras (#1958, #2019)

## 1.21.2 -- October 2020

- Support bech32 and hex formats for reading verification keys (#1852)
- Minor help text improvements (#1661, #1956)
- Fix typo in KES docs (#1917, #1953)
- Improved documentation for CLI multi-signature support (#1976)

## 1.21.1 -- September 2020

None

## 1.21.0 -- September 2020
- Support for multi-signature scripts (#1788, #1880)

## 1.20.0 -- September 2020

- New command for creating genesis key delegation certificates (#1784)
- New command for converting more legacy signing key formats (#1756, #1822)
- Improved support for JSON to Tx metadata conversions, with two supported
  JSON schemas, suitable for different use cases (#1797)
- Support bech32 and hex formats for reading signing keys (#1790)
- Improved error messages for cli errors (#1801, #1839)

## 1.19.1 -- September 2020

- Fix the testnet vs mainnet argument for the genesis create command (#1761)
- Fix the --treasury flag for MIR cert creation (#1780)
- Fix the output rendering in the command to hash genesis files (#1713, #1767)
- Validate CBOR tx metadata when building tx bodies (#1432, #1677)

## 1.19.0 -- August 2020

- Support for converting ITN extended keys to Shelley stake keys (#1579)
- Support for converting password-protected Byron signing keys (#1633)
- Support for building script addresses (#1641)
- Improve the output of the stake-address-info query (#1546, #1636, #1671)
- Support for Bech32-encoded stake pool IDs (#1528, #1638, #1730)
- Reorganise the Byron CLI commands similarly to the Shelley ones (#1609, #1628)
- Code organisation refactoring (#1457, #1594)
- Extra tests and refactoring of tests (#1565, #1566, #1602, #1668)
- Code tidying using hlint and style tool (#1590, #1625, #1663, #1707, #1708)

## 1.18.0 -- July 2020

- Properly display the tx hash in the UTxO query command output (#1526, #1535)
- Refactoring and minor improvements in tests (#1538, #1541)

## 1.17.0 -- July 2020

- Allow genesis keys as tx witnesses (#1483)
- Allow extended genesis delegate keys to sign operational certs (#1497)
- New cli "key" command with key utilities (#1487, #1493)
- More helpful flag defaults in cli command for fee calculation (#1516)
- Default to the Cardano protocol for talking to a node (#1515)

## 1.16.0 -- July 2020

- Accept either a pool id or verification key in delegation cli command (#1460)
- Improved bash completion for flags that accept files (#1459)
- More and improved integration tests (#1429, #1450, #1453)

## 1.15.1 -- July 2020

- Support for interacting with nodes running in Byron-only, Shelley-only or
  the composite Cardano mode (Byron;Shelley) (#1435)
- Add support for byron keys and extended ed25519 keys (#1411)
- Port the CLI command implementations to the new API (#1416)
- Fix the output of the calculate-min-fee command (#1408)
- New stake and VRF key hashing commands (#1407)
- Use JSON output format for the address info command (#1426)

## 1.15.0 -- July 2020

- Fix the ledger state dump query (#1333, #1334)
- Fix the format of Byron addresses used in Byron CLI commands (#1326)
- Port CLI commands to use the new API (#1341, #1375, #1396, #1397)
- Change to JSON output for the "query tip" command (#1340, #1365)
- Moving code around to eliminate the cardano-config package (#1289, #1316)

## 1.14.2 -- June 2020

- Fix the hashing of stake pool metadata
- Fix the query that dumps the ledger state as JSON (#1333)

## 1.14.1 -- June 2020

No changes in the cardano-cli. There were changes in the cardano-node.

## 1.14.0 -- June 2020

- New flags for transaction metadata in tx construction (#1233)
- New flags for reward account withdrawals in tx construction (#1237)
- New command for pool metadata JSON validation and hashing (#1234, #1299)
- New flags for pool metadata in pool registration cert command (#1234)
- New flags for pool relays in pool registration cert command (#1282, #1296)
- New command to convert ITN keys (#1070, #1136)
- New command to get the txid of a tx body (#1231)
- Return appropriate exit code for tx submission failures (#1226)
- Fix the query stake-address-info to accept stake addresses (#1194, #1197)
- More regression tests (pioneer exercises 2, 3, 4) (#1209, #1247, #1279, #1287)
- Start to migrate to using the new typed API from cardano-api lib (#1284, #1298)
- Fix reporting of git revision via version command (#1283)

## 1.13.0 -- June 2020

- Fix the parsing of the pool margin in pool registration certs (#1063, #1110)
- Change the Shelley cli command and flag names to be more consistent (#1068)
- Add a command to query stake addresses, balance and delegation (#1053, #1129)
- Add a command to get the stake pool id (#1069)
- Add a command to create MIR certificates (#1075)
- Improved human readable error messages for Shelley commands (#1021)
- Improve error message for tx-in parser errors (#1066)
- Use a better default value of eMax in generated example genesis files (#1145)
- Regression tests covering the "pioneer" exercises 1 (#1073)
- Prerequisites for Tx metadata support (but not full support yet) (#1080)
- Updated Shelley from scratch documentation (#1062)

## 1.12.0 -- May 2020

- Reorganise the `shelley` subcommands (#840, #845)
- New `shelley genesis create` command (#852, #864, #908, #926, #929)
- New key-gen commands for various Shelley  keys (#846, #870)
- New commands for Shelley  address construction (#870, #872, #887)
- New Shelley transaction sign command (#894, #900)
- New Shelley transaction submission command (#904)
- New node query commands (#880, #884, #903, #918, #920, #933, #994, #1008, #1016)
- New commands to create stake address certificates (#890, #919, #967)
- New commands to create stake pool certificates (#922)
- New system commands to update genesis delgations and create MIR certs (#895)
- New command to calculate the minimum fee for a transaction (#931)
- New command to view the content of the various binary files (#915)
- New command to create Shelley protocol param updates (#950, #1004)
- Byron update proposal vote creation and submission (#804)
- Various refactoring (#874, #875, #949, #958, #966, #972)
- Commands that talk to the node no longer require the node config file (#901,
  #907, #917, #913, #928)
- Improved human readable error messages for Byron commands (#1003)
- Documentation on constructing a Shelley chain from scratch (#893, #932, #1000)
- Add `version` command and `--version` flag, with git revision (#959)
- Additional tests (#898, #935, #941, #952)


## 1.11.0 -- April 2020

- First version of the CLI as a separate package. The package provides a CLI
  (command line interface) to various low level node-related functionality.

  The CLI is not yet stable in this release.

- Split the `cardano-cli` package out of `cardano-node` (#819)
- Initial structure of Shelley CLI commands with a top-level "shelley" command
- Group Byron commands under a top-level "byron" command
- Commands to generate Shelley KES and VRF keys (#816)
- Command to generate Shelley address keys (#824)
