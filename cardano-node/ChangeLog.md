# Changelog for cardano-node

## 1.27 -- April 2021

### node changes

- Add an additional GC heap metric to RTS. (#2592)
- Add details about registering relays on-chain to SPO documentation. (#2591)
- Improved installation documentation. (#2624)
- Preparatory support for Alonzo. (#2547)
- Preparatory support for Voltaire. (#2498)
- Add a docker image option for downloading cardano-node. (#2632)
- Miscellaneous internal improvements. (#2644, #2645, #2646)

### consensus changes

- Some preparation for upcoming work on Voltaire. (#2978)
- Drop support for GHC 8.6.5. Note that support had already been dropped in the
  node. (#3004)
- Introduce an option to configure the snapshot interval. This will primarily be
  used by downstream teams for testing and benchmarking. (#2966, #3092)
- Preparatory work for the implementation of Ouroboros Genesis. (#3002)
- Test varying stake distributions in the automated tests. (#3072)
- Add a peer label to chain sync client traces, allowing us to view which peer
  supplied which header and when. (#3091)
- Provide the ledger with the ability to perform slot to time conversion. This
  will be needed for Plutus scripts in the upcoming Alonzo release. (#3036, #3098)
### ledger changes

- Work on implementing the upcoming Alonzo era. (#2192, #2198, #2200, #2204,
  #2210, #2211, #2212, #2214, #2215, #2216, #2218, #2220, #2221, #2222, #2225,
  #2226, #2227, #2229, #2231, #2235, #2237, #2242, #2243, #2251, #2256, #2260,
  #2264)
- Released the CDDL spec for Alonzo. (#2228)
- Add an optional network ID field to the transaction body. This will only come
  into use in the Alonzo era. (#2219, #2254)
- Define a cardano-ledger-core package containing era-independent primitives.
  (#2207)
- Fixes to the ShelleyMA formal specification. (#2181)
- Various internal refactorings and improvements. (#2191, #2202, #2206, #2217,
  #2233, #2247, #2257)
- Fix a bug in reward update application which could have resulted in the wrong
  amount of funds being added to the treasury. (#2238)
- Some work to reduce the extra CPU load at an epoch boundary, and reduce the
  resulting "pause" in normal operation. (#2240, #2241, #2250, #2262)
- Added formal definitions for much of the crypto used to the Shelley spec.
  (#2236, #2252)
- Expose AdaPots for use in downstream projects. (#2208)
- Some Byron-era serialisation testing. (#2217)
- Some preparation for upcoming work on Voltaire. (#2135)

### network changes

- Removed the deprecated 'KThxBye' message from the TxSubmission protocol.
  (#2238)
- Introduce new representation for local root peers. This is in preparation for
  P2P support. (#3006)
- Add a new KeepAlive codec that produces a valid CBOR encoding. (#3062)
- Various internal updates and refactorings. (#3003, #3077)

## 1.26.1 -- March 2021

### node changes
- Fix RTS options, which got accidentally corrupted in the previous release.
  (#2511)
- Support for GHC 8.6.5 has been dropped. (#2507)
- Various internal improvements and refactorings. (#2505)
- Disable the "uncoupled blocks" metric. This was shown in profiling to have an
  unfortunately large overhead. This reverts the change introduced in #2321.
  (#2510)
- Update the iohk-monitoring framework to fix a file descriptor leak. (#2518)

### ledger changes
- Fix an unevalutated thunk error in reward computation. (#2183)
- Additional properties added to the Mary/Allegra formal specification (#2178)
- Updates to the Alonzo formal specification (#2189, #2194)
- Work on implementing the upcoming Alonzo era. (#2176, #2185, #2190)

### network changes
- Add a tracer for the delay between when a block should have been forged and
  when we're ready to adopt it. (#2995)

## 1.26.0 -- March 2021

### node changes
- Expanded documentation for Windows builds (#1993, #2231)
- Other expanded and updated documentation (#2308, #2334, #2342, #2347, #2429,
  #2431, #2432, #2434, #2435, #2442)
- Simplified configuration for working with RTView (#2303)
- Prototype reporting metrics to EKG directly, rather than via the switchboard.
  (#2355)
- Multiple improvements to the configuration when running cardano-node as a
  Nixos service. (#2374, #2384, #2389, #2404, #2418, #2424, #2436, #2443, #2483)
- KES metrics are now traced even when the KES key has expired. This should make
  it easier for users to detect whether they have an expired KES key. (#2373)
- Add a metric to display the number of forged blocks "lost" due to switching to
  an alternative fork. (#2321)
- Various internal improvements and refactoring (#2331, #2481)
- Set a flag which enables early return of memory to the OS. This should help in
  accurate reporting of memory consumption. (#2493, #2495)

### consensus changes
- Add some additional tracing infrastructure (#2874)
- Add an automated test for forking from Allegra to Mary eras (#2915)
- Continued work on the technical report of the consensus and storate layer
  (#2939, #2943)
- Add the ability to query a node to determine whether it issued a block. This
  supports the "blocks lost due to switching" metric mentioned in the node
  changes above. (#2930)
- Support disabling the maximum known versions for network protocols. This is to
  allow QA and benchmarking teams to evaluate new versions of networking
  protocols. (#2947)
- Add the ability to query for the time a block was forged (this is a very
  fragile feature, only to be used for metrics) (#2988)
- Various internal improvements and refactoring (#2872, #2899, #2900)

### ledger changes
- Update the calculation of rewards to be done progressively, spreading the
  additional CPU load across a number of days. This should resolve the slowdown
  4k/f slots into the epoch which has been seen by a number of node operators.
  (#2142, #2183)
- Introduce the `SafeHash` abstraction which governs what types can be safely
  hashed. This is important since we do not require a canonical representation
  for submitted data (#2104, #2134)
- Changed the aggregation of rewards, such that we store more data about reward
  provenance. This makes it easier for downstream applications to show e.g.
  where rewards come from. (#2117, #2123, #2164)
- Fixed a performance regression in the Allegra and Mary eras. This resulted in
  node slowdown when validating failed transactions. (#2144)
- Fixed a subtle bug in reward computations, where the wrong amount of money
  would be added to the treasury (#2136).
- Updates to the Alonzo formal specification (#2108)
- A lot of work on implementing the upcoming Alonzo era (#2124, #2127, #2148,
  #2161, #2165, #2166, #2169, #2170)
- Add the ability for MIR certificates to transfer money from the reserves
  to/from the treasury. This will be used to support Catalyst funding. (#2146)
- Various internal improvements and refactoring (#2115, #2177)

### network changes
- Various changes to support P2P (#2526, #2911, #2921)
- The `LocalStQuery` and `ChainSync` protocols now allow for additional effects
  to take place once the client has acquired a connection. This should allow for
  additional pipelining in clients such as the wallet. (#2896)
- Various tracing improvements:
  - Trace termination of mini-protocols (#2924)
  - Additional tracing of transaction submission (#2924)
- Remove some code for old and deprecated APIs (#2934)
- Some additional support for `cardano-client`, an API to write tools for
  interacting with the node (#2916)
- Allow configuring the timeouts for protocol handshakes. This should serve to
  mitigate a potential attack where handshakes are begun but not concluded,
  occupying resources on the node. (#2990)
- Various internal improvements and refactoring (#2876, #2922, #2929, #2940,
  #2956, #2958, #2960)

## 1.25.0 -- January 2021

### node changes
- All metrics now use a common name prefix `cardano.node.metrics`. This requires
  a one-off change to the node configuration to route all metrics to the metrics
  backend and not have them in the log files. This should reduce the number of
  such changes in future. (#2281)
- More detailed tracer output for protocol tracers (#2178)

### consensus changes
- Support in testnets for skipping initial eras. This makes it easier and
  quicker to automate the setup of Cardano-mode testnets that start in later
  eras, such as Allegra or Mary. This enables simplifications in the setup
  for integration tests or benchmarks (#2811)
- Improve the handling of the encoding of local IPC queries by older clients
  that do not understand later hard forks. In future this should mean that
  clients that do not understand a hard fork only fail at the time of the hard
  fork itself, and not earlier when the update proposal is confirmed (#2818)
- New query for rewards provenance. This is intended to help wallets explain
  more details about rewards, and to help SPOs and users better understand how
  pools' performance and ranking are calculated. This is not yet supported in
  the CLI in this release. (#2830)
- Support for using multiple leader credentials for the purpose of running
  large scale benchmarks. It supported in testnets only, not mainnet (#2832)
- Various internal improvements and refactoring (#2786, #2792, #2795, #2812)
- Fix a non-critical unexpected thunk detected by the space leak tests (#2798)
- Initial steps of an overhaul of the automated Ouroboros tests (#2835, #2837)
- A first draft of a technical report on the design and the internals of the
  Cardano consensus and storage layer (#2663, #2838, #2841, #2842, #2853)
- Document the existing coding style conventions (#2829, #2834, #2836)
- Fix the QueryAnytimeMary case for queries about the Mary era (#2891)

### ledger changes
- Fix the decoding of multi-asset values to match the specification and improve
  the corresponding tests (#2049, #2050)
- Enforce the size of multi-asset names, to match the specification (#2074)
- Minor change to the ledger CDDL binary specification for the Mary era:
  restrict the range of multi-asset values to -2^63..2^63-1 where previously
  the allowed range was -2^64..2^64-1 (#2092)
- Rename fields in the ledger's CDDL binary specification for clarity and
  consistency, but no actual changes to the binary format (#2045)
- Change the minimum ada UTxO value formula for multi-asset values to better
  reflect the resource costs and to pass on savings (in the form of a lower
  minimum ada UTxO value) for applications that use smaller asset bundles by
  sharing policy ids or using shorter asset names (#2107)
- Improvements to the size of the internal storage format of multi-asset values,
  enabling a lower minimum ada UTxO value for multi-asset output values (#2083)
- Restrict the maximum size of multi-asset output values (#2099)
- Fix the conversion of Allegra era txs to Mary era txs (#2054)
- Internal refactoring to simplify some of the types and make fewer types be
  parametrised by the era, when they do not actually vary by era (#2038)
- Internal support for rewards provenance to support a new node query (#2044,
  #2075)
- Improved test coverage (#2059, #2066, #2088, #2094, #2097, #2098)
- Terminology change: the metadata section is renamed to "auxiliary data" to
  distinguish it from the existing transaction metadata (#2052)
- Extra documentation on native tokens (#2046)
- Various internal improvements and refactoring (#2057, #2070, #2071, #2072,
  #2072, #2078, #2081, #2086, #2090, #2091, #2096, #2103, #2106)
- Progress on the new Alonzo era (#2022, #2055, #2061, #2062, #2067, #2077,
  #2087, #2088, #2095)

### network changes
- Fix the transaction submission protocol handler to accept duplicate tx ids
  properly. This was detected by automated testing. (#2718)
- Internal infrastructure to support seeding the P2P graph construction from
  the SPO relays registered on the chain, weighted by stake (#2535, #2536)
- Introduce v6 of the node-to-node protocol where we reverse the initial agency
  of the tx-submission protocol to match the others. This is preparation for
  the P2P governor which requires the initial agency for mini-protocols to be
  uniform (#2807)
- Introduce v8 of the node-to-client protocol with an extension to the local
  query protocol that allows acquiring the point at the node's current chain
  tip, without having to provide that point explicitly. Using this simplifies
  things for node clients for the common use case where the current tip is
  needed, and eliminates a rare race condition (#2875)
- Fix a resource leak for chain-sync clients (#2235, #2870, #2888)
- Tracing improvements for the mux component (#2794)
- Preparations for publishing io-sim as a public library (#2791)

## 1.24.2 -- December 2020

### node changes
- Eliminate the need to update the `LastKnownBlockVersion-*` entries in the node
  config files for the Shelley-based eras. This means the configuration does not
  need to be updated for the Allegra or Mary eras. (#2193)
- Fix an off-by-one error in the `txsProcessedNum` metric (#2183, #2192)
- Revert the renaming of the metrics from 1.24.1 (#2158, #2187, #2192)
- Export some more metrics for selected OS and RTS stats (#2192)

### consensus changes
None

### ledger changes
None

### network changes
None

## 1.24.1 -- December 2020

### node changes
- Move all metrics under the `cardano.node.metrics` namespace (#2158)
- New metrics for the size of the UTxO and delegation maps (#2158)
- Tracing changes to support "K=1000" benchmarks (#2156, #2175)
- Mention the required `xz` tool in the Cabal build instructions (#2132)

### ledger changes
- Fix the use of Shelley multi-sig scripts in the Allegra and later eras (#2035)
- Fix the serialisation format for multi-asset values to follow the CDDL (#2039)
- Additional "golden" tests for encodings in the Allegra and Mary eras (#2031)
- Additional binary encoding "round-trip" tests (#2032)
- Benchmarks for the transaction generators (#2024)

### consensus changes
- Fix support for Shelley transactions in the Allegra era (#2788)
- Add support for the system wall clock time being adjusted backwards by a small
  amount without triggering a node shutdown and restart. Small backwards
  adjustments can be caused by NTP. (#2781, #2785)

### network changes
- Update the `cardano-ping` tool to support the node-to-node protocol V4 (#2787)

## 1.24.0 -- December 2020

### node changes
- Add a nodeStartTime metric. This is a partial replacement for the uptime
  metric that was removed in the 1.23.0 release. (#2118)
- Miscellaneous "chairman" integration test improvements (#2122, #2123, #2130, #2146)
- Allow starting a node with the credentials for many stake pools. This is used
  for benchmarking large numbers of stake pools more easily. This feature is
  not available in the "Cardano" protocol mode used for the mainnet. (#2068)

### ledger changes
- Support for optional additional scripts in the tx auxiliary data (#1993)
- Ensure the minting field in is not used in the Allegra era (#2028)
- Update the CDDL specification of the blockchain binary format for Allegra
  and Mary eras (#1994, #1999, #2009)
- Improved serialised binary format for multi-asset values (#1979)
- Add a compact in-memory storage format for multi-asset values (#1996)
- Updates to the multi-asset formal specification (#2003)
- Adjust how the major protocol version is handled for soft forks (#1998)
- Extend more Shelley tests to cover the Allegra and Mary eras too (#1997,
  #2012, #2029)
- Internal refactoring and clean-ups (#2013, #2014, #2018, #2021, #2025)
- Initial preparatory steps for the Alonzo era (#2016, #2027)
- Minor corrections to the formal spec arising from internal review (#2023)

### consensus changes
- Use a single set of credentials for all Shelley-based eras. This keeps the
  node configuration simple for the new eras. (#2753)
- Add the Allegra era in the "ThreadNet" consensus tests (#2633, #2641)
- Add support for a Mary-only protocol mode to simplify benchmarking (#2754)
- Internal refactoring and clean-ups (#2598, #2751, #2779, #2778)

### network changes
- Preparations for publishing io-sim as a public library (#2775)
- Internal improvements to the "snockets" API (#2772, #2777)
- Improved logging of the creation of the node's sockts (#2746)

## 1.23.0 -- November 2020

### node changes

- Preliminary support for the upcoming Allegra and Mary eras (#2038, #2080)
- Remove the HardForkNotBeforeEpoch setting from the config file (#2073)
- Tracing changes for the benefit of the RTView monitoring tool (#2047, #2062)
- Minor documentation improvements requested by the external audit (#2046)
- Miscellaneous "chairman" integration test improvements (#2042, #2043, #2048,
  #2061, #2078, #2087, #2086)
- Improve handling of invalid command line input (#2088)

### ledger changes

- Fix a space leak in the stake pool performance tracking that caused writing
  ledger state snapshots in consensus to take too long (#1967)
- Improved ledger state serialisation performance. This involving a change to
  the serialisation format which will require the ledger state to be rebuilt
  upon node startup (#1968)
- Further development for the Allegra era (#1951, #1965, #1984, #1989)
- Further development for the Mary multi-asset era (#1961, #1959, #1971, #1977,
  #1981, #1987, #1988, #1990)
- Internal refactoring to support the new eras (#1954, 1978)
- Extra test coverage (#1958, #1966)
- Minor corrections to the formal spec arising from internal review (#1969, #1992)

### consensus changes

- Internal changes to improve the clarity of the chain selection algorithm and
  to better match the description in the tech report (#2732, #2735, #2743)
- Support for creating ledger state snapshots for testing purposes (#2733)
- Support for the new Allegra and Mary ledger eras (#2668, #2670, #2677, #2679)
- Extra test coverage for new eras (#2669, #2737, #2738, #2740)
- Remove the SafeBeforeEpoch feature as it provided no benefit. In the node
  config file this was called HardForkNotBeforeEpoch. (#2736, #2739)
- Internal refactoring (#2720)

### network changes
- None.

### crypto changes

- Revert the use of libsodium in the KES implementation for now (#1986, #2752)

## 1.22.1 -- October 2020

### node changes
- Miscellaneous "chairman" integration test improvements (#2025, #2028, #2030
  #2033, #2035, #2037)

### ledger changes
- Fix a bug in the serialisation of coin values that appear in tx validation
  errors. Also added regression tests. This bug was introduced in the
  unreleased 1.22.0 so has not been in any released version. (#1995)
- Restore build compatibility with GHC 8.6.5, so this release builds with
  both GHC 8.6.5 and 8.10.2. Future releases will support 8.10.x only. (#1956)
- Further development of the multi-asset ledger rules (#1905, #1938, #1931)

### consensus changes
- Internal updates for ledger interface changes (#2716, #2719)

### network changes
- Internal API improvements that will subsequently allow the cardano-api
  package to present a simpler API for applications that use the node's
  client-side IPC protocols (#2713, #2714)

## 1.22.0 -- October 2020

### node changes
- Remove the deprecated LiveView feature, now that RTView is released (#1977)
- Switch the default compiler version to GHC 8.10.2 (#1990)
- Preliminary support for the upcoming Allegra and Mary eras (#1958)
- Documentation updates (#1613, #1968, #1960, #2015)

### consensus changes
- Preliminary support for the upcoming Allegra and Mary eras (#2666)
- Fixes for building with the compiler version GHC 8.10.2 (#2540, #2652)
- Several new local state queries in v4 of the node-to-client protocol (#2694)
- Keep a compact form of the Shelley genesis content, to allow it to be
  queried later (#2704)
- Switch to the newly-published "nothunks" package (#2664)
- Address technical debt in the db-analyser tool (#2667)
- Internal code renaming for improved clarity of protocol names (#2683)
- Refactoring (#2687)
- Documentation for the hard fork transition (#1741)

### ledger changes
- Fix a corner case in the way the pool performance history is calculated as
  part of the overall pool score calculation (#1897)
- Preliminary support for the upcoming Allegra and Mary eras (#1899)
- Preliminary support for the time-lock feature in the script language to be
  included in the Allegra era (#1895)
- Preparation for multi-asset support by allowing the representation of coin
  values to be chosen differently for different eras (#1847, #1875)
- Preparation for new eras by allowing the representation of tx bodies to be
  chosen differently for different eras (#1908)
- Allow most Shelley ledger rules to be reused in subsequent eras (#1922)
- New internal infrastructure to improve how we handle the translation of
  configuration and state between ledger eras (#1893, #1902, #1923)
- Adjust how the ledger is parametrised to reflect the fact that some parts
  change with the ledger era, while others change with the consensus protocol.
  This makes it easier to handle ledger-only era changes that still use the
  same Praos consensus protocol. (#1915)
- Improved internal infrastructure for managing types, such as txs, that cache
  their serialised representation (#1917)
- Support serialisation for the Shelley genesis data structure to support a
  new node query that can return the genesis parameters (#1927)
- Minor corrections to the formal spec arising from internal and external review
  and internal audit (#1878, #1896, #1918)
- Test improvements (#1843, #1844, #1845, #1886, #1888, #1889, #1900, #1903, #1929)
- Switch to the newly-published "nothunks" package (#1894)

### network changes
- Fix a bug in IPv6 support (introduced in the unreleased 1.21.2) that caused
  the node to be unable to establish any network connections on Windows systems
  that have IPv6 support enabled (#1991, #1994)
- New cardano-ping demo and tool (#2701)
- Make the node fail on startup if we cannot bind to the necessary ports (#2696)
- Improvement to the calculation and collection of network connection
  performance metrics (#2636)
- Improvements to the handling of the initial connection handshake (#2691)
- Preparations for publishing io-sim as a public library (#2631)
- Improved tests for protocol shutdown and restart (#2628, #2629)

## 1.21.2 -- October 2020

### node changes
- Check VRF signing key files have the correct file permissions (#1936, #1938)
- Improve IPv6 support and related internal refactoring (#1928)
- Extend the "chairman" testing tool to support different testnets (#1915)
- Miscellaneous "chairman" integration test improvements (#1951, #1961, #1939,
  #1966, #1970, #1973, #1981)

### consensus changes
- Fix a failure that occurs on starting a node when there are certain kinds of
  DB corruption (specifically a ledger snapshot that is newer than the tip of
  the immutable DB, which would typically occur when chain DB files are manually
  moved or removed) (#2651)
- Fix a long-standing (highly unlikely) bug in evaluating alternative chains
  that cross a hard fork boundary (#2314, #2318, #2657, #2661)
- Internal support for using multiple leader credentials for the purpose of
  running large scale benchmarks. It is not exposed in the node (#2640).

### ledger changes
- None.

### network changes
- Preparations for publishing io-sim as a public library (#2580, #2649)
- Improved IPv6 support (#2662)
- Preparation for the p2p governor: add a new node-to-node protocol version (4)
  with a new negotiated handshake parameter to determine if the connection will
  be uni-directional or bi-directional (#2658)

## 1.21.1 -- September 2020

### node changes
- Fix configuration defaults that was interfering with using systemd socket
  activation (#1927)
- Workaround for building on Linux systems without systemd (#1775)
- Fix the severity level for some protocol tracers (#1910)
- Add more detail to the logging of the keep-alive protocol (#1873)
- Improve the reporting of failures in the "chairman" CI test (#1916, #1923)

### consensus changes
- None.

### ledger changes
- None.

### network changes
- None.

## 1.21.0 -- September 2020

### node changes
- Mention the RTView component (replacement for LiveView) in node docs (#1866)
- Extensions and improvements to the new node+cli integration tests (#1865,
  #1894, #1897)
- New internal infrastructure for more consistent handling of configuration
  from config files and the command line, using the partial options monoid
  pattern (#1850)
- Remove last appearance of stack build support to avoid confusion (#1896)

### consensus changes
- Avoid the expensive reward calculations while checking if the node is the
  slot leader, which should significantly reduce the CPU spike at the 4k/f
  (2day) point within each epoch (#2642)
- Fix a unlikely-but-possible bug in crossing the hard fork from Byron to
  Shelley (#2455, #2626)
- Initial support for hard forks after Shelley (#2452, #2471)
- Trace interesting ledger events during db replay (#2508, #2621, #2627)
- Preparation for unifying two parts of the ledger db (#2621)
- Remove last appearance of stack build support to avoid confusion (#2638)
- Improvements to the automated tests (#2581, #2643)

### ledger changes
- Support for a low-impact soft fork with stricter metadata validation (#1874)
- Further improvements to the performance of reward calculations and some other
  ledger calculations (#1857, #1881, #1884)
- Fix a bug in the calculation of the pool stake fraction reported in the stake
  pool query used by wallets (#1880)
- Fix a bug for the corner case for testnets where all value is in the reserves
  with no value in circulation (#1876)
- Improved transaction generator (used in tests) (#1865)
- Improved automatic tests for stake pool register/de-register (#1882)
- Minor corrections to the formal spec arising from internal review and internal
  audit (#1776, #1808, #1811, #1820, #1861)
- Update the formal spec with the change in how we calculate the overlay
  schedule (#1862)
- Document in the CDDL chain spec the meaning of the MIR pot field (#1864)
- Updates to the document on the details of pool ranking (#1852)
- Internal technical debt improvements (#1859, #1867, #1871, #1872, #1879)

### network changes
- Improved protocol logging (#2609, #2610, #2618, #2611)
- Fixes to the mux protocol description in the network tech report, and other
  documentation typos (#2625, #2639)
- Adjust the outstanding data limit for the block fetch protocol (#2624)

## 1.20.0 -- September 2020

Note that this release will automatically perform a chain DB migration on
first startup, which can take 10-20 minutes.

### node changes
- Extensions and improvements to the new node+cli integration tests (#1782,
  #1799, #1804, #1806, #1807, #1808, #1812, #1813, #1814, #1816, #1818, #1824,
  #1825, #1826, #1840, #1841, #1846)
- Sanity check the Shelley genesis on node start up to help with configuring
  private test nets (#1149, #1478, #1820)
- Minor logging message improvement for DB events (#576, #1819)
- Tidy up the benchmarking scripts (#1810)
- Add a simple script for a node that connects to the current mainnet (#1847)

### consensus changes
- Fix a bug in the time (slot/era) node query (e.g. affecting db-sync) (#2579)
- Micro-optimisations leading to ~15% sync time improvements (#2555)
- Refactoring to reduce tech debt (#225, #548, #2264, #2513, #2514, #2534,
  #2575, #2604)
- Improvements to the automated Shelley tests (#2462, #2557, #2567)
- Improvements to the automated Praos tests (#2577, #2578)
- Improvements to failure reporting in automated tests to make them easier to
  understand (#2582, #2585)
- API improvements to make it easier for node client to set up the necessary
  configuration to get the initial ledger state and apply blocks (#2593)

### ledger changes
- Use the total stake (not total supply) in the pool stake fraction reported in
  the stake pool query used by wallets, and use the current pool stake rather
  than the stake from the last epoch boundary snapshot (#1836, #1850, #1854)
- New document on the details of pool ranking (#1816)
- Performance improvement in and new micro-benchmarks of the reward calculation,
  to reduce the CPU spike at the 4k/f (2day) point in each epoch (#1851)
- Performance and memory improvement in the overlay schedule (#1849)
- Initial parts of a new scheme to support multiple ledger eras in a single code
  base (#1819, #1826, #1827, #1829, #1837, #1846)
- Initial parts of multi-asset support as a separate era (#1830, #1831, #1839)
- Move code from consensus that should be in the ledger (#1813, #1817)
- Improvements to test tx generation to help with consensus tests (#1824)
- Minor test simplifications (#1841)

### network changes
- Build the design docs in CI and link to them in the README (#2589, #2602)

## 1.19.1 -- September 2020

### node changes
- Code tidying using hlint and style tool (#1726, #1727, #1733)
- Documentation updates (#1718, #1736, #1740, #1741, #1750, #1752, #1757, #1763,
  #1764, #1770)
- Extensions to the new node+cli integration tests (#1716, #1760, #1773, #1774)
- Mention TraceBlockchainTime in config files and documentation (#1786)

### consensus changes
- Fix bugs related to the ledger view history (#1935, #2506, #2559, #2562, #2546)
- Fix the reporting of the metrics for the opcert/KES status (#2529)
- Fix an accidental memory use spike in block streaming (#2532)
- Reduce memory use by not keeping unnecessary AVVM balances (#2533)
- Extend automatic tests to include active stake pools (#2388, #2502)
- Adjust the extended nightly tests for recent ledger updates (#2549, #2550)
- Simplify the management of ledger state snapshots (#2440, #2459)
- Internal code simplifications (#2432, #2538, #2543)
- Preparations for adding the new network connection manager (#2548)
- Rearrange the mock and test packages (#2551)
- Merge and improve the db-validator and db-analyser tools (#2501, #2537)
- Support building with GHC 8.10.x (#2542)

### ledger changes
- Minor performance optimisations (#1790, #1798)
- Extend tests to cover different normal vs address hash algorithms (#1802)
- Minor corrections to the formal spec arising from internal review and internal
  audit (#1791, #1797, #1800, #1805, #1806, #1809
- Support building with GHC 8.10.x (#1807)
- Reduce unnecessary build-time dependencies (#1810)
- Remove unused code spotted by the weeder tool (#1812)
- Minor code rearrangements (#1814)

### network changes
- Publish two previously internal reports on the network design (#2522, #2527)
- Preliminary refactoring to simplify adding the new connection manager (#2541)
- Internal code simplifications (#2556)
- Support building with GHC 8.10.x (#2539)

## 1.19.0 -- August 2020

### node changes
- Code organisation refactoring (#1457, #1594, #1621)
- Fix the live view display of memory metrics (#1552)
- Add the KES current and remaining periods to the live view (#1503)
- Fix incorrect console state after live view shutdown (#1569)
- Minor improvement to rendering of points in unstructured log output (#1693)
- Minor internal cleanup of tracing code (#570, #1619, #1651)
- New node+cli integration tests (#1617, #1644, #1657, #1680, #1683, #1705)
- Improve error messages for problems in the JSON topology file (#1446, #1634)
- Code tidying using hlint and style tool (#1590, #1625, #1663, #1707, #1708)

### consensus changes
- Performance optimisations (#2512, #2520, 2521)
- Fix size accounting in block forging to avoid over-filling blocks (#2469)
- Fix size accounting for block fetching (#2480, #2481, #2484)
- Support to forget old copies of KES keys, pending crypto support (#2446)
- Various internal clean-ups and refactoring (#2446)
- Extended automatic tests to cover more parameters (#2497, #2498, #2235, #2491)
- Improve automatic Shelley consensus tests based on ledger tests (#2490, #2503)
- Drop an unnecessary runtime dependency on the io-sim test library (#2507)
- Allow the db-validator tool to work with networks other than mainnet (#2493)

### ledger changes
- Performance optimisations (#1707, #1760, #1771, #1779, #1785, #1786)
- Additions to enable consensus layer performance optimisations (#1742, #1789)
- Fix pool ranking calculation for newly registered pools (#1724)
- Correct which hash algorithm is used for script hashes (#1746)
- Minor corrections to the formal spec arising from internal review and internal
  audit (#1714, #1717, #1725, #1733, #1728, #1745, #1748, #1751, #1752, #1753,
  #1764, #1765, #1773)
- Minor correction to the CDDL specification for tx metadata (#1743)
- Refactoring to how the tests handle crypto types (#1735, #1739, #1754)
- Extend test tx generator to cover tx metadata (#1731)
- Refactoring in the automatic tests (#1749)
- Reorganise the ledger rule examples (#1741, #1757, #1763, #1778)
- Additional ledger assertions, to use in tests (#1780)

### network changes
- Introduce a new keep-alive mini protocol, but not yet used (#2251)

## 1.18.0 -- July 2020

### node changes
- Properly display tx hash in Shelley UTxO query command output (#1535)
- Documentation improvements (#1533, #1534, #1536, #1537)
- Minor changes to test cases (#1538, #1541)
- Changes to genesis file and NIX setup (#1531, #1532)

### consensus changes
- Account for encoding overhead of Shelley transactions (#2466)
- Optimise translating Byron to Shelley UTxOs (#2464)
- Improve error reporting (#2458)
- Tune defaultDiskPolicy (#2454)

### ledger changes
- Changed the expected block count to account for the decentralisation parameter (#111)
- Replace dependency on lens with microlens (#1705)
- Fix typo in CDDL key name (#1706)
- Enable more efficient  transaction translations (#1708)

### network changes
- Reduce default block fetch concurrency deadline to 1 (#2457)
- Update dependencies with the ledger (#2456)


## 1.17.0 -- July 2020

### node changes
- Fix KES metric reporting in the Cardano mode (#1448, #1505)
- Fix rendering of block hashes in logging output (#1488)
- Trace hard fork transition events (#1520)
- Script for setting up a local cluster in Cardano mode (#1487)
- Documentation reorganisation (#1490, #1491, #1508)

### consensus changes
- Trace hard fork transition events (#2449)
- Refactor HFC serialisation and fix a bug in the serialisation for the
  Shelley-only era  (#2239, #2435)
- Initial infrastructure for supporting old transactions in new eras, for
  future hard forks (#2371, #2431)
- Add extra long-running nightly CI tests (#2393, #2398)

### ledger changes
- Unclaimed rewards go back to reserves, not the treasury (#1703)
- Fix an accidental change to the delegation update rule arising from previous
  optimisation work (#1701)
- Fix serialisation of protocol state to be valid CBOR (#1684)
- Memory use optimisations (#1678, #1683)
- Improve performance of a particular calculation in the rules (#1700)
- Improve clarity between executable and formal spec, arising from audit
  feedback (#1685, #1687, #1690, #1694)
- Remove stkCreds and stpools maps from formal spec, in line with
  simplifications from the executable specification (#1692)

### network changes
- Infrastructure for showing the type of protocols in errors (#2419)
- Adjust the local state query wrapper type as needed by db-sync (#2437)
- New tip-sampling mini-protocol for later use in P2P governor (#2340)
- Block fetch improvements (#2430, #2433, #2434, #2441, #2451)

## 1.16.0 -- July 2020

### node changes
- New config param to specify max concurrency of block downloads (#1420, #1469)
- Single-era modes now use the hard-fork combinator for consistency (#1475)
- The initial Praos epoch nonce is now set to the Shelley genesis hash (#1470)
- Changes from refactoring in the API and config libraries (#1422, #1444)

### consensus changes
- Add new local state queries specific to the hard fork support (#2365, #2370)
- Support hard fork queries in all protocol modes (#2399)
- Fix query compatibility between Cardano and Byron modes (#2361, #2385)
- Use the hard-fork combinator for single-era modes (#2405, #2407, #2412, #2414)
- Use slightly smaller KES keys with 2^6 not 2^7 max periods (#2403)
- Improve performance of syncing in Cardano mode (#2375, #2390)
- Use a smaller representation for hashes for reduced memory use (#2266)
- Improve chain selection across future hard forks (#2118, #2416)
- Identify and warn about a likely hard fork misconfiguration (#2386, #2391)
- Add validation for Shelley genesis configurations to avoid mistakes (#2423)
- Test improvements (#1533, #2366, #2130, #2362, #2377, #2361, #2385)
- Extend automated tests to cover Shelley d=0 environments (#2378)
- Extend db-converter tool for use in regression tests (#2369, #2375)
- Internal refactoring (#2036, #2372, #2354, #2357, #2380, #2345, #2381)

### ledger changes
- Change the tx size definition to simply be its size in bytes (#1639)
- Finalise the format and specification of Byron address witness (#1657, #1670)
- Unclaimed epoch pool rewards go to the treasury not the reserves (#1642)
- Limit the sizes of attributes in Byron addresses (#1662)
- Simplify the calculation of the VRF seed (#1659)
- Fix the selection of the epoch nonce (#1651)
- Eliminate protocolMagicId from the Shelley genesis file (#1668)
- Include in the pool ranking function whether the pool pledge is met (#1634)
- Performance optimisation for large UTxO and other state sizes (#1658)
- Memory use optimisations for the UTxO (#1663)
- Audit of uses of serialisation with hashing and signing (#1613, #1659, #1666)
- Additional tests (#1606, #1640, #1661)
- Add ability to run with STS assertions enabled (#1610, #1629, #1672)
- Clarify design specification on how pool pledges are enforced (#1611)
- Fix minor design specification description mistake (#1612)
- Clarifications and fix typos in the formal spec (#1618, #1655)
- Fix the documentation of the sizes of key hashes (#1622)
- Improve the README description the main design and spec documents (#1626)

### network changes
- Refactor how the network protocol versioning is managed (#2358)
- Improved error messages for protocol codec failures (#1964, #2360)
- Make the max concurrency of block downloads be configurable (#2363)
- Enable the keep-alive responder-side protocol handler (#2392)

## 1.15.1 -- July 2020

No changes in the node. There were changes in the cardano-api and cardano-cli.

## 1.15.0 -- July 2020

### node changes
- Support for triggering a hard fork at a specific epoch (#1328)
- Support for triggering a hard fork at a specific protocol version (#1345)
- Changes resulting from refactoring in the Cardano API library (#1289, #1316,
  #1380, #1384)
- Update the README build instructions, including libsodium (#1325, #1361, #1362)
- Trace the UTxO size in block forging to help the benchmarking tools (#1329)
- Remove the "forks created" metric from the live view (#1315)

### consensus changes
- Fix an off-by-one error in the KES cert validity period (#2306)
- Fix KES-related tests (#2306)
- Fix bugs found by the hard fork tests (#2310, #2312)
- Fix bugs found by the hard fork tests in the hard fork tests themselves (#2305)
- Extend hard fork tests to include network partitions at interesting times
  around or during the hard fork (#2292, #2337, #2342)
- Prefer blocks we created, irrespective of the VRF value (#1286, #2348)
- Allow setting the intial Praos nonce (#2005, #2289)
- Richer support for queries when using multiple protocol eras to allow some
  queries to be answered outside of the era to which they belong (#2349)
- Optimisation in chain selection for forks (#1223, #2323)
- Preparation for removing EBBs (#2322, #2336, #2339)
- Clarify and document the interface for triggering a hard fork (#2307)
- Internal cleanups and refactoring (#2327, #2328, #2329)

### ledger changes
- Fix bug in the pool reaping that could cause a crash on epoch boundaries (#1593)
- Fix a preservation of value bug in rewards to retired stake pools (#1598, #1604)
- Fix a bug in pool reaping when multiple pools share a reward account (#1605)
- Fix the calculation of non-myopic rewards that is queried by the wallet (#1601)
- Allow update proposals to be submitted near the end of an epoch, but apply
  them only at the end of the following epoch (#1574)
- Simpler calculation for turning the VRF output into the leader value (#1579)
- Check the VRF for BFT blocks too (#1590)
- Tests and other clean-up for witnesses for spending from Byron address in
  Shelley transactions (#1573, #1591)
- Additional tests for serialisation of parts of the ledger state (#1603)
- Fix the presentation of pool metadata hashes in JSON output (#1596)
- Clean up the serialisation used with hashing (#1602)

### network changes
- Additional concurrency utilities (#2298)
- Improved tests for error handling of socket send/recv (#2317)
- Port existing DNS-related bug fixes to the new DNS provider in the p2p
  governor (#1873, #1893, #2311)
- Internal cleanups (#2096)

## 1.14.2 -- June 2020

No changes in the node. There were changes in the cardano-api and cardano-cli.

## 1.14.1 -- June 2020

- Include the libsodium.dll info Windows build artefacts (#1327)
- Fix compiler warnings on Windows (#1303)
- Split live view code into more modules (#1323)

## 1.14.0 -- June 2020

### node changes
- Initial integration of the "Byron;Shelley" composite protocol (#1242, #1294)
- Fix a minor regression in the logging so all tracers are configurable (#1192)
- Minor adjustment to logging verbosity defaults (#1225)
- Log warnings and alerts for certificate expiry (#1228)
- Report number of missed opportunities to make blocks in the live view (#1202)
- Allow overriding the hostname shown in logs with an env var (#1278)
- Fix reporting of git revision (#1263)

### consensus changes
- Refactoring of serialisation and other details for the Byron-to-Shelley
  hard-fork (#2147, #2207, #2224, #2226, #2237, #2240, #2241, #2243, #2244, #2246,
  #2248, #2250, #2252, #2263)
- Extend the automated testing (#2027, #2029, #2066, #2222, #2277, #2282, #2293)
- New common protocol params interface for querying ledger-specific params (#2275)
- Change the ledger state dump query to return the whole state (#2288)

### ledger changes
- Remove the decaying deposits feature. Deposits are returned in full. (#1512)
- New feature for a minimum pool cost, via updateable protocol parameter (#1546)
- Change address hashes to be 224 bit, as in the design spec (#1549)
- Track historical pool performance for pool ranking (#1565)
- Support pool ranking with hypothetical amount to delegate (#1543)
- Remove unnecessary pool relay port number override in DNS SRV case (#1570)
- Move genesis JSON support into the ledger (#1534, #1536)
- Add genesis JSON support for initial stake pools and delegation (#1552)
- Fix genesis JSON conversion of fractional values to preserve precision (#1569)
- Benchmarks and optimisations (#1527, #1566)
- Update to latest VRF API changes (#1577)
- Update the formal specification to cover MIR drawing from the treasury (#1526)
- Update the design specification address terminology, and multi-sig (#1547, #1554)
- Update the design specification on stake pool metadata (#1507)

### network changes
- Timeouts for chain sync are drawn from a Praos-specific distribution (#2249)
- New keep-alive mini-protocol (#2230)
- New support for restarting mini-protocols (#2205, #2221, #2286)
- Testing for bi-directional use of mini-protocols (#2203)
- Adjust and simplify the block fetch calculation for determining if we are in
  deadline mode or in bulk sync mode (#2267)

## 1.13.0 -- June 2020

### node changes

- Fix override order for the node socket: CLI then config file (#1124, #1145)
- New monitoring metric for KES key periods, including remaining (#1030, #1144)
- Improved logging for mempool tx validation failures (#1035)
- Log the epoch number as well as the slot number (#1095, #1108)
- Fix logging of node info on startup (#1122)
- Extended tracing for benchmarking analysis (#1091)
- Fix error handling on genesis file parse errors (#1134)
- Various live view fixes (#1076, #1126)

### consensus changes
- Hard fork related refactoring and improvements (#2139, #2150, #2151, #2152,
  #2158, #2165, #2177, #2183, #2192, #2199)
- Preparation for the Byron-to-Shelley hard-fork (#2163, #2145, #2182, #2185,
  #2187, #2189)
- Basic tests for the hard fork combinator (#2063, #2079, #2134, #2167, #2197)
- Deterministic Praos chain selection using the leader VRF (#2195)
- Fix Praos stability window calculations (#2201, #2202, #2219, #2220)
- Add more local ledger state queries (#2161)
- Mempool refactoring and fixes (#1498, #2006, #2153)
- Evolve the KES keys eagerly rather than on-demand (#2160)
- Extended documentation of the "ThreadNet" consensus tests (#2053)
- Distinguish cases when a node should be slot leader but cannot forge (#2169,
  #2171, #2172, #2176, #2193)
- Improved tracing of block forging, and forge errors (#2168, #2170)

### ledger changes (Byron)
- None

### ledger changes (Shelley)
- Give delegators a whole epoch to react to pool parameter changes (#1494)
- Fix off-by-one error in pool retirement epoch (spec and impl) (#1511)
- Genesis delegate mapping now includes the VRF key hash (#1495, #1501)
- MIR certs can now draw from the treasury or reserves (#1513)
- MIR certs are witnessed by the genesis delegates not genesis keys (#1513)
- Drop the use of the "set" tag from the CDDL (#1505)
- Use the right binary format for stake addresses (#1491)
- Include the network id in addresses (by value, not type) (#1487)
- Document the network id and binary format in the specification doc (#1500)
- Move serialisation property tests from ouroboros-consensus (#1497, #1506)

### network changes
- Increase the header size estimate to avoid ingress queue overruns (#2206)
- Mux refactoring for on-demand and bi-directional mini-protocols (#1731, #2166)
- Fix long thread delays on 32bit systems more widely (#2135, #2162)
- New keep alive mini-protocol (#2175)
- Refactoring of OuroborosApplication type (#2121)
- Allow compilation with GHC 8.10.x (#2141)

## 1.12.0 -- May 2020

### node changes

- Support initial Shelley protocol parameters in the Shelley genesis (#906)
- New `--shutdown-on-slot-synced` flag for benchmarking and CI tests (#776)
- systemd socket activiation support (#856, #1022)
- systemd support on Linux is now optional (#911)
- Minor improvement to `FetchDecision` tracing output (#863)
- Improved suppression of frequent repeated log messages (#842)
- Trace the node uptime (#892)
- Trace the number of transactions processed and blocks forged (#937, #943)
- Trace the current set of node peers for the live view (#1007)
- Fix structured tracing of txs within adopted blocs for analysis tools (#993)
- Log the node's network magic number on startup (#921)
- Adjust configuration to keep metrics out of the log files (#930)
- Added OSX (Darwin) support for OS metics in the "live view" (#888)
- Various live view fixes (#964, #965, #974)

### consensus changes
- Hard fork support, and related refactoring (#1994, #1999, #2003, #2020,
  #2021, #2037, #2042, #2057, #2059, #2064, #2065, #2070, #2088, #2093, #2100,
  #2102, #2103, #2122, #2128, #2034)
- Improve DB file locking behaviour (#1906, #2031, #2033)
- Memory leak detection for Shelley transactions (for mempool) (#2010)
- Tests for the Shelley consensus protocol with d < 1 (#1991, #2105)
- Split Shelley tests into a separate test library (#2129)
- Fix the construction of the Shelley initial UTxO (#2038, #2046)
- Include the Shelley protocol parameters into the Shelley genesis (#2040)
- Use an improved more realistic KES implementation (#2092)
- Fix handling of operational certs for KES validity periods (#2092, #2094)
- Shelley-compliant handling of OCert issue number in chain selection (#2108)
- Provide interface for node clients needing less configuration (#2050, #2056)
- Local state query for UTxO, whole or filtered (#2016, #2110)
- Improve calculation of tx sizes (#2114, #2115, #2116)
- Fix long thread delays on 32bit systems (#2095)

### ledger changes (Byron)
- Repository merged with Shelley ledger repository
- Expose bits of the ledger state needed for the node live view (#772).

### ledger changes (Shelley)
- Relay nodes' IP/DNS included in pool reg certs (#1391, #1397, #1398, #1413)
- Make the active slot coefficient not an on-chain updateable parameter (#1394)
- Simplified specification and implementation of tx size calculation (#1405)
- Fix tx signing in spec and impl to sign the body hash, not body (1418)
- Change the definition of total stake slightly (#1428, #1437)
- Support for minimum utxo outputs, aka non-decaying deposits (#1447)
- Decide final network Id number assignments in address format (#1465)
- Document specific crypto choices in the specification appendix (#1389)
- Local state query for querying the UTxO, all or by address (#1402, #1422)
- Local state query for dumping whole ledger state for test and debug (#2113)
- Complete the support for caching serialised representation (#1380, #1408)
- Improved test coverage (#1387, #1393)
- Fix handling of the overlay schedule when there are no BFT nodes (#1401)
- Eliminate use of partial functions (#1407, #1477)
- Internal improvements to handling of crypto key roles (#1410)
- Improved details in ledger validation error reports (#1442, #1458, #1463,
  #1469, #1470, 1476)
- Refactor PRTCL state transitions for better consensus integration (#1450, #1457)
- Initial support for Byron addresses (#1473)

### network changes
- Support for using existing sockets, for systemd socket activation (#1995)
- Use TCP_NODELAY for faster block relaying (#2069)
- Add a new cardano-client package to provide a simpler node client API (#2109)
- Ability to shut down tx submission protocol cleanly by receiving side (#2090)
- Make protocol handlers depend on the negotiated protocol version (#2090)
- Split up the new peer selection into more manageable modules (#2018)
- Refactoring of protocol handshake (#2044)

## 1.11.0 -- April 2020

### node changes
- Shelley preparatory refactoring (#775, #792, #807)
- Shelley genesis file support (#798)
- Initial support for starting a node in Shelley mode (#820)
- Initial support for loading Shelley node leader credentials (#832)
- Split out CLI functionality into cardano-cli package (#819)
- Support for reading/writing readable Shelley key file formats (#826, #829)
- Option for log output in readable text form, rather than structured (#760)
- Performance improvements in logging/monitoring system (#831)
- Suppress high frequency logging output during syncing (#773)
- Improve example configuration for "live view" (#827)
- README improvements (#793, #828)

### consensus changes
- Refactoring in preparation for hard fork support (#1946, #1942, #1968, #1973)
- Shelley ledger integration improvements (#1963, #1821, #1967, #1984, #1986)
- Verify no excessive memory use in Shelley ledger state (#1558, #1928)
- Verify no excessive memory use in inbound transaction path (#1645, #1708)
- Improve restart times by storing a ledger snapshot after a long replay (#1956)
- Move immutable chain db files from ".epoch" to ".chunk" (#1755, #1954)
- Minor disk I/O optimisation when serving headers and blocks (#1978)

### ledger changes (Byron)
- None

### ledger changes (Shelley)
- Avoid excessive memory use in ledger state by using normal form types (#1343)
- Keep serialised forms of transactions and tx sequences (#1361, #1377, #1379)
- Minor changes to the CBOR CDDL binary schema (#1376)
- Adjust the address type to better reflect the logical structure (#1370)
- Improve the representation of the overlay schedule (#1378)
- Various exports needed for node and CLI integration (#1370, #1373, #1375)

### network changes
- Enable the local state query protocol with a V2 local IPC protocol (#1522)
- Fix handling of unknown versions in protocol version negotiation (#1981)
- API improvements for the local node IPC protocol (#1950, #1959, #1962)
- DNS error handling improvements during temporary network outages (#1961)
- Add timeouts on message sends, rather than just receive (#1953)
- Unified library API docs https://input-output-hk.github.io/ouroboros-network/

## 1.10.1 -- April 2020

### node changes
- None

### consensus changes
- Improve chain DB concurrency during syncing by batching GC actions (#1932)

### ledger changes
- Exemptions for historical null update proposals on staging blockchain (#768)
- Document Byron-era protections against network address confusion (#727, #755)

### network changes
- Windows sync performance improvements with new timeouts implementation (#1931)
- Fix handling of negative timeouts in the IO simulator (#1939)

## 1.10.0 -- April 2020

### node changes
- Change interpretation of relative paths in the config file (#750)
- Fix activation of the TraceForwarder logging system plugin (#743)
- Added a cross-platform clean shutdown mechanism (#767)
- Log node version on startup (#757)
- Minor simplifications of tracing output and internals (#763, #768)
- Improved docs for building and running (#718, #752)

### consensus changes
- Performance improvement in sync from improved hash representation (#1887)
- Chain DB locking improvements for better concurrent performance (#1816, #1866, #1907, #1919)
- Further test coverage for recovery from chain DB corruption (#1628, #1839)
- Disable support for rewinding system clock to simplify hard fork support (#1908)
- Improved tests for hard fork history support (#1888, #1898)
- Refactoring of time support in preparation for hard fork support (#1918)
- Internal simplifications in the chain DB (#1890)
- Improved tracing (#1861, #1895)
- Label consensus threads for more useful performance profile output (#1916)

### ledger changes
- Fix mismatch with specs with update proposal version numbers (#759, #766)
- Performance improvement in validation from improved hash representation (#760)

### network changes
- Improved Windows IO mangager exception handling (#1897)
- Handle DNS server changes due to switching network connection (#1891)
- Numerous small memory-use reductions in low level network code (#1902)
- Label network threads for more useful performance profile output (#1900)
- Extensions to a simplified library interface for node clients (#1894)

## 1.9.3 -- March 2020

### node changes
- None.

### consensus changes
- None.

### ledger changes
- None

### network changes
- Fix Windows named pipes bug that limited node client connections (#1876)
- More reliable establishment of connections with Windows named pipes (#1881)
- Workaround DNS timeout problems on Windows (#1873, #1878)
- Add a timeout in one state of the chain sync protocol (#1871)

## 1.9.2 -- March 2020

### node changes
- None.

### consensus changes
- None.

### ledger changes
- Fix off-by-one mismatch in the minimum transaction fee (#756, #757)

### network changes
- None.

## 1.9.1 -- March 2020

### node changes
- CLI support for submitting Byron era update proposals (#714)

### consensus changes
- None.

### ledger changes
- None.

### network changes
- Fix failures in the NTP client on Windows (#1862, 1865)

## 1.9.0 -- March 2020

### node changes
- Remove CLI override for genesis file. It must be in the config file. (#683)
- Genesis file path in config relative to the config file location (#648)
- For security adjust default config to not listen on local ports (#707)
- Use new DNS relay pool in default mainnet configuration (#708, )
- CLI support for creating Byron era update proposals (#696)
- Improved Windows CI (#669, #685)
- More robust "chairman" integration test (#681)
- Documentation updates (#677,709)
- Progress meter in monitoring output for long running block replay (#712)
- Improved trace verbosity for block fetch decisions (#701)
- Improved trace output for mux timeouts (#717)
- Improved trace output for mempool events (#724)
- Improved trace output for subscription and DNS lookups (725)

### consensus changes
- Shelley ledger (#258, #982, #1403, #1405, #1820, #1822, #1824, #1695)
- More internal framework to support hard forks (#1698)
- Updated Shelley support for the local state query (#1848)
- Fix subtle block fetch concurrency bug found by QC tests (#1845, #1850)
- Tests for Byron era software update proposals (#1733, #1790)
- Tests for transactions with TTLs as used by Shelley (#1297, #1564)
- Minor optimisation in storage layer (#1810)

### ledger changes
- More tests for mempool validation (#707, #748)
- Export the mainnet protocol magic id, for convenience (#750)
- Test clusters started in OBFT mode have an EBB matching the legacy one (#751)
- Adjust the update proposal endorsement rule to simplify hard forks (#753)

### network changes
- Significant update to the Win32 network I/O library (#1574, #1627, #1844)
- Limit the number of accepted connections (#1391, #1831)
- Protocol timeouts, per-protocol state (#1395, #1813)
- Add a minor missing check in transaction submission protocol handler (#1856)
- Improve sync performance for far away nodes with high network latency (#1843)
- Improved selection of nearby low latency peers (#1858, #1859, #1860)
- Improved Windows CI (#1808)

## 1.8.0 -- March 2020

### node changes
- Move genesis file and socket path items into config file (#612)
- Remove need for unnecessary flags in various CLI commands ()
- New `pretty-print-cbor` and `validate-cbor` commands in CLI (#545, #637)
- Fix configuration for routing of metrics to monitoring backends (#622, #659)
- Improve default configuration for logging filters (#633)
- Improve tracers for logging and benchmarking (#624, #653, #663, #664, #655, #670)
- Build fixes for Windows (#646)
- Remove wallet demo code (#665)

### consensus changes
- Improved consensus tests that found numerous subtle bugs (#1489, #1506, #1524)
- Improved tests for protocol version updates (#1514, #1747)
- Fix error classification policies (#1553, #1738)
- Fix a couple resource management bugs (#1787, #1792)
- Internal changes to prepare for the Shelley hard fork (#1750, #1775)
- Squash accidental memory retention in a couple places (#1718, #1793)
- Numerous internal improvements and refactoring (#1739, #1742, #1744, #1748, #1761, #1780)
- Drop dependency on OpenSSL, allowing easier builds on Windows and ARM (#1774)

### ledger changes
- Fix calculation of transaction size to match legacy Byron implementation (#703)
- Fix replay protection for votes on protocol updates (#739)
- Fix minor mismatch vs specification on tx size in a block (#718, #742)
- Drop dependency on OpenSSL, allowing easier builds on Windows and ARM
- Clarify code for genesis generation to address external audit concerns (#732)

### network changes
- Add timeouts for the handshake phase in the node-to-node protocol (#1752)
- Fix race condition with 0-length SDUs (#1730)
- Reduce fetching of duplicate transactions (#1749)
- Build fixes for Windows (#1791)
- Improved tracing and error reporting (#1762)
- Vectored async IO for Windows network sockets (#1552)
- Minor clean ups for the NTP client library used by the wallet (#1788)

## 1.7.0 -- February 2020

### node changes
- New CLI command to get the node tip (#591)
- Fix the unstructured message output for journald (#590)
- Add structured logging for TxSubmission (#584)
- Add logging detail for TraceForgedInvalidBlock (#605)
- Allow avoiding building the wallet client demo and chairman tool (#597)

### consensus changes
- Split consensus library: generic, byron, mock, testing  (#1651, #1652)
- Internal refactoring to prepare for the hard fork combinator (#1674, #1679)
- Improve use of PRNGs (#1554, #1616, #1678, #1677)
- Simplify the test infrastructure for the volatile DB (#1639, #1680)
- Fix an EBB-related bug (#1690, #1691)
- Fix a bug and improve performance in the chain db adding blocks (#1463, #1709)

### ledger changes
- none

### network changes
- Integrate Windows IOCP-based async I/O, with abstraction layer (#1499)
- Refactoring of the mux, in preparation for bi-directional connections (#1687)
- Trace transaction flow in tx submission for tx system tracing (#1688)
- Fix block fetch bug found by tests (#1147, #1705)
- Introduce ouroboros-network-framework library and rearrange modules (#1655)
- Refactor protocol bundle API (#1655)

## 1.6.0 -- February 2020

### node changes
- Improve node CLI for real protocol vs mock protocols (#297, #314, #318, #335)
- Improve log output for normal block forging and errors (#537)
- Improve log output for normal mempool events (#527, #538)
- Remove redundant `--genesis-hash` flag from `cardano-cli` (#540)
- Move benchmarking CLI commands to their own sub-group (#540)
- Extend tx generator to be able to use the tx submission HTTP API (#549)
- The "live view" now displays the node id (#534)
- Report program version via logging and monitoring system (#542)
- Disable SMP on ARM CPUs due to an RTS ARM SMP bug in GHC 8.6.x (#560)

### consensus changes
- Fix bugs found by tests related to block number handling (#1578, #1584, #1589)
- Better handling of the block number at the genesis (#1585, #1594, #1595, #1597)
- Fix bugs found by other tests (#1543, #1547, #1559, #1562, #1511, #1544)
- Fix bugs found by dual ledger tests (#1608, #1571, #1617, #1577)
- Fix a number of EBB-related bugs (#1620, #1621, #1624, #1625)
- Introduce more sophisticated mempool tests and fix bugs (#1301, #1565, #1599)
- Add tests for unusual changes in wall clock time (#759, #1554, #1601)
- Handle restarting after wall clock time has been moved back (#1550, #1563)
- Use specific program exit codes for node chain db errors (#1201, #1541)
- Revalidate chain db files after unclean shutdown (#1551, #1623, #1304, #1568)
- Add ability to report mempool capacity in tracing (#1509, #1510)
- Add better support for versioned network protocols (#1632)
- Fix minor space leaks (#1602, #1605)

### ledger changes
- Fix rare bug in validation of delegation certs in the mempool (#715, #716)
- Fix a space leak (#717)
- API refactoring (#722)
- Move code from consensus that should be in the ledger library (#676)
- Clean up LovelacePortion representation and API
- Add generic derived JSON instances for downstream users
- Switch to Apache 2.0 license

### network changes
- New NTP client time check library for the wallet (#1327)
- Rearrange and move modules between network libraries (#1561)
- Minor bug fixes in Win32 async I/O code (#1573, #1576)
- Improve `io-sim-classes` support for monad stacks (#1539)

## 1.5.0 -- January 2020

### node changes
- Eliminate a space leak by replacing the Prometheus monitoring backend (#491)
- Change cardano-cli transaction format to be the raw chain format (#529)
- Add checks to prevent space leaks in the node console "live view" (#500)
- Improve bulk sync performance by adjusting default RTS options (#506)
- Adjust the default set of enabled tracers (#494)
- Allow logging output to journald on Linux (#502)
- Show network status information in the node console "live view" (#493)
- Set PBFT signature threshold to the default value in the config files (#452)
- Blank fields in config files use default values (#453)
- Add tracers for the benchmarking of block forging (#464)
- Remove unused code in the configuration and CLI code (#482)
- Improve documentation of transaction creation (#497)
- Improvements to the benchmarking transaction generator (#505)

### consensus changes
- Implement consensus support for local state query protocol (#1366, #1507)
- Set default mempool size as twice the max block size (#1467, #1468)
- Fix an EBB-related bug in the chain DB iterators (#1435, #1475)
- Correct the implementation PBFT window check to match specification (#1480)
- Correct the size calculation of Byron txs in the mempool (#1535, #1540)
- Improve the node shutdown to close things in the right order (#1470, #1488)
- Optimise adding blocks to the chain DB (#1398)
- Better ledger DB snapshot policy for faster node startup (#1264, #1456, #1518)
- Add conformance testing against ledger executable spec (#1425, #1503, #1517)
- Make the components within the chain DB share a common API (#1372, #1471)
- Refactor consensus protocol type classes (#1527, #1534)
- Internal refactoring (#1497)
- Improve error messages referring to chain DB files (##305, #1529)
- Improvements to test code (#1180, #745, #1479, #1523, #1537)

### ledger changes
- none

### network changes
- Add Windows IOCP-based async I/O for sockets and named pipes (#738, #1423)
- Update to network-3.1 library, and related libraries (#1423)
- Simplify tracing in the typed protocol drivers (#1481)
- Refactor the network mux library (#1494)
- Reorganise ouroboros-network modules (#1519)
- Make the maximum concurrency in block fetch configurable (#1525)
- Improve syncing performance by avoiding concurrent block fetch for now (#1525)

## 1.4.0 -- January 2020

### node changes
- Move configuration of tracers from CLI to the config file (#474)
- Move support for trace forwarding into a logging plugin (#454)
- Make the launcher scripts able to be used with nix, cabal or stack (#458)
- Fix non-liveview mode in shelley-testnet.sh script (#459)
- Elide repeated log messages (#445)
- Simplify cardano-cli interface (#476)
- Remove unneeded cardano-cli dump-hardcoded-genesis subcommand (#451)
- Remove dependency on cardano-ledger-test (#451)
- Remove message counters from config files (#454)
- Add the mempool size metric to the console live view (#457)
- Update scripts and README

### consensus changes
- Limit forged block size based on current limits from the ledger state (#1400, #1363)
- Add ability to get recent ledger states, for local query protocol (#1440, #1446)
- Refactor block forging code (#786, #1445)
- Fix rare bug in block forging (due to unavoidable race condition) (#1437, #1459)
- Fix a case of dubious async exception handling in chain DB (#1452, #1453)
- Additional tests to better cover EBBs in combination with PBFT consensus. (#1353)
- Various tidying up in the consensus QC tests. (#1401)
- Allow disabling assertions for production builds (#1248)
- Add support to get mempool snapshot size for use in system benchmarks (#1431)
- Adjustments to tracing in block forging for system benchmarks (#1432)

### ledger changes
- Relax the validation rule for on-chain registered software versions to better
  match the legacy implementation. This fixes validation of the testnet.

### network changes
- Significant refactoring of network-mux package (#1247)
- Reduce CPU cost of sending over the mux (approx 10%) (#1420, #1434)
- Simplify IOSim's Async representation (#1394)

## 1.3.0 -- January 2020

### node changes
- Update to latest dependencies (consensus, ledger, logging etc)

### consensus changes
- Add initial support for multiple slot lengths (for hard forks) (#282, #1385)
- Do chain selection based only on the latest header in each chain (#1227)
- Significant performance improvements in serving headers and blocks (#1378)
- Snapshot ledger state on shutdown to avoid long restart times (1388)
- Fix garbage collection of previously applied points in the ledger DB (#1381)
- Fix unnecessary memory retention in the volatile DB indices (#1379)

### ledger changes
- Update to latest version of cardano-ledger-specs

### network changes
- None

## 1.2.0 -- December 2019

### node changes
- Update to latest dependencies (consensus, ledger, logging etc)
- More monitoring counters/statistics, including Prometheus output (#366)
- Remove unused legacy and wallet configuration fields (code and config files)
- Improve README files
- Hide tracing options from default `--help` command
- Fix flakeyness in logging setup & shutdown
- Stop message counter messages from appearing in log files
- Refactor CLI and config parser code.

### consensus changes
- Improve chain sync serving performance by binary streaming of headers (#1330)
- Much more reliable detection of disk corruption in epoch files (#290, #1253)
- Limit the size of forged blocks (#686)
- Change mempool capacity from number of transactions to size in bytes (#974)
- Set node's default mempool capacity to 2x the mainnet block size
- Avoid logging messages about block forging for nodes that do not forge
- Allow starting before genesis start time by waiting, and log message
- Fix a number of bugs related to EBBs, found by QC tests
- Improved the QC test case generators to cover EBBs better
- Fix a memory retention bug and make thunk detection tests pass
- Use file locks for the chain DB (#1266)
- Get the slot length from the genesis file (#1345)

### ledger changes
- Remove support for HD addresses (not needed by the ledger, just wallets)
- Remove unnecessary SafeSigner abstraction
- Remove unnecessary EncryptedSigningKey
- Remove dependency on scrypt
- Add tests for isRedeemAddress, improve address encoding/decoding

### #network changes
- Added initial peer-to-peer governor with QC tests. Not yet used.

## 1.1.0 -- December 2019

### node changes
- Updated to latest consensus and network versions
- Script to connect to mainnet using deployed mainnet relays
- CI integration test for mixed cluster of old cardano-sl nodes and new nodes
- Improved CI "chairman" integration test
- Improved CLI and config file handling
- Adjusted log severity levels for many trace messages
- Better default RTS flags
- New --validate-db flag to revalidate all on-disk database files
- Updated README instructions

### consensus changes
- Adjusted the dividing line between ledger and consensus for block production
  code for clearer structure and so features are tested in the right place.
- Progress on refactoring needed to support the hard fork protocol combinator.
- Serve blocks as binary blobs without deserialising for improved performance.
- Check checksums when reading blocks to detect disk corruption.
- Finish feature to support accepting blocks from the near "future", once the
  local time catches up. This gives a degree of lenience for clock skew, while
  still respecting the Ouroboros rule of "blocks from the future" being invalid.
- Added more extensive QuickCheck tests for BFT consensus.
- Fixed bugs identified by QuickCheck state machine tests.
- Improvements to the API of the IO simulator.
- Trace the reason for a known block being invalid when rejecting a header.
- Add additional trace points.

### network changes
- Simplified API to network layer used by consensus and node clients.
- Documented wire format of the local transaction submission protocol.
- Added infrastructure to support size and time limits in mini-protocol driver.

## 1.0.0 -- November 2019

- Complete rewrite compared to previous cardano-sl series.

- New modular design. The cardano-node is the top level for the node and
  aggregates the other components from other packages: consensus, ledger and
  networking, with configuration, CLI, logging and monitoring.

- The node no longer incorporates wallet or explorer functionality. The wallet
  backend and explorer backend are separate components that run in separate
  external processes that communicate with the node via local IPC.
