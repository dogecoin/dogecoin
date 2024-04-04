RPC method maturity
-------------------

This document records the maturity of each method made available by the Dogecoin
Core RPC. Maturity is expressed over 3 stages:

1. UNSTABLE - These methods are often new, and can change within minor version
   updates.
2. STABLE - These methods can be used safely and will not see breaking changes
   on minor version updates.
3. DEPRECATED - These methods are no longer recommended to be used and may be
   removed in future versions.

-------

| method                 | Maturity   | Comment                                    |
|------------------------|------------|--------------------------------------------|
| abandontransaction     | STABLE     |                                            |
| addmultisigaddress     | STABLE     |                                            |
| addnode                | STABLE     |                                            |
| addwitnessaddress      | UNSTABLE   | Not functional yet                         |
| backupwallet           | UNSTABLE   | Breaking change since 1.14.6               |
| bumpfee                | STABLE     |                                            |
| clearbanned            | STABLE     |                                            |
| createauxblock         | STABLE     |                                            |
| createmultisig         | STABLE     |                                            |
| createrawtransaction   | STABLE     |                                            |
| decoderawtransaction   | STABLE     |                                            |
| decodescript           | STABLE     |                                            |
| disconnectnode         | STABLE     |                                            |
| dumpprivkey            | STABLE     |                                            |
| dumpwallet             | UNSTABLE   | Breaking change since 1.14.6               |
| encryptwallet          | STABLE     |                                            |
| estimatefee            | UNSTABLE   | Brought back from deprecation since 1.14.7 |
| estimatepriority       | DEPRECATED | To be replaced with estimatefee            |
| estimatesmartfee       | UNSTABLE   | Brought back from deprecation since 1.14.7 |
| estimatesmartpriority  | DEPRECATED | To be replaced with estimatesmartfee       |
| fundrawtransaction     | STABLE     |                                            |
| generate               | STABLE     |                                            |
| generatetoaddress      | STABLE     |                                            |
| getaccount             | DEPRECATED | Deprecated since 1.14.0                    |
| getaccountaddress      | DEPRECATED | Deprecated since 1.14.0                    |
| getaddednodeinfo       | STABLE     |                                            |
| getaddressesbyaccount  | DEPRECATED | Deprecated since 1.14.0                    |
| getauxblock            | STABLE     |                                            |
| getbalance             | STABLE     |                                            |
| getbestblockhash       | STABLE     |                                            |
| getblock               | STABLE     |                                            |
| getblockchaininfo      | STABLE     |                                            |
| getblockcount          | STABLE     |                                            |
| getblockhash           | STABLE     |                                            |
| getblockheader         | STABLE     |                                            |
| getblockstats          | UNSTABLE   | Introduced in 1.14.7                       |
| getblocktemplate       | STABLE     |                                            |
| getchaintips           | STABLE     |                                            |
| getconnectioncount     | STABLE     |                                            |
| getdifficulty          | STABLE     |                                            |
| getinfo                | DEPRECATED | Deprecated since 1.14.0                    |
| getmemoryinfo          | STABLE     |                                            |
| getmempoolancestors    | STABLE     |                                            |
| getmempooldescendants  | STABLE     |                                            |
| getmempoolentry        | STABLE     |                                            |
| getmempoolinfo         | STABLE     |                                            |
| getmininginfo          | STABLE     |                                            |
| getnettotals           | STABLE     |                                            |
| getnetworkhashps       | STABLE     |                                            |
| getnetworkinfo         | STABLE     |                                            |
| getnewaddress          | STABLE     |                                            |
| getpeerinfo            | STABLE     |                                            |
| getrawchangeaddress    | STABLE     |                                            |
| getrawmempool          | STABLE     |                                            |
| getrawtransaction      | STABLE     |                                            |
| getreceivedbyaccount   | DEPRECATED | Deprecated since 1.14.0                    |
| getreceivedbyaddress   | STABLE     |                                            |
| gettransaction         | STABLE     |                                            |
| gettxout               | STABLE     |                                            |
| gettxoutproof          | STABLE     |                                            |
| gettxoutsetinfo        | STABLE     |                                            |
| getunconfirmedbalance  | STABLE     |                                            |
| getwalletinfo          | STABLE     |                                            |
| help                   | STABLE     |                                            |
| importaddress          | STABLE     |                                            |
| importmulti            | STABLE     |                                            |
| importprivkey          | STABLE     |                                            |
| importprunedfunds      | STABLE     |                                            |
| importpubkey           | STABLE     |                                            |
| importwallet           | STABLE     |                                            |
| keypoolrefill          | STABLE     |                                            |
| listaccounts           | DEPRECATED | Deprecated since 1.14.0                    |
| listaddressgroupings   | STABLE     |                                            |
| listbanned             | STABLE     |                                            |
| listlockunspent        | STABLE     |                                            |
| listreceivedbyaccount  | DEPRECATED | Deprecated since 1.14.0                    |
| listreceivedbyaddress  | STABLE     |                                            |
| listsinceblock         | STABLE     |                                            |
| liststucktransactions  | STABLE     |                                            |
| listtransactions       | STABLE     |                                            |
| listunspent            | STABLE     |                                            |
| lockunspent            | STABLE     |                                            |
| move                   | DEPRECATED | Deprecated since 1.14.0                    |
| ping                   | STABLE     |                                            |
| preciousblock          | STABLE     |                                            |
| prioritisetransaction  | STABLE     |                                            |
| pruneblockchain        | STABLE     |                                            |
| removeprunedfunds      | STABLE     |                                            |
| rescan                 | STABLE     |                                            |
| sendfrom               | STABLE     |                                            |
| sendmany               | STABLE     |                                            |
| sendrawtransaction     | STABLE     |                                            |
| sendtoaddress          | STABLE     |                                            |
| setaccount             | STABLE     |                                            |
| setban                 | STABLE     |                                            |
| setmaxconnections      | UNSTABLE   | New since 1.14.6                           |
| setnetworkactive       | STABLE     |                                            |
| settxfee               | STABLE     |                                            |
| signmessage            | STABLE     |                                            |
| signmessagewithprivkey | STABLE     |                                            |
| signrawtransaction     | STABLE     |                                            |
| stop                   | STABLE     |                                            |
| submitauxblock         | STABLE     |                                            |
| submitblock            | STABLE     |                                            |
| uptime                 | STABLE     | Introduced in 1.15.0                       |
| validateaddress        | STABLE     |                                            |
| verifychain            | STABLE     |                                            |
| verifymessage          | STABLE     |                                            |
| verifytxoutproof       | STABLE     |                                            |
