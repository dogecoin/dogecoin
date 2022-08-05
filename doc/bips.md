BIP List
---------

This document lists the protocol features that are implemented by Dogecoin Core
(up-to-date up to **v1.14.6**):

### From Bitcoin

* [`BIP 11`](https://github.com/bitcoin/bips/blob/master/bip-0011.mediawiki): Multisig outputs are standard since **v1.4.0**.
* [`BIP 13`](https://github.com/bitcoin/bips/blob/master/bip-0013.mediawiki): The address format for P2SH addresses has been implemented since **v1.0.0**.
* [`BIP 14`](https://github.com/bitcoin/bips/blob/master/bip-0014.mediawiki): The subversion string is being used as User Agent since **v1.0.0**.
* [`BIP 16`](https://github.com/bitcoin/bips/blob/master/bip-0016.mediawiki): The pay-to-script-hash evaluation rules have been implemented since **v1.6.0**.
* [`BIP 21`](https://github.com/bitcoin/bips/blob/master/bip-0021.mediawiki): The URI format for Dogecoin payments has been implemented since **v1.0.0**.
* [`BIP 22`](https://github.com/bitcoin/bips/blob/master/bip-0022.mediawiki): The 'getblocktemplate' (GBT) RPC protocol for mining has been implemented since **v1.7.0**.
* [`BIP 23`](https://github.com/bitcoin/bips/blob/master/bip-0023.mediawiki): Some extensions to GBT have been implemented since **v1.10.0**, including longpolling and block proposals.
* [`BIP 30`](https://github.com/bitcoin/bips/blob/master/bip-0030.mediawiki): The evaluation rules to forbid creating new transactions with the same txid as previous not-fully-spent transactions were implemented since **v1.0.0**.
* [`BIP 31`](https://github.com/bitcoin/bips/blob/master/bip-0031.mediawiki): The 'pong' protocol message (and the protocol version bump to 60001) has been implemented since **v1.0.0**.
* [`BIP 32`](https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki): Hierarchical Deterministic Wallets has been implemented since **v1.14.0**.
* [`BIP 34`](https://github.com/bitcoin/bips/blob/master/bip-0034.mediawiki): The rule that requires blocks to contain their height (number) in the coinbase input, and the introduction of version 2 blocks has been implemented since **v1.4.0**.
* [`BIP 35`](https://github.com/bitcoin/bips/blob/master/bip-0035.mediawiki): The 'mempool' protocol message (and the protocol version bump to 60002) has been implemented since **v1.0.0**.
* [`BIP 37`](https://github.com/bitcoin/bips/blob/master/bip-0037.mediawiki): The bloom filtering for transaction relaying, partial merkle trees for blocks, and the protocol version bump to 70001 (enabling low-bandwidth SPV clients) has been implemented since **v1.4.0**
* [`BIP 61`](https://github.com/bitcoin/bips/blob/master/bip-0061.mediawiki): The 'reject' protocol message (and the protocol version bump to 70002) was added in **v1.7.0**.
* [`BIP 65`](https://github.com/bitcoin/bips/blob/master/bip-0065.mediawiki): The CHECKLOCKTIMEVERIFY softfork was merged in **v1.14.0**
* [`BIP 66`](https://github.com/bitcoin/bips/blob/master/bip-0066.mediawiki): The strict DER rules and associated version 3 blocks have been implemented since **v1.10.0**
* [`BIP 70`](https://github.com/bitcoin/bips/blob/master/bip-0070.mediawiki) [`71`](https://github.com/bitcoin/bips/blob/master/bip-0071.mediawiki) [`72`](https://github.com/bitcoin/bips/blob/master/bip-0072.mediawiki): Payment Protocol support has been available in Dogecoin Core GUI since **v1.7.0**
* [`BIP 111`](https://github.com/bitcoin/bips/blob/master/bip-0111.mediawiki): `NODE_BLOOM` service bit added, and enforced for all peer versions as of **v1.10.0**.
* [`BIP 125`](https://github.com/bitcoin/bips/blob/master/bip-0125.mediawiki): Opt-in full replace-by-fee signaling honoured in mempool and mining as of **v1.14.0**.
* [`BIP 130`](https://github.com/bitcoin/bips/blob/master/bip-0130.mediawiki): direct headers announcement is negotiated with peer versions `>=70012` as of **v1.14.0**.
* [`BIP 133`](https://github.com/bitcoin/bips/blob/master/bip-0133.mediawiki): feefilter messages are respected and sent for peer versions `>=70013` as of **v1.14.0**.
* [`BIP 152`](https://github.com/bitcoin/bips/blob/master/bip-0152.mediawiki): Compact block transfer version 1 are used as of **v1.14.0**.

### From Litecoin

* (Spec missing!): Scrypt-1024 proof-of-work was enabled since **v1.0.0**.

### From Namecoin

* [Merged Mining](https://en.bitcoin.it/wiki/Merged_mining_specification) (AuxPoW): AuxPoW blocks were enabled as valid proof of work as of **v1.8.0**.

### From DigiByte

* (Spec missing!): DigiShield difficulty adjustments were enabled as of **v1.6.0**.
