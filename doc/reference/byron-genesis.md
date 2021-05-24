# Byron genesis data format

Please see [the byron ledger spec](https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/byronLedgerSpec/latest/download-by-type/doc-pdf/ledger-spec) which will describe the changes and deviations of these values in more detail while moving from the Byron era to the OBFT era.

Let's start with an example:

```
{
    "avvmDistr": {
        "0cAZzmtB2CUjlsMULb30YcBrocvK7VhQjUk--MyY2_Q=": "8333333000000",
        ...
        "zE56EfVAvv0XekVyBDGh2Iz1QT6X38YxlcBYO20hqa0=": "1773135000000"
    },
    "blockVersionData": {
        "heavyDelThd": "300000000000",
        "maxBlockSize": "2000000",
        "maxHeaderSize": "2000000",
        "maxProposalSize": "700",
        "maxTxSize": "4096",
        "mpcThd": "20000000000000",
        "scriptVersion": 0,
        "slotDuration": "20000",
        "softforkRule": {
            "initThd": "900000000000000",
            "minThd": "600000000000000",
            "thdDecrement": "50000000000000"
        },
        "txFeePolicy": {
            "multiplier": "43946000000",
            "summand": "155381000000000"
        },
        "unlockStakeEpoch": "18446744073709551615",
        "updateImplicit": "10000",
        "updateProposalThd": "100000000000000",
        "updateVoteThd": "1000000000000"
    },
    "bootStakeholders": {
        "0d916567f96b6a65d204966e6aab5fbd242e56c321833f8ba5d607da": 1,
        "4bd1884f5ce2231be8623ecf5778a9112e26514205b39ff53529e735": 1,
        "5f53e01e1366aeda8811c2a630f0e037077a7b651093d2bdc4ef7200": 1,
        "ada3ab5c69b945c33c15ca110b444aa58906bf01fcfe55d8818d9c49": 1
    },
    "ftsSeed": "736b6f766f726f64612047677572646120626f726f64612070726f766f646120",
    "nonAvvmBalances": {},
    "protocolConsts": {
        "k": 2160,
        "protocolMagic": 60987900,
        "vssMaxTTL": 6,
        "vssMinTTL": 2
    },
    "heavyDelegation": {
        "c6c7fb227037a1719b9d871ea49b6039325aeb293915da7db9620e3f": {
            "cert": "0f2ff9d1f071793dbd90fce8d47e96a09b594ee6dd00a4bac9664e4bd6af89830035ec921698b774c779eb1b6a2772d3d6ae37e630c06c75fbfecd02a6410a02",
            "delegatePk": "I07+5HIUW0Lkq0poMMzGziuILwxSyTJ4lMSsoQz4HZunD5Xsx3MfBZWc4l+206lUOFaU+spdJg7MkmFKBoVV0g==",
            "issuerPk": "AZzlT+pC5M4xG+GuRHJEXS+isikmVBorTqOyjRDGUQ+Lst9fn1zQn5OGKSXK29G2dn7R7JCugfcUebr0Dq7wPw==",
            "omega": 1
        }
    },
    "startTime": 1505621332,
    "vssCerts": {
        "0d916567f96b6a65d204966e6aab5fbd242e56c321833f8ba5d607da": {
            "expiryEpoch": 1,
            "signature": "396fe505f4287f832fd26c1eba1843a49f3d23b64d664fb3c8a2f25c8de73ce6f2f4cf37ec7fa0fee7750d1d6c55e1b07e1018ce0c6443bacdb01fb8e15f1a0f",
            "signingKey": "ohsV3RtEFD1jeOzKwNulmMRhBG2RLdFxPbcSGbkmJ+xd/2cOALSDahPlydFRjd15sH0PkPE/zTvP4iN8wJr/hA==",
            "vssKey": "WCECtpe8B/5XPefEhgg7X5veUIYH/RRcvXbz6w7MIJBwWYU="
        },
        ...
    }
}
```

## FAQ:

- What is an issuer? An issuer is a stakeholder/delegator that delegates their stake to a delegatee.

## avvmDistr
The key `avvmDistr` contains AVVM addresses with corresponding balances (lovelaces). People who purchased Ada at a pre-sale were issued a certificate at the Ada Voucher Vending Machine (AVVM) during the pre-sale period. AVVM addresses can be converted to Byron addresses and initially each address has this amount of ADA (as balance, not stake).

## blockVersionData

The key `blockVersionData` contains fundamental blockchain-related values:

#### heavyDelThd
heavyweight delegation threshold. The delegate (i.e stakeholder) must possess no less than this threshold of stake in order to participate in heavyweight delegation.

#### maxBlockSize
 maximum size of block in bytes.
#### maxHeaderSize
 maximum size of block's header in bytes.
#### maxProposalSize
 maximum size of Cardano SL update proposal in bytes.
#### maxTxSize
 maximum size of transaction in bytes.
#### mpcThd
 threshold for participation in SSC (shared seed computation). See SSC [here.](https://cardanodocs.com/glossary/) This will be deprecated in the OBFT era.
#### scriptVersion
Redundant. This was meant to be the version of the Plutus smart contract language however there are no smart contracts in the Byron era.
#### slotDuration
slot duration, in milliseconds.

#### softforkRule
Values defining softfork resolution rule. When the stake belonging to block issuers, issuing a given block version, is greater than the current softfork resolution threshold, this block version is adopted. The current softfork resolution threshold is determined as follows: If a proposal is confirmed at the `c`-th epoch, softfork resolution threshold at a later `t`-th epoch will be `max minThd (spInitThd - (t - c) * spThdDecrement)`.

The proportion of ADA that these threshold values (`initThd`,`minThd`,`thdDecrement`) represent is calculated by dividing by 10<sup>15</sup>. This is an artitifact of the old implementation as it would make more sense if the proportion was calculated by the total amount of circulating ADA which is 45 x 10<sup>15</sup>. Note that in the specification we only make use of the `minThd`. See [here](https://hydra.iohk.io/build/1084792/download/1/ledger-spec.pdf) in section 6.5.4 Adoption threshold.

- `initThd` - Initial threshold (right after proposal is confirmed).
- `minThd` - minimal threshold (i.e. threshold can't become less than this one).
- `thdDecrement` - theshold will be decreased by this value after each epoch.

#### txFeePolicy
Transaction fee policy represents a formula to compute the minimal allowed Fee for a transaction. Transactions with lesser fees won't be accepted. At the moment we have just one policy type (a linear Equation on the transaction size, fee = summand + multiplier * txsize), but in the future other policies may be added.
    - `summand`
    - `multipler`
#### unlockStakeEpoch
This has been re-purposed for unlocking the OuroborosBFT logic in the software. Relevant: [CDEC-610](https://iohk.myjetbrains.com/youtrack/issue/CDEC-610)
#### updateImplicit
Time to live for a protocol update proposal. The number of slots a proposal has to gather a majority of votes. If a majority of votes has not been reached before this period, then the proposal is rejected.
#### updateProposalThd
threshold for Cardano SL update proposal.
#### updateVoteThd
threshold for voting for Cardano SL update proposal.

## bootStakeholders
The key `bootStakeholders` contains bootstrap era stakeholders' identifiers (StakeholderIds) with corresponding weights.

## ftsSeed

Redundant The key `ftsSeed` contains seed value required for Follow-the-Satoshi mechanism (hex-encoded). However this is being deprecated in the OBFT era.

## protocolConsts
The key `protocolConsts` contains basic protocol constants:
   - `k` - security parameter from the paper,
   - `pm` - protocol magic number
   - `requiresNetworkMagic` - Used to distinguish between mainnet/staging and testing.
   - `vssMaxTTL` - VSS certificates maximum timeout to live (number of epochs),
   - `vssMinTTL` - VSS certificates minimum timeout to live (number of epochs).

## protocolMagic
The key `protocolMagic` defines the protocol magic number. When the protocol magic is changed, all signatures become invalid. This is used to distinguish different networks.

- `pm` - is the protocol magic number, is included in serialized blocks and headers, and is part of signed data. It is used for differentiating between different clusters.

- `requiresNetworkMagic` - can be `NMMustBeNothing`,`NMMustBeJust`, `RequiresNoMagic`, or `RequiresMagic`. It is essentially a Boolean like flag to indicate whether we are on mainnet/staging (`NMMustBeJust`
/`RequiresMagic`) or a testnet (`NMMustBeNothing`/`RequiresNoMagic`).

The `protocolMagic` value can either be an object with the two fields described above, or just a plain integer. In the latter case, `requiresNetworkMagic` will take the default value of `RequiresMagic`.

Relevant:
- [cardano-ledger - Genesis test](https://github.com/input-output-hk/cardano-ledger/blob/master/cardano-ledger/test/Test/Cardano/Chain/Genesis/Json.hs#L69)
- [cardano-ledger - ProtocolMagic](https://github.com/input-output-hk/cardano-ledger/blob/master/crypto/src/Cardano/Crypto/ProtocolMagic.hs)

## heavyDelegation
The key `heavyDelegation` contains an information about heavyweight delegation:

- `cert` - delegation certificate.
- `delegatePk` - delegate's public key.
- `issuerPk` - stakeholder's (issuer's) public key.
- `omega` - index of epoch the block PSK is announced in, it is needed for replay attack prevention, can be set to arbitrary value in genesis.

Keys in `heavyDelegation` dictionary are StakeholderIds of certificates' issuers. They must be consistent with issuerPk values.

## startTime
The key `startTime` is the timestamp of the 0-th slot. Ideally it should be few seconds later than the cluster actually starts. If it's significantly later, nodes won't be doing anything for a while. If it's slightly before the actual starts, some slots will be missed, but it shouldn't be critical as long as less than k slots are missed.

## vssCerts

The key `vssCerts` contains VSS certificates:

- `expiryEpoch` - index of epoch until which (inclusive) the certificate is valid.
- `signature` - signature of certificate.
- `signingKey` - key used for signing.
- `vssKey` - VSS public key.

Keys in `vssCerts` dictionary are StakeholderIds of certificates' issuers. They must be consistent with signingKey values.

## Balances and stakes
In genesis data there are two fields which determine balances: `avvmDistr` and `nonAvvmBalances`

- `avvmDistr` is a map from AVVM addresses to balances
- `nonAvvmBalances` is a map from Byron addresses to balances.
AVVM addresses can be easily converted to Byron addresses (redeem type), so obtaining a final map from Byron addresses to balances is trivial.

Stakes are computed from balances w.r.t. `bootStakeholders` map, because all genesis addresses have BootstrapEra stake distribution. Keys in `bootStakeholders` map are StakeholderIds which will have stake and values are their weights (stake will be distributed proportionally to those weights).
