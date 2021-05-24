# Understanding your configuration files and how to use them

#### The topology.json file

Tells your node to which nodes in the network it should talk to. A minimal version of this file looks like this:

```json
{
  "Producers": [
    {
      "addr": "x.x.x.x",
      "port": 3001,
      "valency": 1
    }
  ]
}
```
* This means that your node will contact node at ip `x.x.x.x` on `port 3001`.

* `valency` tells the node how many connections your node should have. It only has an effect for dns addresses. If a dns address is given, valency governs to how many resolved ip addresses should we maintain active (hot) connection; for ip addresses, valency is used as a Boolean value, where `0` means to ignore the address.

Your __block-producing__ node must __ONLY__ talk to your __relay nodes__, and the relay node should talk to other relay nodes in the network. Go to our telegram channel to find out IP addresses and ports of peers.


#### The genesis.json file

The genesis file is generated with the `cardano-cli` by reading a `genesis.spec.json` file, which is out of scope for this document.
But it is important because it is used to set:

* `genDelegs`, a mapping from genesis keys to genesis delegates.
* `initialFunds`, a mapping from the initial addresses to the initial values at those address.
* `maxLovelaceSupply`, the total amount of lovelaces in the blockchain.
* `systemStart`, the time of slot zero.

The `genesis.json` file looks like the one below.
```json
{
  "activeSlotsCoeff": 0.05,
  "protocolParams": {
    "protocolVersion": {
      "minor": 0,
      "major": 2
    },
    "decentralisationParam": 1,
    "eMax": 18,
    "extraEntropy": {
      "tag": "NeutralNonce"
    },
    "maxTxSize": 16384,
    "maxBlockBodySize": 65536,
    "maxBlockHeaderSize": 1100,
    "minFeeA": 44,
    "minFeeB": 155381,
    "minUTxOValue": 1000000,
    "poolDeposit": 500000000,
    "minPoolCost": 340000000,
    "keyDeposit": 2000000,
    "nOpt": 150,
    "rho": 0.003,
    "tau": 0.20,
    "a0": 0.3
  },
  "genDelegs": {
    "ad5463153dc3d24b9ff133e46136028bdc1edbb897f5a7cf1b37950c": {
      "delegate": "d9e5c76ad5ee778960804094a389f0b546b5c2b140a62f8ec43ea54d",
      "vrf": "64fa87e8b29a5b7bfbd6795677e3e878c505bc4a3649485d366b50abadec92d7"
    },
    "b9547b8a57656539a8d9bc42c008e38d9c8bd9c8adbb1e73ad529497": {
      "delegate": "855d6fc1e54274e331e34478eeac8d060b0b90c1f9e8a2b01167c048",
      "vrf": "66d5167a1f426bd1adcc8bbf4b88c280d38c148d135cb41e3f5a39f948ad7fcc"
    },
    "60baee25cbc90047e83fd01e1e57dc0b06d3d0cb150d0ab40bbfead1": {
      "delegate": "7f72a1826ae3b279782ab2bc582d0d2958de65bd86b2c4f82d8ba956",
      "vrf": "c0546d9aa5740afd569d3c2d9c412595cd60822bb6d9a4e8ce6c43d12bd0f674"
    },
    "f7b341c14cd58fca4195a9b278cce1ef402dc0e06deb77e543cd1757": {
      "delegate": "69ae12f9e45c0c9122356c8e624b1fbbed6c22a2e3b4358cf0cb5011",
      "vrf": "6394a632af51a32768a6f12dac3485d9c0712d0b54e3f389f355385762a478f2"
    },
    "162f94554ac8c225383a2248c245659eda870eaa82d0ef25fc7dcd82": {
      "delegate": "4485708022839a7b9b8b639a939c85ec0ed6999b5b6dc651b03c43f6",
      "vrf": "aba81e764b71006c515986bf7b37a72fbb5554f78e6775f08e384dbd572a4b32"
    },
    "2075a095b3c844a29c24317a94a643ab8e22d54a3a3a72a420260af6": {
      "delegate": "6535db26347283990a252313a7903a45e3526ec25ddba381c071b25b",
      "vrf": "fcaca997b8105bd860876348fc2c6e68b13607f9bbd23515cd2193b555d267af"
    },
    "268cfc0b89e910ead22e0ade91493d8212f53f3e2164b2e4bef0819b": {
      "delegate": "1d4f2e1fda43070d71bb22a5522f86943c7c18aeb4fa47a362c27e23",
      "vrf": "63ef48bc5355f3e7973100c371d6a095251c80ceb40559f4750aa7014a6fb6db"
    }
  },
  "updateQuorum": 5,
  "networkId": "Mainnet",
  "initialFunds": {},
  "maxLovelaceSupply": 45000000000000000,
  "networkMagic": 764824073,
  "epochLength": 432000,
  "systemStart": "2017-09-23T21:44:51Z",
  "slotsPerKESPeriod": 129600,
  "slotLength": 1,
  "maxKESEvolutions": 62,
  "securityParam": 2160
}
```
Here is a brief description of each parameter. You can learn more in the [spec](https://github.com/input-output-hk/cardano-ledger-specs/tree/master/shelley/chain-and-ledger/executable-spec).


| PARAMETER | MEANING |
|----------| --------- |
| activeSlotsCoeff | The proportion of slots in which blocks should be issued. |
| poolDeposit | The amount of a pool registration deposit |
| protocolVersion| Accepted protocol versions |
| decentralisationParam | Percentage of blocks produced by federated nodes |
| maxTxSize | Maximum transaction size |
| minPoolCost | Stake pools cannot register/re-register their stake cost below this value |
| minFeeA | The linear factor for the minimum fee calculation |
| maxBlockBodySize | Maximum block body size |
| minFeeB | The constant factor for the minimum fee calculation |
| eMax | Epoch bound on pool retirement |
| extraEntropy | Well, extra entropy =) |
| maxBlockHeaderSize | Maximum block header size |
| keyDeposit | The amount of a key registration deposit |
| nOpt | Desired number of pools |
| rho | Monetary expansion |
| tau | Treasury expansion |
| a0 | Pool's pledge influence |
| networkMagic | To identify the testnets |
| systemStart | Time of slot 0 |
| genDelegs | Mapping from genesis keys to genesis delegate |
| updateQuorum | Determines the quorum needed for votes on the protocol parameter updates |
| initialFunds | Mapping address to values |
| maxLovelaceSupply | The total number of lovelace in the system, used in the reward calculation. |
| networkMagic | To identify the testnet |
| epochLength | Number of slots in an epoch. |
| staking | Initial delegation |
| slotsPerKESPeriod | Number of slots in an KES period |
| slotLength | in seconds |
| maxKESEvolutions | The maximum number of time a KES key can be evolved before a pool operator must create a new operational certificate |
| securityParam | Security parameter k |


#### The config.json file

The default `config.json` file that we downloaded is shown below.

This file has __4__ sections that allow you to have full control on what your node does and how the informtion is presented.

__NOTE Due to how the config.json file is generated, fields on the real file are shown in a different (less coherent) order. Here we present them in a more structured way__

#### Basic Node Configuration.

First section relates the basic node configuration parameters. Make sure you have to `TPraos`as the protocol, the correct path to the `mainnet-shelley-genesis.json` file, `RequiresMagic`for its use in a testnet.

	  "Protocol": "TPraos",
	  "GenesisFile": "mainnet-shelley-genesis.json",
	  "RequiresNetworkMagic": "RequiresMagic",

#### Update parameters

This protocol version number gets used by block producing nodes as part of the system for agreeing on and synchronising protocol updates. You just need to be aware of the latest version supported by the network. You don't need to change anything here.

	  "ApplicationName": "cardano-sl",
	  "ApplicationVersion": 0,
	  "LastKnownBlockVersion-Alt": 0,
	  "LastKnownBlockVersion-Major": 2,
	  "LastKnownBlockVersion-Minor": 0,


#### Tracing

`Tracers` tell your node what information you are interested in when logging. Like switches that you can turn ON or OFF according the type and quantity of information that you are interesetd in. This provides fairly coarse grained control, but it is relatively efficient at filtering out unwanted trace output.

`TurnOnLogging`: Enables or disables logging overall.

`TurnOnLogMetrics`: Enable the collection of various OS metrics such as memory and CPU use. These metrics can be directed to the logs or monitoring backends.

`setupBackends`, `defaultBackends`, `hasEKG`and `hasPrometheus`: The system supports a number of backends for logging and monitoring. This settings list the backends available to use in the configuration. The logging backend is called `Katip`.
Also enable the EKG backend if you want to use the EKG or Prometheus monitoring interfaces.

`setupScribes` and `defaultScribes`: For the Katip logging backend we must set up outputs (called scribes) The available types of scribe are:

* FileSK: for files
* StdoutSK/StderrSK: for stdout/stderr
* JournalSK: for systemd's journal system
* DevNullSK
* The scribe output format can be ScText or ScJson.

`rotation` The default file rotation settings for katip scribes, unless overridden in the setupScribes above for specific scribes.

```json
"TurnOnLogging": true,
"TurnOnLogMetrics": true,
"TracingVerbosity": "NormalVerbosity",
"minSeverity": "Debug",
"TraceBlockFetchClient": false,
"TraceBlockFetchDecisions": false,
"TraceBlockFetchProtocol": false,
"TraceBlockFetchProtocolSerialised": false,
"TraceBlockFetchServer": false,
"TraceBlockchainTime": false,
"TraceChainDb": true,
"TraceChainSyncBlockServer": false,
"TraceChainSyncClient": false,
"TraceChainSyncHeaderServer": false,
"TraceChainSyncProtocol": false,
"TraceDNSResolver": true,
"TraceDNSSubscription": true,
"TraceErrorPolicy": true,
"TraceForge": true,
"TraceHandshake": false,
"TraceIpSubscription": true,
"TraceLocalChainSyncProtocol": false,
"TraceLocalErrorPolicy": true,
"TraceLocalHandshake": false,
"TraceLocalTxSubmissionProtocol": false,
"TraceLocalTxSubmissionServer": false,
"TraceMempool": true,
"TraceMux": false,
"TraceTxInbound": false,
"TraceTxOutbound": false,
"TraceTxSubmissionProtocol": false,
"setupBackends": [
  "KatipBK"
],
"defaultBackends": [
  "KatipBK"
],
"hasEKG": 12788,
"hasPrometheus": [
  "127.0.0.1",
  12798
],
"setupScribes": [
  {
    "scFormat": "ScText",
    "scKind": "StdoutSK",
    "scName": "stdout",
    "scRotation": null
  }
],
"defaultScribes": [
  [
    "StdoutSK",
    "stdout"
  ]
],
"rotation": {
  "rpKeepFilesNum": 10,
  "rpLogLimitBytes": 5000000,
  "rpMaxAgeHours": 24
  },
```

#### Fine grained logging control

It is also possible to have more fine grained control over filtering of trace output, and to match and route trace output to particular backends. This is less efficient than the coarse trace filters above but provides much more precise control. `options`:

`mapBackends`This routes metrics matching specific names to particular backends. This overrides the defaultBackends listed above. And note that it is an **override** and not an extension so anything matched here will not go to the default backend, only to the explicitly listed backends.

`mapSubtrace` This section is more expressive, we are working on its documentation.

```json
	  "options": {
	    "mapBackends": {
	      "cardano.node.metrics": [
	        "EKGViewBK"
	      ]
	    },
	    "mapSubtrace": {
	      "cardano.node.metrics": {
	        "subtrace": "Neutral"
	      }
	    }
	  }
	}
```
