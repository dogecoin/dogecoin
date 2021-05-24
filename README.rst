.. raw:: html

   <p align="center">
     <a href="https://github.com/input-output-hk/cardano-node/releases"><img src="https://img.shields.io/github/release-pre/input-output-hk/cardano-node.svg?style=for-the-badge" /></a>
     <a href="https://buildkite.com/input-output-hk/cardano-node"><img src="https://img.shields.io/buildkite/a978cbb4def7018be3d0a004127da356f4db32f1c318c1a48a/master?label=BUILD&style=for-the-badge"/></a>
   </p>

*************************
``cardano-node`` Overview
*************************

Integration of the `ledger <https://github.com/input-output-hk/cardano-ledger-specs>`_, `consensus <https://github.com/input-output-hk/ouroboros-network/tree/master/ouroboros-consensus>`_,
`networking <https://github.com/input-output-hk/ouroboros-network/tree/master/ouroboros-network>`_ and
`node shell <https://github.com/input-output-hk/cardano-shell>`_ repositories.

`Logging <https://github.com/input-output-hk/iohk-monitoring-framework>`_ is provided as a
`feature <https://github.com/input-output-hk/cardano-shell/blob/master/app/Cardano/Shell/Features/Logging.hs>`_ by the node shell to the other packages.

- The cardano-node is the top level for the node and
  aggregates the other components from other packages: consensus, ledger and
  networking, with configuration, CLI, logging and monitoring.

- The node no longer incorporates wallet or explorer functionality. The wallet
  backend and explorer backend are separate components that run in separate
  external processes that communicate with the node via local IPC.

Network Configuration, Genesis and Topology Files
=================================================

The latest supported networks can be found at `<https://hydra.iohk.io/job/Cardano/cardano-node/cardano-deployment/latest-finished/download/1/index.html>`_

How to build
============

Documentation for building the node can be found `here <https://docs.cardano.org/projects/cardano-node/en/latest/getting-started/install.html>`_.

Linux Executable
==================

You can download the latest version of ``cardano-node`` and ``cardano-cli``:

* `linux <https://hydra.iohk.io/job/Cardano/cardano-node/cardano-node-linux/latest-finished>`_
* `win64 <https://hydra.iohk.io/job/Cardano/cardano-node/cardano-node-win64/latest-finished>`_
* `macos <https://hydra.iohk.io/job/Cardano/cardano-node/cardano-node-macos/latest-finished>`_

Windows Executable
==================

Download
--------

You can download `here <https://hydra.iohk.io/job/Cardano/cardano-node/cardano-node-win64/latest-finished>`_.

Instructions
------------

The download includes cardano-node.exe and a .dll. To run the node with cardano-node run you need to reference a few files and directories as arguments. These can be copied from the cardano-node repo into the executables directory. The command to run the node on mainnet looks like this:

.. code-block:: console

    cardano-node.exe run --topology ./mainnet-topology.json --database-path ./state --port 3001 --config ./configuration-mainnet.yaml --socket-path \\.\pipe\cardano-node

Docker image
============

You can pull the docker image with the latest version of cardano-node from `here <https://hub.docker.com/r/inputoutput/cardano-node>`_.

.. code-block:: console

    docker pull inputoutput/cardano-node

``cardano-node``
================
This refers to the client that is used for running a node.

The general synopsis is as follows:

.. code-block:: console

   Usage: cardano-node run [--topology FILEPATH] [--database-path FILEPATH]
                           [--socket-path FILEPATH]
                           [--byron-delegation-certificate FILEPATH]
                           [--byron-signing-key FILEPATH]
                           [--shelley-kes-key FILEPATH]
                           [--shelley-vrf-key FILEPATH]
                           [--shelley-operational-certificate FILEPATH]
                           [--host-addr IPV4-ADDRESS]
                           [--host-ipv6-addr IPV6-ADDRESS]
                           [--port PORT]
                           [--config NODE-CONFIGURATION] [--validate-db]
     Run the node.

* ``--topology`` - Filepath to a topology file describing which peers the node should connect to.

* ``--database-path`` - Path to the blockchain database.

* ``--byron-delegation-certificate`` - Optional path to the Byron delegation certificate. The delegation certificate allows the delegator (the issuer of said certificate) to give his/her own block signing rights to somebody else (the delegatee). The delegatee can then sign blocks on behalf of the delegator.

* ``--byron-signing-key`` - Optional path to the Bryon signing key.

* ``--shelley-signing-key`` - Optional path to the Shelley signing key.

* ``--shelley-kes-key`` - Optional path to the Shelley KES signing key.

* ``--shelley-vrf-key`` - Optional path to the Shelley VRF signing key.

* ``--shelley-operational-certificate`` - Optional path to the Shelley operational certificate.

* ``--socket-path`` - Path to the socket file.

* ``--host-addr`` - Optionally specify your node's IPv4 address.

* ``--host-ipv6-addr`` - Optionally specify your node's IPv6 address.

* ``--port`` - Specify which port to assign to the node.

* ``--config`` - Specify the filepath to the config ``.yaml`` file. This file is responsible for all the other node's required settings. See examples in ``configuration`` (e.g. `config-0.yaml <configuration/defaults/simpleview/config-0.yaml>`_).

* ``--validate-db`` - Flag to revalidate all on-disk database files

Configuration ``.yaml`` files
=============================

The ``--config`` flag points to a ``.yaml`` file that is responsible to configuring the logging & other important settings for the node. E.g. see the Byron mainnet configuration in this
`configuration.yaml <https://github.com/input-output-hk/cardano-node/blob/master/configuration/defaults/byron-mainnet/configuration.yaml>`_.
Some of the more important settings are as follows:

* ``Protocol: RealPBFT`` -- Protocol the node will execute

* ``RequiresNetworkMagic``: RequiresNoMagic -- Used to distinguish between mainnet (``RequiresNoMagic``) and testnets (``RequiresMagic``)


Logging
========

Logs are output to the ``logs/`` dir.

Profiling & statistics
======================

Profiling data and RTS run stats are stored in the ``profile/`` dir.

Please see ``scripts/README.md`` for how to obtain profiling information using the scripts.

Scripts
=======

Please see ``scripts/README.md`` for information on the various scripts.

``cardano-cli``
===============

A CLI utility to support a variety of key material operations (genesis, migration, pretty-printing..) for different system generations.
Usage documentation can be found at ``cardano-cli/README.md``.

The general synopsis is as follows:

.. code-block:: console

   Usage: cardano-cli (Era based commands | Byron specific commands | Miscellaneous commands)

> NOTE: the exact invocation command depends on the environment.  If you have only built ``cardano-cli``, without installing it, then you have to prepend ``cabal run -- ``
before ``cardano-cli``.  We henceforth assume that the necessary environment-specific adjustment has been made, so we only mention ``cardano-cli``.

The subcommands are subdivided in groups, and their full list can be seen in the output of ``cardano-cli --help``.

All subcommands have help available.  For example:

.. code-block:: console

   cabal run -- cardano-cli -- byron key migrate-delegate-key-from --help

   cardano-cli -- byron key migrate-delegate-key-from
   Usage: cardano-cli byron key migrate-delegate-key-from --from FILEPATH
                                                          --to FILEPATH
     Migrate a delegate key from an older version.


   Available options:
     --byron-legacy-formats   Byron/cardano-sl formats and compatibility
     --byron-formats          Byron era formats and compatibility
     --from FILEPATH          Signing key file to migrate.
     --to FILEPATH            Non-existent file to write the signing key to.
     -h,--help                Show this help text


Genesis operations
==================

Generation
----------

The Byron genesis generation operations will create a directory that contains:

* ``genesis.json``:
  The genesis JSON file itself.

* ``avvm-seed.*.seed``:
  Ada Voucher Vending Machine seeds (secret). Affected by ``--avvm-entry-count`` and ``--avvm-entry-balance``.

* ``delegate-keys.*.key``:
  Delegate private keys. Affected by: ``--n-delegate-addresses``.

* ``delegation-cert.*.json``:
  Delegation certificates. Affected by: ``--n-delegate-addresses``.

* ``genesis-keys.*.key``:
  Genesis stake private keys. Affected by: ``--n-delegate-addresses``, ``--total-balance``.

* ``poor-keys.*.key``:
  Non-delegate private keys with genesis UTxO. Affected by: ``--n-poor-addresses``, ``--total-balance``.

More details on the Byron Genesis ``JSON`` file can be found in ``docs/reference/byron-genesis.md``

 Byron genesis delegation and related concepts are described in detail in:

  `<https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/byronLedgerSpec/latest/download-by-type/doc-pdf/ledger-spec>`_

The canned ``scripts/benchmarking/genesis.sh`` example provides a nice set of defaults and
illustrates available options.

Key operations
==============

Note that key operations do not support password-protected keys.

Signing key generation & verification key extraction
----------------------------------------------------

Signing keys can be generated using the ``keygen`` subcommand.

Extracting a verification key out of the signing key is performed by the ``to-verification`` subcommand.

Delegate key migration
----------------------

In order to continue using a delegate key from the Byron Legacy era in the new implementation,
it needs to be migrated over, which is done by the ``migrate-delegate-key-from`` subcommand:

.. code-block:: console

  $ cabal v2-run -- cardano-cli byron key migrate-delegate-key-from
          --from key0.sk --to key0Converted.sk

Signing key queries
-------------------

One can gather information about a signing key's properties through the ``signing-key-public``
and ``signing-key-address`` subcommands (the latter requires the network magic):

.. code-block:: console

   $ cabal v2-run -- cardano-cli byron key signing-key-public --byron-formats --secret key0.sk

   public key hash: a2b1af0df8ca764876a45608fae36cf04400ed9f413de2e37d92ce04
   public key: sc4pa1pAriXO7IzMpByKo4cG90HCFD465Iad284uDYz06dHCqBwMHRukReQ90+TA/vQpj4L1YNaLHI7DS0Z2Vg==

   $ cabal v2-run -- cardano-cli signing-key-address --byron-formats --secret key0.pbft --testnet-magic 42

   2cWKMJemoBakxhXgZSsMteLP9TUvz7owHyEYbUDwKRLsw2UGDrG93gPqmpv1D9ohWNddx
   VerKey address with root e5a3807d99a1807c3f161a1558bcbc45de8392e049682df01809c488, attributes: AddrAttributes { derivation path: {} }

Transactions
============

Creation
--------

Transactions can be created via the  ``issue-genesis-utxo-expenditure`` & ``issue-utxo-expenditure`` commands.

The easiest way to create a transaction is via the ``scripts/benchmarking/issue-genesis-utxo-expenditure.sh`` script as follows:

``./scripts/benchmarking/issue-genesis-utxo-expenditure.sh transaction_file``

NB: This by default creates a transaction based on ``configuration/defaults/liveview/config-0.yaml``

If you do not have a ``genesis_file`` you can run ``scripts/benchmarking/genesis.sh`` which will create an example ``genesis_file`` for you.
The script ``scripts/benchmarking/issue-genesis-utxo-expenditure.sh`` has defaults for all the requirements of the ``issue-genesis-utxo-expenditure`` command.

Submission
----------

The ``submit-tx`` subcommand provides the option of submitting a pre-signed
transaction, in its raw wire format (see GenTx for Byron transactions).

The canned ``scripts/benchmarking/submit-tx.sh`` script will submit the supplied transaction to a testnet
launched by ``scripts/benchmarking/shelley-testnet-liveview.sh`` script.

Issuing UTxO expenditure (genesis and regular)
----------------------------------------------

To make a transaction spending UTxO, you can either use the:

  - ``issue-genesis-utxo-expenditure``, for genesis UTxO
  - ``issue-utxo-expenditure``, for normal UTxO

subcommands directly, or, again use canned scripts that will make transactions tailored
for the aforementioned testnet cluster:

  - ``scripts/benchmarking/issue-genesis-utxo-expenditure.sh``.
  - ``scripts/benchmarking/issue-utxo-expenditure.sh``.

The script requires the target file name to write the transaction to, input TxId
(for normal UTxO), and optionally allows specifying the source txin output index,
source and target signing keys and lovelace value to send.

The target address defaults to the 1-st richman key (``configuration/delegate-keys.001.key``)
of the testnet, and lovelace amount is almost the entirety of its funds.

Local node queries
==================

You can query the tip of your local node via the ``get-tip`` command as follows

1. Open `tmux`
2. Run ``cabal build cardano-node``
3. Run ``./scripts/lite/shelley-testnet.sh example``
4. Run ``export CARDANO_NODE_SOCKET_PATH=/cardano-node/example/socket/node-1-socket
4. ``cabal exec cardano-cli -- get-tip --testnet-magic 42``

You will see output from stdout in this format:

.. code-block:: console

   Current tip:
   Block hash: 4ab21a10e1b25e39
   Slot: 6
   Block number: 5

Update proposals
================

Update proposal creation
------------------------

A Byron update proposal can be created as follows:

.. code-block:: console

   cardano-cli -- byron governance
                  create-update-proposal
                    (--mainnet | --testnet-magic NATURAL)
                    --signing-key FILEPATH
                    --protocol-version-major WORD16
                    --protocol-version-minor WORD16
                    --protocol-version-alt WORD8
                    --application-name STRING
                    --software-version-num WORD32
                    --system-tag STRING
                    --installer-hash HASH
                    --filepath FILEPATH
                  ..

The mandatory arguments are ``--mainnet | --testnet-magic``, ``signing-key``, ``protocol-version-major``, ``protocol-version-minor``, ``protocol-version-alt``, ``application-name``, ``software-version-num``, ``system-tag``, ``installer-hash`` and ``filepath``.

The remaining arguments are optional parameters you want to update in your update proposal.

You can also check your proposal's validity using the `validate-cbor` command. See: `Validate CBOR files`_.

See the `Byron specification <https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/byronLedgerSpec/latest/download-by-type/doc-pdf/ledger-spec>`_
for more details on update proposals.

Update proposal submission
--------------------------

You can submit your proposal using the ``submit-update-proposal`` command.

Example:

.. code-block:: console

   cardano-cli -- byron governance
               submit-update-proposal
               --config configuration/defaults/mainnet/configuration.yaml
               (--mainnet | --testnet-magic NATURAL)
               --filepath my-update-proposal

See the `Byron specification <https://hydra.iohk.io/job/Cardano/cardano-ledger-specs/byronLedgerSpec/latest/download-by-type/doc-pdf/ledger-spec>`_
for more deatils on update proposals.

Update proposal voting
======================

You can create and submit byron update proposal votes with the ``create-proposal-vote`` & ``submit-proposal-vote`` commands. The following are two example commands:


Byron vote creation:

.. code-block:: console

   cabal exec cardano-cli -- byron governance create-proposal-vote
                          (--mainnet | --testnet-magic NATURAL)
                          --signing-key configuration/defaults/liveview/genesis/delegate-keys.000.key
                          --proposal-filepath ProtocolUpdateProposalFile
                          --vote-yes
                          --output-filepath UpdateProposalVoteFile

Byron vote submission:

.. code-block:: console

   cabal exec cardano-cli -- byron governance submit-proposal-vote
                          (--mainnet | --testnet-magic NATURAL)
                          --filepath UpdateProposalVoteFile

Development
===========

run *ghcid* with: ``ghcid -c "cabal repl exe:cardano-node --reorder-goals"``

Testing
========

``cardano-node`` is essentially a container which implements several components such networking, consensus, and storage. These components have individual test coverage. The node goes through integration and release testing by Devops/QA while automated CLI tests are ongoing alongside development.

Developers on ``cardano-node`` can `launch their own testnets <doc/getting-started/launching-a-testnet.md>`_ or `run the chairman tests <doc/getting-started/running-chairman-tests.md>`_ locally.

Chairman tests
--------------

Debugging
=========

Pretty printing CBOR encoded files
----------------------------------

It may be useful to print the on chain representations of blocks, delegation certificates, txs and update proposals. There are two commands that do this (for any cbor encoded file):

To pretty print as CBOR:
``cabal exec cardano-cli -- pretty-print-cbor --filepath CBOREncodedFile``

Validate CBOR files
-------------------

You can validate Byron era blocks, delegation certificates, txs and update proposals with the ``validate-cbor`` command.

``cabal exec cardano-cli -- validate-cbor --byron-block 21600 --filepath CBOREncodedByronBlockFile``


Native Token Pre-Production Environment
=======================================

Thanks for your interest in building native tokens on Cardano. To help you get started we have compiled a handy list of resources:

`Cardano Forum discussion forum <https://forum.cardano.org/c/developers/cardano-tokens/150>`_

`Developer Documentation for Native Tokens <https://developers.cardano.org/en/development-environments/native-tokens/native-tokens/>`_


**Please note that over the holiday period, technical support for the pre-production environment and token builder tool will be extremely limited**. Support is unavailable between the dates of 23rd - 27th December and 31 December - 3rd January inclusive. Outside these hours, our technical and community teams will be periodically checking in on the GitHub repo and dedicated Cardano Forum discussion forum, to expedite any urgent queries or requests. We encourage you to draw on community feedback and support as much as possible.

If you require test ada during this period, please `fill out this form <https://input-output.typeform.com/to/KmeBcnDa>`_ and you will be sent your test ada. Note that until the wallet backend is fully integrated, this is an essentially manual process and there may therefore be some delay before the request is processed.  For technical reasons, it may only be possible to fund newly created addresses that have been properly set up on the Pre-Production Environment.  Unfortunately, since the form only records payment addresses, it will not be possible to contact you if the funding attempt fails, or to notify you that it has succeeded. Please check that you have submitted the address correctly, and retry if you need to

API Documentation
=================
The API documentation is published `here <https://input-output-hk.github.io/cardano-node/>`_.

The documentation is built with each push, but is only published from `master` branch.  In order to
test if the documentation is working, build the documentation locally with `./scripts/haddocs.sh` and
open `haddocks/index.html` in the browser.
