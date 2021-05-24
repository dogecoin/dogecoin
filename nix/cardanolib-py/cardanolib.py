"""Cardano Python Library."""
import functools
import json
import subprocess
import os
from copy import copy
from pathlib import Path
from time import sleep
from contextlib import contextmanager

@contextmanager
def cd(path):
    if not os.path.exists(path):
        os.mkdir(path, 0o700)
    old_dir = os.getcwd()
    os.chdir(path)
    try:
        yield
    finally:
        os.chdir(old_dir)


class CardanoCLIError(Exception):
    pass

class CardanoWrongEraError(Exception):
    pass

class CardanoCluster:
    """Cardano Cluster Tools."""

    def __init__(self, network_magic, state_dir, num_delegs, protocol="cardano"):
        self.cli = CardanoCLIWrapper(network_magic, state_dir, protocol=protocol)
        if(protocol == "cardano" or protocol == "byron"):
            self.byron_delegate_keys = list(map(lambda x: self.cli.state_dir / "byron" / f"delegate-keys.{x:03}.key", list(range(0,num_delegs))))
        if(protocol == "cardano" or protocol == "shelley"):
            self.shelley_delegate_skeys = list(map(lambda x: self.cli.state_dir / "shelley" / "delegate-keys" / f"delegate{x}.skey", list(range(1,num_delegs+1))))
            self.shelley_genesis_vkeys = list(map(lambda x: self.cli.state_dir / "shelley" / "genesis-keys" / f"genesis{x}.vkey", list(range(1,num_delegs+1))))
            if os.path.exists( self.cli.state_dir / "shelley" / "genesis-utxo.skey"):
                self.genesis_utxo_vkey = self.cli.state_dir / "shelley" / "genesis-utxo.vkey"
                self.genesis_utxo_skey = self.cli.state_dir / "shelley" / "genesis-utxo.skey"
                self.genesis_utxo_addr = self.cli.get_genesis_addr(self.genesis_utxo_vkey)

    # We're not adding methods to CLIWrapper for these complex byron commands
    def hard_fork_byron(self, version):
        version_params = version.split(".")
        proposal_path = self.cli.state_dir / f"{version}.proposal"
        self.cli.cmd(["cardano-cli", "byron", "create-update-proposal", "--testnet-magic", str(self.cli.network_magic), "--signing-key", self.byron_delegate_keys[0], "--protocol-version-major", version_params[0], "--protocol-version-minor", version_params[1], "--protocol-version-alt", version_params[2], "--application-name", "cardano-sl", "--software-version-num", "1", "--system-tag", "linux", "--installer-hash", "0", "--filepath", proposal_path])
        self.cli.cmd(["cardano-cli", "byron", "submit-update-proposal", "--testnet-magic", str(self.cli.network_magic), "--filepath", proposal_path])
        for index,key in enumerate(self.byron_delegate_keys, start=1):
            proposal_vote_path = f"{proposal_path}-vote{index}"
            self.cli.cmd(["cardano-cli", "byron", "create-proposal-vote", "--proposal-filepath", proposal_path, "--testnet-magic", str(self.cli.network_magic), "--signing-key", key, "--vote-yes", "--output-filepath", proposal_vote_path])
            self.cli.cmd(["cardano-cli", "byron", "submit-proposal-vote", "--testnet-magic", str(self.cli.network_magic), "--filepath", proposal_vote_path])
        self.update_config_version(version_params)
        for i in range(len(self.byron_delegate_keys)):
            self.restart_node(f"bft{i+1}")
            sleep(1)
        # TODO: wait until new epoch where shelley starts
        if version == "2.0.0":
            self.cli.current_protocol = "shelley"

    # only works with shelley
    def sleep_until_next_epoch(self):
        tip = self.cli.get_tip()
        slot = tip["slotNo"]
        epoch_length = self.cli.epoch_length
        slots_remaining = epoch_length - (slot % epoch_length)
        sleep(self.cli.slot_length * slots_remaining)

    def restart_node(self, node):
        self.cli.cmd(["supervisorctl", "restart", node])

    def update_config_version(self, version_params, config_file="config.json"):
        config_file = self.cli.state_dir / config_file
        with open(config_file) as in_json:
            config = json.load(in_json)
        config["LastKnownBlockVersion-Major"] = int(version_params[0])
        config["LastKnownBlockVersion-Minor"] = int(version_params[1])
        config["LastKnownBlockVersion-Alt"] = int(version_params[2])
        with open(config_file, "w") as out_json:
            json.dump(config, out_json)

    def send_tx_genesis(
        self, txouts=None, certificates=None, signing_keys=None, proposal_file=None,
    ):
        txouts = txouts or []
        certificates = certificates or []
        signing_keys = signing_keys or []

        utxo = self.cli.get_utxo(address=self.genesis_utxo_addr)
        total_input_amount = 0
        txins = []
        for k, v in utxo.items():
            total_input_amount += v["amount"]
            txin = k.split("#")
            txin = (txin[0], txin[1])
            txins.append(txin)

        signing_keys.append(str(self.genesis_utxo_skey))
        utxo = self.cli.get_utxo(address=self.genesis_utxo_addr)
        txins = []
        for k, v in utxo.items():
            txin = k.split("#")
            txin = (txin[0], txin[1], v["amount"])
            txins.append(txin)
        txout_total = sum(map(lambda x: x[1], txouts))

        # Build, Sign and Send TX to chain
        try:
            self.cli.build_tx(
                txins=txins,
                txouts=txouts + [(self.genesis_utxo_addr, 0)],
                certificates=certificates,
                proposal_file=proposal_file,
            )
            fee = self.cli.estimate_fee(len(txins), len(txouts), len(signing_keys))
            ttl = self.cli.get_tip()["slotNo"] + 5000
            self.cli.build_tx(
                txins=txins,
                txouts=txouts + [(self.genesis_utxo_addr, total_input_amount - fee - txout_total)],
                certificates=certificates,
                fee=fee,
                ttl=ttl,
                proposal_file=proposal_file,
            )
            self.cli.sign_tx(signing_keys=signing_keys)
            self.cli.submit_tx()
        except CardanoCLIError as err:
            raise CardanoCLIError(
                f"Sending a genesis transaction failed!\n"
                f"utxo: {utxo}\n"
                f"txins: {txins} txouts: {txouts} signing keys: {signing_keys}\n{err}"
            )

    def submit_update_proposal(self, proposal_opts):
        # TODO: assumption is update proposals submitted near beginning of epoch
        epoch = self.cli.get_tip()["slotNo"] // self.cli.epoch_length

        self.cli.create_update_proposal(proposal_opts, self.shelley_genesis_vkeys, epoch=epoch)
        self.send_tx_genesis(signing_keys=self.shelley_delegate_skeys, proposal_file="update.proposal")

    # TODO:
    def byron_generate_tx_slice(self, start, slice_size, tx_filename, snapshot, wallet, fee):
        s = slice(start, start+slice_size)
        records = snapshot[s]
        txouts = list(map( lambda x: getTxOut(x), records))
        txouts_total = sum(map(lambda x: x["value"], records))

        wallet["value"] = wallet["value"] - txouts_total - fee
        txout_args = self.cli.prepend_flag("--txout", txouts)
        self.cli.cmd(["cardano-cli", "issue-utxo-expenditure", "--tx", tx_filename, "--testnet-magic", str(self.networkMagic), "--txin", getTxIn(current_tx), "--wallet-key", str(wallet["key"]), "--txout", getTxOut(wallet) ] + txout_args)
        return wallet

class CardanoCLIWrapper:
    """Cardano CLI Wrapper."""

    def __init__(self, network_magic, state_dir, shelley_keys="shelley", byron_keys="byron", protocol="shelley"):
        self.network_magic = network_magic

        self.state_dir = Path(state_dir).expanduser().absolute()
        self.shelley_genesis_json = self.state_dir / shelley_keys / "genesis.json"
        self.byron_genesis_json = self.state_dir / byron_keys / "genesis.json"
        self.pparams_file = self.state_dir / "pparams.json"

        self.check_state_dir()


        self.pparams = None
        self.current_protocol = protocol
        if protocol == "shelley":
            with open(self.shelley_genesis_json) as in_json:
                self.shelley_genesis = json.load(in_json)
            self.refresh_pparams()
            self.slot_length = self.shelley_genesis["slotLength"]
            self.epoch_length = self.shelley_genesis["epochLength"]
        elif protocol == "byron":
            with open(self.byron_genesis_json) as in_json:
                self.byron_genesis = json.load(in_json)

        elif protocol == "cardano":
            # TODO: check if byron or shelley
            self.current_protocol = "byron"
            with open(self.shelley_genesis_json) as in_json:
                self.shelley_genesis = json.load(in_json)
            with open(self.byron_genesis_json) as in_json:
                self.byron_genesis = json.load(in_json)


    def check_state_dir(self):
        if not self.state_dir.exists():
            raise CardanoCLIError(f"The state dir `{self.state_dir}` doesn't exist.")

        for file_name in ([
            self.shelley_genesis_json
        ]):
            if not file_name.exists():
                raise CardanoCLIError(f"The file `{file_name}` doesn't exist.")

    @staticmethod
    def cmd(cli_args):
        p = subprocess.Popen(cli_args, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        stdout, stderr = p.communicate()
        if p.returncode != 0:
            raise CardanoCLIError(f"An error occurred running a CLI command `{p.args}`: {stderr}")
        return stdout

    @staticmethod
    def prepend_flag(flag, contents):
        return sum(([flag, x] for x in contents), [])

    def query_cli(self, cli_args):
        return self.cmd(
            [
                "cardano-cli",
                "shelley",
                "query",
                *cli_args,
                "--testnet-magic",
                str(self.network_magic),
            ]
        )


    def refresh_pparams(self):
        if self.current_protocol == "byron":
            raise CardanoWrongEraError("Attempted to run shelley command in byron era")
        self.query_cli(["protocol-parameters", "--out-file", str(self.pparams_file)])
        with open(self.pparams_file) as in_json:
            self.pparams = json.load(in_json)

    def estimate_fee(self, txins=1, txouts=1, witnesses=1, byron_witnesses=0, txbody_file="tx.body"):
        self.refresh_pparams()
        stdout = self.cmd(
            [
                "cardano-cli",
                "shelley",
                "transaction",
                "calculate-min-fee",
                "--testnet-magic",
                str(self.network_magic),
                "--protocol-params-file",
                str(self.pparams_file),
                "--tx-in-count",
                str(txins),
                "--tx-out-count",
                str(txouts),
                "--byron-witness-count",
                str(byron_witnesses),
                "--witness-count",
                str(witnesses),
                "--tx-body-file",
                str(txbody_file),
            ]
        )
        fee, __ = stdout.decode().split(" ")
        return int(fee)

    def build_tx(
        self,
        out_file="tx.body",
        txins=None,
        txouts=None,
        certificates=None,
        fee=0,
        ttl=None,
        proposal_file=None,
    ):
        if ttl == None:
            ttl = self.get_tip()["slotNo"] + 5000
        txins = txins or []
        txouts_copy = copy(txouts) if txouts else []
        certificates = certificates or []

        txins_combined = [f"{x[0]}#{x[1]}" for x in txins]
        txouts_combined = [f"{x[0]}+{x[1]}" for x in txouts_copy]

        txin_args = self.prepend_flag("--tx-in", txins_combined)
        txout_args = self.prepend_flag("--tx-out", txouts_combined)
        cert_args = self.prepend_flag("--certificate-file", certificates)

        build_args = [
            "cardano-cli",
            "shelley",
            "transaction",
            "build-raw",
            "--invalid-hereafter",
            str(ttl),
            "--fee",
            str(fee),
            "--out-file",
            str(out_file),
            *txin_args,
            *txout_args,
            *cert_args,
        ]

        if proposal_file:
            build_args.extend(["--update-proposal-file", proposal_file])

        self.cmd(build_args)

    def sign_tx(self, tx_body_file="tx.body", out_file="tx.signed", signing_keys=None):
        signing_keys = signing_keys or []
        key_args = self.prepend_flag("--signing-key-file", signing_keys)
        self.cmd(
            [
                "cardano-cli",
                "shelley",
                "transaction",
                "sign",
                "--tx-body-file",
                str(tx_body_file),
                "--out-file",
                str(out_file),
                "--testnet-magic",
                str(self.network_magic),
                *key_args,
            ]
        )

    def submit_tx(self, tx_file="tx.signed"):
        if self.current_protocol == "byron":
            raise CardanoWrongEraError("Attempted to run shelley command in byron era")
        self.cmd(
            [
                "cardano-cli",
                "shelley",
                "transaction",
                "submit",
                "--testnet-magic",
                str(self.network_magic),
                "--tx-file",
                str(tx_file),
            ]
        )

    def get_payment_address(self, payment_vkey, stake_vkey=None):
        if not payment_vkey:
            raise CardanoCLIError("Must set payment key.")

        cli_args = ["--payment-verification-key-file", str(payment_vkey)]
        if stake_vkey:
            cli_args.extend(["--stake-verification-key-file", str(stake_vkey)])

        return (
            self.cmd(
                [
                    "cardano-cli",
                    "shelley",
                    "address",
                    "build",
                    "--testnet-magic",
                    str(self.network_magic),
                    *cli_args,
                ]
            )
            .rstrip()
            .decode("ascii")
        )

    def get_genesis_addr(self, vkey_path):
        return (
            self.cmd(
                [
                    "cardano-cli",
                    "shelley",
                    "genesis",
                    "initial-addr",
                    "--testnet-magic",
                    str(self.network_magic),
                    "--verification-key-file",
                    str(vkey_path),
                ]
            )
            .rstrip()
            .decode("ascii")
        )

    def get_utxo(self, address):
        if self.current_protocol == "byron":
            raise CardanoWrongEraError("Attempted to run shelley command in byron era")
        self.query_cli(["utxo", "--address", address, "--out-file", "utxo.json"])
        with open("utxo.json") as in_json:
            utxo = json.load(in_json)
        return utxo

    def get_tip(self):
        return json.loads(self.query_cli(["tip"]))


    def create_update_proposal(self, proposal_opts, genesis_keys, epoch=1, file_name="update.proposal"):
        genesis_key_args = self.prepend_flag("--genesis-verification-key-file", genesis_keys)
        proposal_args = sum(list(map(lambda x: [ f"--{x[0]}", str(x[1]) ], proposal_opts)), [])
        self.cmd(
            [
                "cardano-cli",
                "shelley",
                "governance",
                "create-update-proposal",
                "--out-file",
                file_name,
                "--epoch",
                str(epoch),
                *proposal_args,
                *genesis_key_args
            ]
        )
    def create_utxo(self, name):
        self.cmd(["cardano-cli",
                  "shelley",
                  "address",
                  "key-gen",
                  "--verification-key-file",
                  f"{name}.vkey",
                  "--signing-key-file",
                  f"{name}.skey"
                ])

    def create_stake_address_and_cert(self, name):
        self.cmd(["cardano-cli",
                  "shelley",
                  "stake-address",
                  "key-gen",
                  "--verification-key-file",
                  f"{name}.vkey",
                  "--signing-key-file",
                  f"{name}.skey"
                ])
        self.cmd(["cardano-cli",
                  "shelley",
                  "stake-address",
                  "registration-certificate",
                  "--stake-verification-key-file",
                  f"{name}.vkey",
                  "--out-file",
                  f"{name}.cert"
                ])


    def create_cold_key(self, name):
        self.cmd(["cardano-cli",
                  "shelley",
                  "node",
                  "key-gen",
                  "--cold-verification-key-file",
                  f"{name}.vkey",
                  "--cold-signing-key-file",
                  f"{name}.skey",
                  "--operational-certificate-issue-counter-file",
                  f"{name}.counter"
                ])

    def create_vrf_key(self, name):
        self.cmd(["cardano-cli",
                  "shelley",
                  "node",
                  "key-gen-VRF",
                  "--verification-key-file",
                  f"{name}.vkey",
                  "--signing-key-file",
                  f"{name}.skey"
                ])

    def create_stake_pool(self, prefix, owner_vkey, pledge, cost, margin, relay_dns, relay_port, metadata_url, metadata_hash):
        self.cmd(["cardano-cli",
                  "shelley",
                  "stake-pool",
                  "registration-certificate",
                  "--cold-verification-key-file",
                  f"{prefix}-cold.vkey",
                  "--vrf-verification-key-file",
                  f"{prefix}-vrf.vkey",
                  "--pool-pledge",
                  str(pledge),
                  "--pool-cost",
                  str(cost),
                  "--pool-margin",
                  str(margin),
                  "--pool-reward-account-verification-key-file",
                  f"{prefix}-reward.vkey",
                  "--pool-owner-stake-verification-key-file",
                  owner_vkey,
                  "--single-host-pool-relay",
                  relay_dns,
                  "--pool-relay-port",
                  str(relay_port),
                  "--metadata-url",
                  metadata_url,
                  "--metadata-hash",
                  metadata_hash,
                  "--out-file",
                  f"{prefix}.cert",
                  "--testnet-magic",
                  str(self.network_magic)
                ])

    def create_metadata_file(self, name, file_name, description, ticker, homepage):
        contents = {
                "name": name,
                "description": description,
                "ticker": ticker,
                "homepage": homepage
        }
        with open(file_name, "w") as out_json:
            json.dump(contents, out_json)
        return self.cmd(["cardano-cli",
                  "shelley",
                  "stake-pool",
                  "metadata-hash",
                  "--pool-metadata-file",
                  file_name
                ]).rstrip()

    def create_delegation(self, stake, pool, file_name):
        self.cmd(["cardano-cli",
                  "shelley",
                  "stake-address",
                  "delegation-certificate",
                  "--stake-verification-key-file",
                  stake,
                  "--cold-verification-key-file",
                  pool,
                  "--out-file",
                  file_name
                ])

    # Creates owners, keys, pool registration and owner delegation, then submits all in a transaction
    def register_stake_pool(self, name, ticker, description, homepage, pledge, cost, margin, index, relay_dns, relay_port, metadata_url, prefix="node", payment_key="utxo", directory="pools"):
        self.refresh_pparams()
        prev_dir = os.getcwd()
        with cd(directory):
            # create owner keys and stake registration certificate
            p = f"{prefix}{index}"
            if os.path.exists(f"{p}.cert"):
                raise CardanoCLIError("Pool already exists! Aborting!")
            self.create_utxo(f"{p}-owner-utxo")
            self.create_stake_address_and_cert(f"{p}-owner-stake")
            self.create_stake_address_and_cert(f"{p}-reward")
            self.create_cold_key(f"{p}-cold")
            self.create_vrf_key(f"{p}-vrf")
            metadata_hash = self.create_metadata_file(name, f"{p}-metadata.json", description, ticker, homepage)
            self.create_stake_pool(p, f"{p}-owner-stake.vkey", pledge, cost, margin, relay_dns, relay_port, metadata_url, metadata_hash)
            self.create_delegation(f"{p}-owner-stake.vkey", f"{p}-cold.vkey", f"{p}-owner-delegation.cert")
            source_payment_address = self.get_payment_address(f"{prev_dir}/{payment_key}.vkey")
            source_payment_utxo = self.get_utxo(source_payment_address)
            # TODO: function for this???
            txins = []
            total_input_amount = 0
            for k, v in source_payment_utxo.items():
                total_input_amount += v["amount"]
                txin = k.split("#")
                txin = (txin[0], txin[1])
                txins.append(txin)

            owner_payment_address = self.get_payment_address(f"{p}-owner-utxo.vkey", f"{p}-owner-stake.vkey")

            txouts = [(source_payment_address, total_input_amount), (owner_payment_address, pledge)]
            deposits = self.pparams["poolDeposit"] + (2 * self.pparams["keyDeposit"])
            signing_keys = [ f"{prev_dir}/{payment_key}.skey", f"{p}-owner-stake.skey", f"{p}-cold.skey", f"{p}-reward.skey" ]
            certificates = [ f"{p}-owner-stake.cert", f"{p}-reward.cert", f"{p}.cert", f"{p}-owner-delegation.cert" ]
            self.build_tx(f"tx-register-{p}.txbody", txins, txouts, certificates)
            fee = self.estimate_fee(len(txins), len(txouts), len(signing_keys), txbody_file=f"tx-register-{p}.txbody")
            txouts = [(source_payment_address, total_input_amount - pledge - fee - deposits ), (owner_payment_address, pledge)]
            self.build_tx(f"tx-register-{p}.txbody", txins, txouts, certificates, fee)
            self.sign_tx(f"tx-register-{p}.txbody", f"tx-register-{p}.txsigned", signing_keys)
            self.submit_tx(f"tx-register-{p}.txsigned")

    def convert_itn_vkey(self, key):
        with open(f"{key}.pk", "w") as fname:
            fname.write(k)
        self.cmd(["cardano-cli", "stake-address", "convert-itn-key", "--itn-verification-key-file", f"{key}.pk", "--out-file", f"{key}.vkey"])

    def convert_itn_skey(self, key):
        with open(f"{key}.sk", "w") as fname:
            fname.write(k)
        self.cmd(["cardano-cli", "stake-address", "convert-itn-key", "--itn-signing-key-file", f"{key}.sk", "--out-file", f"{key}.skey"])

    # Byron specific commands
    def byron_txin_format(self, tx):
        return "(\"{}\",{})".format(tx[0],tx[1])

    def byron_txout_format(self, fund):
        return "(\"{}\",{})".format(fund["address"], fund["value"])


