#!/usr/bin/env python3
# Copyright (c) 2018-2020 The Bitcoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
"""Test bitcoin-wallet."""

import hashlib
import os
import stat
import subprocess
import textwrap

from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import assert_equal

BUFFER_SIZE = 16 * 1024


class ToolWalletTest(BitcoinTestFramework):
    def set_test_params(self):
        self.num_nodes = 1
        self.setup_clean_chain = True
        self.rpc_timeout = 120

    def skip_test_if_missing_module(self):
        self.skip_if_no_wallet()
        self.skip_if_no_wallet_tool()

    def bitcoin_wallet_process(self, *args):
        binary = self.config["environment"]["BUILDDIR"] + '/src/dogecoin-wallet' + self.config["environment"]["EXEEXT"]
        default_args = ['-datadir={}'.format(self.nodes[0].datadir), '-chain=%s' % self.chain]
        if self.options.descriptors and 'create' in args:
            default_args.append('-descriptors')

        return subprocess.Popen([binary] + default_args + list(args), stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)

    def assert_raises_tool_error(self, error, *args):
        p = self.bitcoin_wallet_process(*args)
        stdout, stderr = p.communicate()
        assert_equal(p.poll(), 1)
        assert_equal(stdout, '')
        assert_equal(stderr.strip(), error)

    def assert_tool_output(self, output, *args):
        p = self.bitcoin_wallet_process(*args)
        stdout, stderr = p.communicate()
        assert_equal(stderr, '')
        assert_equal(stdout, output)
        assert_equal(p.poll(), 0)

    def wallet_shasum(self):
        h = hashlib.sha1()
        mv = memoryview(bytearray(BUFFER_SIZE))
        with open(self.wallet_path, 'rb', buffering=0) as f:
            for n in iter(lambda: f.readinto(mv), 0):
                h.update(mv[:n])
        return h.hexdigest()

    def wallet_timestamp(self):
        return os.path.getmtime(self.wallet_path)

    def wallet_permissions(self):
        return oct(os.lstat(self.wallet_path).st_mode)[-3:]

    def log_wallet_timestamp_comparison(self, old, new):
        result = 'unchanged' if new == old else 'increased!'
        self.log.debug('Wallet file timestamp {}'.format(result))

    def test_invalid_tool_commands_and_args(self):
        self.log.info('Testing that various invalid commands raise with specific error messages')
        self.assert_raises_tool_error('Invalid command: foo', 'foo')
        # `bitcoin-wallet help` raises an error. Use `bitcoin-wallet -help`.
        self.assert_raises_tool_error('Invalid command: help', 'help')
        self.assert_raises_tool_error('Error: two methods provided (info and create). Only one method should be provided.', 'info', 'create')
        self.assert_raises_tool_error('Error parsing command line arguments: Invalid parameter -foo', '-foo')
        locked_dir = os.path.join(self.options.tmpdir, "node0", "regtest", "wallets")
        error = 'Error initializing wallet database environment "{}"!'.format(locked_dir)
        if self.options.descriptors:
            error = "SQLiteDatabase: Unable to obtain an exclusive lock on the database, is it being used by another dogecoind?"
        self.assert_raises_tool_error(
            error,
            '-wallet=' + self.default_wallet_name,
            'info',
        )
        path = os.path.join(self.options.tmpdir, "node0", "regtest", "wallets", "nonexistent.dat")
        self.assert_raises_tool_error("Failed to load database path '{}'. Path does not exist.".format(path), '-wallet=nonexistent.dat', 'info')

    def test_tool_wallet_info(self):
        # Stop the node to close the wallet to call the info command.
        self.stop_node(0)
        self.log.info('Calling wallet tool info, testing output')
        #
        # TODO: Wallet tool info should work with wallet file permissions set to
        # read-only without raising:
        # "Error loading wallet.dat. Is wallet being used by another process?"
        # The following lines should be uncommented and the tests still succeed:
        #
        # self.log.debug('Setting wallet file permissions to 400 (read-only)')
        # os.chmod(self.wallet_path, stat.S_IRUSR)
        # assert self.wallet_permissions() in ['400', '666'] # Sanity check. 666 because Appveyor.
        # shasum_before = self.wallet_shasum()
        timestamp_before = self.wallet_timestamp()
        self.log.debug('Wallet file timestamp before calling info: {}'.format(timestamp_before))
        if self.options.descriptors:
            out = textwrap.dedent('''\
                Wallet info
                ===========
                Name: default_wallet
                Format: sqlite
                Descriptors: yes
                Encrypted: no
                HD (hd seed available): yes
                Keypool Size: 6
                Transactions: 0
                Address Book: 1
            ''')
        else:
            out = textwrap.dedent('''\
                Wallet info
                ===========
                Name: \

                Format: bdb
                Descriptors: no
                Encrypted: no
                HD (hd seed available): yes
                Keypool Size: 2
                Transactions: 0
                Address Book: 3
            ''')
        self.assert_tool_output(out, '-wallet=' + self.default_wallet_name, 'info')
        timestamp_after = self.wallet_timestamp()
        self.log.debug('Wallet file timestamp after calling info: {}'.format(timestamp_after))
        self.log_wallet_timestamp_comparison(timestamp_before, timestamp_after)
        self.log.debug('Setting wallet file permissions back to 600 (read/write)')
        os.chmod(self.wallet_path, stat.S_IRUSR | stat.S_IWUSR)
        assert self.wallet_permissions() in ['600', '666']  # Sanity check. 666 because Appveyor.
        #
        # TODO: Wallet tool info should not write to the wallet file.
        # The following lines should be uncommented and the tests still succeed:
        #
        # assert_equal(timestamp_before, timestamp_after)
        # shasum_after = self.wallet_shasum()
        # assert_equal(shasum_before, shasum_after)
        # self.log.debug('Wallet file shasum unchanged\n')

    def test_tool_wallet_info_after_transaction(self):
        """
        Mutate the wallet with a transaction to verify that the info command
        output changes accordingly.
        """
        self.start_node(0)
        self.log.info('Generating transaction to mutate wallet')
        self.nodes[0].generate(1)
        self.stop_node(0)

        self.log.info('Calling wallet tool info after generating a transaction, testing output')
        shasum_before = self.wallet_shasum()
        timestamp_before = self.wallet_timestamp()
        self.log.debug('Wallet file timestamp before calling info: {}'.format(timestamp_before))
        if self.options.descriptors:
            out = textwrap.dedent('''\
                Wallet info
                ===========
                Name: default_wallet
                Format: sqlite
                Descriptors: yes
                Encrypted: no
                HD (hd seed available): yes
                Keypool Size: 6
                Transactions: 1
                Address Book: 1
            ''')
        else:
            out = textwrap.dedent('''\
                Wallet info
                ===========
                Name: \

                Format: bdb
                Descriptors: no
                Encrypted: no
                HD (hd seed available): yes
                Keypool Size: 2
                Transactions: 1
                Address Book: 3
            ''')
        self.assert_tool_output(out, '-wallet=' + self.default_wallet_name, 'info')
        shasum_after = self.wallet_shasum()
        timestamp_after = self.wallet_timestamp()
        self.log.debug('Wallet file timestamp after calling info: {}'.format(timestamp_after))
        self.log_wallet_timestamp_comparison(timestamp_before, timestamp_after)
        #
        # TODO: Wallet tool info should not write to the wallet file.
        # This assertion should be uncommented and succeed:
        # assert_equal(timestamp_before, timestamp_after)
        assert_equal(shasum_before, shasum_after)
        self.log.debug('Wallet file shasum unchanged\n')

    def test_tool_wallet_create_on_existing_wallet(self):
        self.log.info('Calling wallet tool create on an existing wallet, testing output')
        shasum_before = self.wallet_shasum()
        timestamp_before = self.wallet_timestamp()
        self.log.debug('Wallet file timestamp before calling create: {}'.format(timestamp_before))
        out = textwrap.dedent('''\
            Topping up keypool...
            Wallet info
            ===========
            Name: foo
            Format: bdb
            Descriptors: no
            Encrypted: no
            HD (hd seed available): yes
            Keypool Size: 2000
            Transactions: 0
            Address Book: 0
        ''')
        self.assert_tool_output(out, '-wallet=foo', 'create')
        shasum_after = self.wallet_shasum()
        timestamp_after = self.wallet_timestamp()
        self.log.debug('Wallet file timestamp after calling create: {}'.format(timestamp_after))
        self.log_wallet_timestamp_comparison(timestamp_before, timestamp_after)
        assert_equal(timestamp_before, timestamp_after)
        assert_equal(shasum_before, shasum_after)
        self.log.debug('Wallet file shasum unchanged\n')

    def test_getwalletinfo_on_different_wallet(self):
        self.log.info('Starting node with arg -wallet=foo')
        self.start_node(0, ['-nowallet', '-wallet=foo'])

        self.log.info('Calling getwalletinfo on a different wallet ("foo"), testing output')
        shasum_before = self.wallet_shasum()
        timestamp_before = self.wallet_timestamp()
        self.log.debug('Wallet file timestamp before calling getwalletinfo: {}'.format(timestamp_before))
        out = self.nodes[0].getwalletinfo()
        self.stop_node(0)

        shasum_after = self.wallet_shasum()
        timestamp_after = self.wallet_timestamp()
        self.log.debug('Wallet file timestamp after calling getwalletinfo: {}'.format(timestamp_after))

        assert_equal(0, out['txcount'])
        assert_equal(1000, out['keypoolsize'])
        assert_equal(1000, out['keypoolsize_hd_internal'])
        assert_equal(True, 'hdseedid' in out)

        self.log_wallet_timestamp_comparison(timestamp_before, timestamp_after)
        assert_equal(timestamp_before, timestamp_after)
        assert_equal(shasum_after, shasum_before)
        self.log.debug('Wallet file shasum unchanged\n')

    def test_salvage(self):
        # TODO: Check salvage actually salvages and doesn't break things. https://github.com/bitcoin/bitcoin/issues/7463
        self.log.info('Check salvage')
        self.start_node(0)
        self.nodes[0].createwallet("salvage")
        self.stop_node(0)

        self.assert_tool_output('', '-wallet=salvage', 'salvage')

    def test_dump_createfromdump(self):
        self.start_node(0)
        self.nodes[0].createwallet("todump")
        file_format = self.nodes[0].get_wallet_rpc("todump").getwalletinfo()["format"]
        self.nodes[0].createwallet("todump2")
        self.stop_node(0)

        self.log.info('Checking dump arguments')
        self.assert_raises_tool_error('No dump file provided. To use dump, -dumpfile=<filename> must be provided.', '-wallet=todump', 'dump')

        self.log.info('Checking basic dump')
        wallet_dump = os.path.join(self.nodes[0].datadir, "wallet.dump")
        self.assert_tool_output('The dumpfile may contain private keys. To ensure the safety of your Bitcoin, do not share the dumpfile.\n', '-wallet=todump', '-dumpfile={}'.format(wallet_dump), 'dump')

        dump_data = self.read_dump(wallet_dump)
        orig_dump = dump_data.copy()
        # Check the dump magic
        assert_equal(dump_data['DOGECOIN_CORE_WALLET_DUMP'], '1')
        # Check the file format
        assert_equal(dump_data["format"], file_format)

        self.log.info('Checking that a dumpfile cannot be overwritten')
        self.assert_raises_tool_error('File {} already exists. If you are sure this is what you want, move it out of the way first.'.format(wallet_dump),  '-wallet=todump2', '-dumpfile={}'.format(wallet_dump), 'dump')

        self.log.info('Checking createfromdump arguments')
        self.assert_raises_tool_error('No dump file provided. To use createfromdump, -dumpfile=<filename> must be provided.', '-wallet=todump', 'createfromdump')
        non_exist_dump = os.path.join(self.nodes[0].datadir, "wallet.nodump")
        self.assert_raises_tool_error('Unknown wallet file format "notaformat" provided. Please provide one of "bdb" or "sqlite".', '-wallet=todump', '-format=notaformat', '-dumpfile={}'.format(wallet_dump), 'createfromdump')
        self.assert_raises_tool_error('Dump file {} does not exist.'.format(non_exist_dump), '-wallet=todump', '-dumpfile={}'.format(non_exist_dump), 'createfromdump')
        wallet_path = os.path.join(self.nodes[0].datadir, 'regtest/wallets/todump2')
        self.assert_raises_tool_error('Failed to create database path \'{}\'. Database already exists.'.format(wallet_path), '-wallet=todump2', '-dumpfile={}'.format(wallet_dump), 'createfromdump')
        self.assert_raises_tool_error("The -descriptors option can only be used with the 'create' command.", '-descriptors', '-wallet=todump2', '-dumpfile={}'.format(wallet_dump), 'createfromdump')

        self.log.info('Checking createfromdump')
        self.do_tool_createfromdump("load", "wallet.dump")
        self.do_tool_createfromdump("load-bdb", "wallet.dump", "bdb")
        if self.is_sqlite_compiled():
            self.do_tool_createfromdump("load-sqlite", "wallet.dump", "sqlite")

        self.log.info('Checking createfromdump handling of magic and versions')
        bad_ver_wallet_dump = os.path.join(self.nodes[0].datadir, "wallet-bad_ver1.dump")
        dump_data["DOGECOIN_CORE_WALLET_DUMP"] = "0"
        self.write_dump(dump_data, bad_ver_wallet_dump)
        self.assert_raises_tool_error('Error: Dumpfile version is not supported. This version of dogecoin-wallet only supports version 1 dumpfiles. Got dumpfile with version 0', '-wallet=badload', '-dumpfile={}'.format(bad_ver_wallet_dump), 'createfromdump')
        assert not os.path.isdir(os.path.join(self.nodes[0].datadir, "regtest/wallets", "badload"))
        bad_ver_wallet_dump = os.path.join(self.nodes[0].datadir, "wallet-bad_ver2.dump")
        dump_data["DOGECOIN_CORE_WALLET_DUMP"] = "2"
        self.write_dump(dump_data, bad_ver_wallet_dump)
        self.assert_raises_tool_error('Error: Dumpfile version is not supported. This version of dogecoin-wallet only supports version 1 dumpfiles. Got dumpfile with version 2', '-wallet=badload', '-dumpfile={}'.format(bad_ver_wallet_dump), 'createfromdump')
        assert not os.path.isdir(os.path.join(self.nodes[0].datadir, "regtest/wallets", "badload"))
        bad_magic_wallet_dump = os.path.join(self.nodes[0].datadir, "wallet-bad_magic.dump")
        del dump_data["DOGECOIN_CORE_WALLET_DUMP"]
        dump_data["not_the_right_magic"] = "1"
        self.write_dump(dump_data, bad_magic_wallet_dump, "not_the_right_magic")
        self.assert_raises_tool_error('Error: Dumpfile identifier record is incorrect. Got "not_the_right_magic", expected "DOGECOIN_CORE_WALLET_DUMP".', '-wallet=badload', '-dumpfile={}'.format(bad_magic_wallet_dump), 'createfromdump')
        assert not os.path.isdir(os.path.join(self.nodes[0].datadir, "regtest/wallets", "badload"))

        self.log.info('Checking createfromdump handling of checksums')
        bad_sum_wallet_dump = os.path.join(self.nodes[0].datadir, "wallet-bad_sum1.dump")
        dump_data = orig_dump.copy()
        checksum = dump_data["checksum"]
        dump_data["checksum"] = "1" * 64
        self.write_dump(dump_data, bad_sum_wallet_dump)
        self.assert_raises_tool_error('Error: Dumpfile checksum does not match. Computed {}, expected {}'.format(checksum, "1" * 64), '-wallet=bad', '-dumpfile={}'.format(bad_sum_wallet_dump), 'createfromdump')
        assert not os.path.isdir(os.path.join(self.nodes[0].datadir, "regtest/wallets", "badload"))
        bad_sum_wallet_dump = os.path.join(self.nodes[0].datadir, "wallet-bad_sum2.dump")
        del dump_data["checksum"]
        self.write_dump(dump_data, bad_sum_wallet_dump, skip_checksum=True)
        self.assert_raises_tool_error('Error: Missing checksum', '-wallet=badload', '-dumpfile={}'.format(bad_sum_wallet_dump), 'createfromdump')
        assert not os.path.isdir(os.path.join(self.nodes[0].datadir, "regtest/wallets", "badload"))
        bad_sum_wallet_dump = os.path.join(self.nodes[0].datadir, "wallet-bad_sum3.dump")
        dump_data["checksum"] = "2" * 10
        self.write_dump(dump_data, bad_sum_wallet_dump)
        self.assert_raises_tool_error('Error: Dumpfile checksum does not match. Computed {}, expected {}{}'.format(checksum, "2" * 10, "0" * 54), '-wallet=badload', '-dumpfile={}'.format(bad_sum_wallet_dump), 'createfromdump')
        assert not os.path.isdir(os.path.join(self.nodes[0].datadir, "regtest/wallets", "badload"))


    def run_test(self):
        self.wallet_path = os.path.join(self.nodes[0].datadir, self.chain, 'wallets', self.default_wallet_name, self.wallet_data_filename)
        self.test_invalid_tool_commands_and_args()
        # Warning: The following tests are order-dependent.
        self.test_tool_wallet_info()
        self.test_tool_wallet_info_after_transaction()
        if not self.options.descriptors:
            # TODO: Wallet tool needs more create options at which point these can be enabled.
            self.test_tool_wallet_create_on_existing_wallet()
            self.test_getwalletinfo_on_different_wallet()
            # Salvage is a legacy wallet only thing
            self.test_salvage()

if __name__ == '__main__':
    ToolWalletTest().main()
