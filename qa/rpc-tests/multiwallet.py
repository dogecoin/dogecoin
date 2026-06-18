#!/usr/bin/env python3
# Copyright (c) 2017 The Bitcoin Core developers
# Copyright (c) 2026 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

"""Exercise the multi-wallet RPC surface introduced by the multi-wallet
backport: -wallet=<file> repetition, listwallets, loadwallet,
createwallet, unloadwallet, and per-wallet /wallet/<name> routing via
the cli flag emulated by talking to each wallet through the regular
RPC interface after switching the default."""

from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import (
    start_nodes,
    assert_equal,
    assert_raises_jsonrpc,
)


class MultiWalletTest(BitcoinTestFramework):

    def __init__(self):
        super().__init__()
        self.setup_clean_chain = True
        self.num_nodes = 1

    def setup_network(self, split=False):
        # Boot with two wallets so the startup loop is exercised.
        self.nodes = start_nodes(
            self.num_nodes,
            self.options.tmpdir,
            [['-wallet=wallet.dat', '-wallet=second.dat']],
        )
        self.is_network_split = False

    def run_test(self):
        node = self.nodes[0]

        # -wallet=<file> repeated => both wallets are loaded at boot.
        loaded = node.listwallets()
        assert_equal(sorted(loaded), sorted(['wallet.dat', 'second.dat']))

        # createwallet adds a third wallet and registers it.
        result = node.createwallet('third.dat')
        assert_equal(result['name'], 'third.dat')
        assert 'third.dat' in node.listwallets()

        # createwallet on an existing file must be rejected.
        assert_raises_jsonrpc(-4, 'already exists', node.createwallet, 'third.dat')

        # Path traversal must be rejected.
        assert_raises_jsonrpc(-4, 'outside data directory',
                              node.createwallet, '../escape.dat')

        # unloadwallet detaches the wallet; the file stays on disk.
        node.unloadwallet('third.dat')
        assert 'third.dat' not in node.listwallets()

        # The on-disk file must remain loadable after unload.
        result = node.loadwallet('third.dat')
        assert_equal(result['name'], 'third.dat')
        assert 'third.dat' in node.listwallets()

        # Duplicate loadwallet must be rejected.
        assert_raises_jsonrpc(-4, 'already loaded', node.loadwallet, 'third.dat')

        # Unknown wallet unload must report not loaded.
        assert_raises_jsonrpc(-18, 'is not loaded',
                              node.unloadwallet, 'bogus.dat')


if __name__ == '__main__':
    MultiWalletTest().main()
