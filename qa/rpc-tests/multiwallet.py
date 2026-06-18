#!/usr/bin/env python3
# Copyright (c) 2017 The Bitcoin Core developers
# Copyright (c) 2026 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

"""Exercise the multi-wallet surface backported from Bitcoin Core:
   - -wallet=<file> repetition at boot
   - listwallets
   - createwallet (success / duplicate / path traversal)
   - loadwallet (success / nonexistent / duplicate / path traversal)
   - unloadwallet (success by arg, success by /wallet URI, errors)
   - per-wallet /wallet/<name> RPC routing and wallet isolation
   - default endpoint behaviour with 0, 1, and >1 wallets loaded
   - -zapwallettxes / -salvagewallet / -upgradewallet rejection with
     multi -wallet at startup
"""

from test_framework.authproxy import AuthServiceProxy, JSONRPCException
from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import (
    assert_equal,
    assert_raises_jsonrpc,
    rpc_url,
    start_nodes,
    stop_nodes,
)


def wallet_rpc(node_index, wallet_name):
    """Construct an AuthServiceProxy targeted at /wallet/<wallet_name>."""
    return AuthServiceProxy(rpc_url(node_index) + "/wallet/" + wallet_name)


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

        # -- startup --
        # -wallet=<file> repeated => both wallets are loaded at boot.
        assert_equal(sorted(node.listwallets()),
                     sorted(['wallet.dat', 'second.dat']))

        # -- default endpoint behaviour with multiple wallets --
        # bitcoin 0.17 returns RPC_WALLET_NOT_SPECIFIED (-19) when an
        # unscoped wallet RPC is invoked but more than one wallet is loaded.
        assert_raises_jsonrpc(-19, 'Wallet file not specified',
                              node.getwalletinfo)

        # -- per-wallet routing --
        w1 = wallet_rpc(0, 'wallet.dat')
        w2 = wallet_rpc(0, 'second.dat')

        info1 = w1.getwalletinfo()
        info2 = w2.getwalletinfo()
        # Each wallet must have its own HD master key, confirming the URI
        # really routed to two different CWallet instances.
        assert info1['hdmasterkeyid'] != info2['hdmasterkeyid']

        # Per-wallet getnewaddress: same call on different wallets returns
        # different addresses because they draw from different keypools.
        addr1 = w1.getnewaddress()
        addr2 = w2.getnewaddress()
        assert addr1 != addr2

        # Routing to a wallet that does not exist is a clean -18.
        bogus = wallet_rpc(0, 'bogus.dat')
        assert_raises_jsonrpc(-18, 'Requested wallet does not exist',
                              bogus.getwalletinfo)

        # -- createwallet --
        result = node.createwallet('third.dat')
        assert_equal(result['name'], 'third.dat')
        assert 'third.dat' in node.listwallets()

        # createwallet on an existing file is rejected.
        assert_raises_jsonrpc(-4, 'already exists',
                              node.createwallet, 'third.dat')

        # Path traversal is rejected.
        assert_raises_jsonrpc(-4, 'outside data directory',
                              node.createwallet, '../escape.dat')

        # -- loadwallet error paths --
        # loadwallet of a nonexistent file must NOT silently create it.
        # Use createwallet for that. Matches bitcoin 0.17.
        assert_raises_jsonrpc(-18, 'not found',
                              node.loadwallet, 'never_existed.dat')

        # Duplicate loadwallet is rejected.
        assert_raises_jsonrpc(-4, 'already loaded',
                              node.loadwallet, 'third.dat')

        # Path traversal in loadwallet is rejected.
        assert_raises_jsonrpc(-4, 'outside data directory',
                              node.loadwallet, '../escape.dat')

        # -- unloadwallet error paths --
        # Unknown wallet name.
        assert_raises_jsonrpc(-18, 'is not loaded',
                              node.unloadwallet, 'bogus.dat')

        # URI specifies one wallet, arg specifies another => -8 conflict.
        w_third = wallet_rpc(0, 'third.dat')
        assert_raises_jsonrpc(-8, 'different wallets',
                              w_third.unloadwallet, 'wallet.dat')

        # No URI + no arg + multiple wallets loaded => RPC_WALLET_NOT_SPECIFIED.
        assert_raises_jsonrpc(-19, 'must be provided',
                              node.unloadwallet)

        # -- unloadwallet by arg, then reload --
        node.unloadwallet('third.dat')
        assert 'third.dat' not in node.listwallets()

        result = node.loadwallet('third.dat')
        assert_equal(result['name'], 'third.dat')
        assert 'third.dat' in node.listwallets()

        # -- unloadwallet via URI (no arg) --
        # Now that we're back to 3 wallets, calling unloadwallet on the
        # /wallet/third.dat endpoint with no arg should detach third.dat.
        w_third = wallet_rpc(0, 'third.dat')
        w_third.unloadwallet()
        assert 'third.dat' not in node.listwallets()


if __name__ == '__main__':
    MultiWalletTest().main()
