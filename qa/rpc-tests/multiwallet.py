#!/usr/bin/env python3
# Copyright (c) 2017 The Bitcoin Core developers
# Copyright (c) 2026 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

"""Exercise the multi-wallet surface backported from Bitcoin Core:
   - -wallet=<file> repetition at boot
   - listwallets
   - createwallet (success / duplicate / path traversal / invalid and
     non-portable names / directory layout)
   - loadwallet (success / nonexistent / duplicate / path traversal)
   - the wallet name alone decides flat vs directory layout; a file
     squatting a directory-layout name is refused, not guessed around
   - unloadwallet (success by arg, success by /wallet URI, errors,
     repeated unload/reload, unload under concurrent RPC load)
   - per-wallet /wallet/<name> RPC routing and wallet isolation, for
     single requests, JSON-RPC batches, validateaddress and
     addmultisigaddress
   - default endpoint behaviour with 0, 1, and >1 wallets loaded
   - clean shutdown flushes and closes every wallet's BDB environment
"""

import os
import threading

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
        # Boot with two wallets so the startup loop is exercised. Extra RPC
        # threads let the concurrent-unload check run real parallel calls.
        self.nodes = start_nodes(
            self.num_nodes,
            self.options.tmpdir,
            [['-wallet=wallet.dat', '-wallet=second.dat', '-rpcthreads=8']],
        )
        self.is_network_split = False

    def walletdir(self):
        return os.path.join(self.options.tmpdir, "node0", "regtest")

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

        # -- per-wallet routing for JSON-RPC batches --
        # Every batch entry inherits the /wallet/<name>/ URI.
        batch = w2._batch([{'method': 'getwalletinfo', 'params': [], 'id': 1}])
        assert_equal(batch[0]['error'], None)
        assert_equal(batch[0]['result']['hdmasterkeyid'], info2['hdmasterkeyid'])
        batch = bogus._batch([{'method': 'getwalletinfo', 'params': [], 'id': 1}])
        assert_equal(batch[0]['error']['code'], -18)

        # -- validateaddress / addmultisigaddress use the routed wallet --
        # second.dat is not the legacy default wallet, so these only pass if
        # the key lookups go to the wallet the URI names.
        v = w2.validateaddress(addr2)
        assert v['ismine']
        assert 'pubkey' in v
        assert 'pubkey' not in w1.validateaddress(addr2)
        w2.addmultisigaddress(1, [addr2])
        assert_raises_jsonrpc(-1, 'no full public key',
                              w1.addmultisigaddress, 1, [addr2])

        # -- createwallet --
        result = node.createwallet('third.dat')
        assert_equal(result['name'], 'third.dat')
        assert 'third.dat' in node.listwallets()

        # createwallet on an existing file is rejected.
        assert_raises_jsonrpc(-4, 'already exists',
                              node.createwallet, 'third.dat')

        # Path traversal is rejected.
        for name in ['../escape.dat', '/etc/passwd', 'sub/dir/w']:
            assert_raises_jsonrpc(-4, 'resides outside',
                                  node.createwallet, name)

        # Names that are not file names at all are rejected by the name
        # check itself, not by whatever fails further down.
        for name in ['.', '..', '....']:
            assert_raises_jsonrpc(-4, 'not a valid file name',
                                  node.createwallet, name)
            assert_raises_jsonrpc(-4, 'not a valid file name',
                                  node.loadwallet, name)
        assert_raises_jsonrpc(-4, 'must not be empty', node.createwallet, '')

        # Names Windows cannot store are refused at creation, so a wallet
        # made on one platform opens on all of them.
        assert_raises_jsonrpc(-4, "contains ':'", node.createwallet, '....:')
        assert_raises_jsonrpc(-4, 'reserved device name', node.createwallet, 'nul.dat')
        assert_raises_jsonrpc(-4, "must not end with", node.createwallet, 'trailing.')
        assert not os.path.exists(os.path.join(self.walletdir(), '....:'))

        # -- directory layout: the name alone decides --
        result = node.createwallet('dirwallet')
        assert_equal(result['name'], 'dirwallet')
        assert os.path.isfile(os.path.join(self.walletdir(), 'dirwallet', 'wallet.dat'))
        wdir = wallet_rpc(0, 'dirwallet')
        assert wdir.getwalletinfo()['hdmasterkeyid'] not in (info1['hdmasterkeyid'], info2['hdmasterkeyid'])

        # A regular file squatting a no-dot name is refused with a precise
        # error instead of being opened as a flat wallet.
        squat = os.path.join(self.walletdir(), 'squat')
        with open(squat, 'w') as f:
            f.write('not a wallet')
        assert_raises_jsonrpc(-4, 'is a regular file', node.createwallet, 'squat')
        assert_raises_jsonrpc(-4, 'is a regular file', node.loadwallet, 'squat')
        assert os.path.isfile(squat)

        # -- loadwallet error paths --
        # loadwallet of a nonexistent file must NOT silently create it.
        # Use createwallet for that. Matches bitcoin 0.17.
        assert_raises_jsonrpc(-18, 'not found',
                              node.loadwallet, 'never_existed.dat')
        assert_raises_jsonrpc(-18, 'not found',
                              node.loadwallet, 'never_existed')

        # Duplicate loadwallet is rejected.
        assert_raises_jsonrpc(-4, 'already loaded',
                              node.loadwallet, 'third.dat')
        assert_raises_jsonrpc(-4, 'already loaded',
                              node.loadwallet, 'dirwallet')

        # Path traversal in loadwallet is rejected.
        assert_raises_jsonrpc(-4, 'resides outside',
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
        w_third = wallet_rpc(0, 'third.dat')
        w_third.unloadwallet()
        assert 'third.dat' not in node.listwallets()

        # -- repeated unload/reload keeps refcounts balanced --
        # A leaked database reference would make a later unload refuse
        # ("open database handle") or a reload trip Verify().
        dir_master = wdir.getwalletinfo()['hdmasterkeyid']
        for _ in range(5):
            for name in ['third.dat', 'dirwallet']:
                if name in node.listwallets():
                    node.unloadwallet(name)
                node.loadwallet(name)
                wallet_rpc(0, name).getnewaddress()
        assert_equal(wdir.getwalletinfo()['hdmasterkeyid'], dir_master)

        # -- unload while other RPCs are using the wallet --
        # Workers hammer dirwallet while it is unloaded and reloaded. Every
        # call must either succeed or cleanly report the wallet as not
        # loaded; anything else (or a crashed node) is a lifetime defect.
        self.check_unload_under_load(node, 'dirwallet')

        # -- clean shutdown closes every environment --
        # Each directory wallet has its own BDB env with a database/ log
        # directory; a clean shutdown must remove all of them, not only the
        # first wallet's.
        node.createwallet('dirwallet2')
        stop_nodes(self.nodes)
        for env_dir in ['', 'dirwallet', 'dirwallet2']:
            log_dir = os.path.join(self.walletdir(), env_dir, 'database')
            assert not os.path.exists(log_dir), "%s left behind after clean shutdown" % log_dir

        # Directory wallets load at startup from the same name.
        self.nodes = start_nodes(
            self.num_nodes, self.options.tmpdir,
            [['-wallet=wallet.dat', '-wallet=dirwallet']])
        assert_equal(sorted(self.nodes[0].listwallets()), ['dirwallet', 'wallet.dat'])
        assert_equal(wallet_rpc(0, 'dirwallet').getwalletinfo()['hdmasterkeyid'], dir_master)

    def check_unload_under_load(self, node, name):
        stop = threading.Event()
        failures = []
        calls = [0]

        def worker():
            proxy = wallet_rpc(0, name)
            while not stop.is_set():
                try:
                    proxy.getwalletinfo()
                    proxy.getbalance()
                    calls[0] += 1
                except JSONRPCException as e:
                    if e.error['code'] != -18:
                        failures.append(e.error)
                        return
                except Exception as e:
                    failures.append(repr(e))
                    return

        workers = [threading.Thread(target=worker) for _ in range(4)]
        for t in workers:
            t.start()
        try:
            for _ in range(10):
                node.unloadwallet(name)
                node.loadwallet(name)
        finally:
            stop.set()
            for t in workers:
                t.join()
        assert_equal(failures, [])
        assert calls[0] > 0
        assert name in node.listwallets()


if __name__ == '__main__':
    MultiWalletTest().main()
