#!/usr/bin/env python3
# Copyright (c) 2022 The Dogecoin Core Developers
# Distributed under the MIT software license, see the accompanying
# file LICENSE or http://www.opensource.org/licenses/mit-license.php.
"""Stuck Transaction Test

# Tests liststucktransactions RPC endpoint
"""

from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import *
from test_framework.mininode import wait_until
from decimal import Decimal

class ListStuckTransactionsTest(BitcoinTestFramework):

    def __init__(self):
        super().__init__()
        self.setup_clean_chain = True
        self.num_nodes = 2
        self.mocktime = int(time.time())
        self.burn_addr = "n4LRQGEKcyRCXqD2MH3ompyMTJKitxu1WP" # no one has this

    def setup_nodes(self, split=False):
        nodes = []

        # Our wallet
        nodes.append(start_node(0, self.options.tmpdir,
            ["-acceptnonstdtxn=0", "-discardthreshold=0.1", "-mempoolexpiry=1", "-paytxfee=0.1"]))

        # A miner that doesn't allow tx under 1 DOGE
        nodes.append(start_node(1, self.options.tmpdir,
            ["-acceptnonstdtxn=0", "-softdustlimit=1", "-harddustlimit=1", "-mempoolexpiry=1", "-debug=net"]))

        return nodes

    def setup_network(self, split = False):
        self.nodes = self.setup_nodes()

        # set the time explicitly
        self.set_mocktime(int(time.time()))

        # connect wallet node to relay nodes
        connect_nodes_bi(self.nodes, 0, 1)

        self.is_network_split = False
        self.sync_all()

    def incrementmocktime(self, seconds):
        self.mocktime += seconds
        self.set_mocktime(self.mocktime)

    def mine_one_minute_blocks(self, miner_node, num):
        for i in range(num):
            self.incrementmocktime(60)
            time.sleep(0.05)
            miner_node.generate(1)

    def wait_for_txs(self, node, txids):
        def has_tx():
            mempool = node.getrawmempool()
            for txid in txids:
                if txid not in mempool:
                    return False
            return True
        return wait_until(has_tx, timeout=30)

    def create_custom_tx(self, source_node, source_addr, source_txout, target_addr, amount, fee):
        avail = source_node.listunspent(1, 99999, [source_addr])[source_txout]
        change = source_node.getrawchangeaddress()
        inputs = [ {'txid': avail['txid'], 'vout': avail['vout']} ]
        outputs = { target_addr: amount , change: avail['amount'] - amount - fee }
        rawtx = source_node.createrawtransaction(inputs, outputs)
        sigtx = source_node.signrawtransaction(rawtx)
        return sigtx["hex"]

    def run_test(self):
        wallet = self.nodes[0]
        miner = self.nodes[1]

        # mine 100 blocks to get some spendable coin
        miner.generate(100)
        self.sync_all()

        # make an address on the miner to be watchonly on the wallet and seed it
        watchonly_addr = miner.getnewaddress()
        wallet.importaddress(watchonly_addr)
        assert(wallet.validateaddress(watchonly_addr)["iswatchonly"])

        # seed the wallet and watchonly addresses with some coins to spend
        seed_addrs = []
        num_seeded_addrs = 2
        for i in range(num_seeded_addrs):
            address = wallet.getnewaddress()
            miner.sendtoaddress(address, 10)
            seed_addrs.append(address)
            miner.sendtoaddress(watchonly_addr, 10)

        # mine a block
        self.mine_one_minute_blocks(miner, 1)
        sync_blocks(self.nodes)

        # make sure all the transactions arrived
        assert(len(wallet.listunspent(1, 999, seed_addrs)) == num_seeded_addrs)
        assert(len(wallet.listunspent(1, 999, [watchonly_addr])) == num_seeded_addrs)

        # send out tx from wallet
        bad_wallet_tx = wallet.sendtoaddress(self.burn_addr, Decimal("9.5"))
        good_wallet_tx = wallet.sendtoaddress(self.burn_addr, Decimal("8"))

        # create watchonly tx, but send from wallet, not miner
        good_txhex = self.create_custom_tx(miner, watchonly_addr, 0, self.burn_addr, Decimal("8"), Decimal("0.1"))
        bad_txhex = self.create_custom_tx(miner, watchonly_addr, 1, self.burn_addr, Decimal("9.5"), Decimal("0.1"))
        good_watchonly_tx = wallet.sendrawtransaction(good_txhex)
        bad_watchonly_tx = wallet.sendrawtransaction(bad_txhex)

        # give it time
        self.incrementmocktime(60)
        time.sleep(0.05)

        # good txs go out, bad txs do not
        assert(self.wait_for_txs(miner, [good_wallet_tx, good_watchonly_tx]))
        for tx in [bad_wallet_tx, bad_watchonly_tx]:
            assert(tx not in miner.getrawmempool())
            assert(tx in wallet.getrawmempool())

        # restart the wallet node with parameters that also reject the bad tx
        stop_node(wallet, 0)
        wallet = start_node(0, self.options.tmpdir,
            ["-acceptnonstdtxn=0", "-softdustlimit=1", "-harddustlimit=1", "-mempoolexpiry=1"])
        self.nodes[0] = wallet

        # mine 61 blocks to expire the mempool
        self.mine_one_minute_blocks(miner, 61)

        # connect the nodes and sync
        connect_nodes_bi(self.nodes, 0, 1)
        sync_blocks(self.nodes)

        # make sure no one has the any of these tx in mempool
        for n in self.nodes:
            for tx in [bad_wallet_tx, bad_watchonly_tx, good_wallet_tx, good_watchonly_tx]:
                assert(tx not in n.getrawmempool())

        # liststucktransactions without checking watchonly should only contain
        # the bad wallet tx
        stuck_excl_watchonly = wallet.liststucktransactions(False, False)
        assert(bad_wallet_tx in stuck_excl_watchonly)
        assert(bad_watchonly_tx not in stuck_excl_watchonly)
        assert(good_wallet_tx not in stuck_excl_watchonly)
        assert(good_watchonly_tx not in stuck_excl_watchonly)

        # liststucktransactions with checking watchonly should contain both
        # the bad wallet tx and the bad watchonly tx
        stuck_incl_watchonly = wallet.liststucktransactions(False, True)
        assert(bad_wallet_tx in stuck_incl_watchonly)
        assert(bad_watchonly_tx in stuck_incl_watchonly)
        assert(good_wallet_tx not in stuck_incl_watchonly)
        assert(good_watchonly_tx not in stuck_incl_watchonly)

        # make sure the original bad tx gets fully serialized with verbose=true
        stuck_incl_watchonly_json = wallet.liststucktransactions(True, True)
        assert(any(tx['hex'] == bad_txhex for tx in stuck_incl_watchonly_json))
        assert(all(tx['hex'] != good_txhex for tx in stuck_incl_watchonly_json))

if __name__ == '__main__':
    ListStuckTransactionsTest().main()
