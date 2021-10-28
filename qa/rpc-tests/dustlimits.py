#!/usr/bin/env python3
# Copyright (c) 2021 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
"""Dust limit QA test.

# Tests nodes with differing mempool/relay dust limits
"""

from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import *
from decimal import Decimal

class DustLimitTest(BitcoinTestFramework):

    def __init__(self):
        super().__init__()
        self.setup_clean_chain = True
        self.num_nodes = 4

        # set up receiving addresses outside of nodes' wallets
        self.recv_1 = "n4LRQGEKcyRCXqD2MH3ompyMTJKitxu1WP"
        self.recv_2 = "n1eAe5K2AQUtbmMxVzWnGAyq4hkWJdse2x"

        # seed moneys
        self.seed = 100

    def setup_nodes(self, split=False):
        nodes = []

        # 1.10.0-like node with only a soft dust limit
        nodes.append(start_node(0, self.options.tmpdir,
            ["-acceptnonstdtxn=0", "-dustlimit=1", "-harddustlimit=0.0", "-minrelaytxfee=1", "-debug"]))

        # 1.14.2-like node with only a hard dust limit
        nodes.append(start_node(1, self.options.tmpdir,
            ["-acceptnonstdtxn=0", "-dustlimit=1", "-harddustlimit=1", "-minrelaytxfee=1", "-debug"]))

        # 1.14.5-like node with a lower, different hard and soft dust limit
        nodes.append(start_node(2, self.options.tmpdir,
            ["-acceptnonstdtxn=0", "-dustlimit=0.01", "-harddustlimit=0.001", "-debug"]))

        # node that should accept everything
        nodes.append(start_node(3, self.options.tmpdir,
            ["-acceptnonstdtxn=0", "-dustlimit=0.0", "-harddustlimit=0.0", "-minrelaytxfee=0.00000001", "-debug"]))

        return nodes

    def setup_network(self, split = False):
        self.nodes = self.setup_nodes()

        # connect everything to everything
        for s in range(0, self.num_nodes):
            for t in range(s+1, self.num_nodes):
                connect_nodes_bi(self.nodes, s, t)

        self.is_network_split = False
        self.sync_all()

    def run_test(self):

        # make sure the dust limits got configured
        self.check_dust_config(self.nodes[0], Decimal("1.0"), Decimal("0.0"))
        self.check_dust_config(self.nodes[1], Decimal("1.0"), Decimal("1.0"))
        self.check_dust_config(self.nodes[2], Decimal("0.01"), Decimal("0.001"))
        self.check_dust_config(self.nodes[3], Decimal("0.0"), Decimal("0.0"))

        # set up 10 seeded addresses for node 0-2
        addrs = []
        for i in range(3):
            for _ in range(10):
                addrs.append(self.nodes[i].getnewaddress())

        # mine some blocks and prepare some coins
        self.nodes[2].generate(1)
        self.sync_all()
        self.nodes[0].generate(101)
        self.sync_all()
        for addr in addrs:
            self.nodes[0].sendtoaddress(addr, self.seed)
        self.nodes[0].generate(1)
        self.sync_all()

        # create dusty transactions

        txids = [
            self.send_dusty_tx(self.nodes[1], Decimal("1"), Decimal("1")), # goes to all
            self.send_dusty_tx(self.nodes[0], Decimal("0.9"), Decimal("2")), # goes to 3/4
            self.send_dusty_tx(self.nodes[0], Decimal("0.0009"), Decimal("2")), # goes to 3/4
            self.send_dusty_tx(self.nodes[2], Decimal("0.001"), Decimal("2")), # goes to 3/4
            self.send_dusty_tx(self.nodes[2], Decimal("0.001"), Decimal("0.02")), # goes to 2/4
        ]

        # nodes do not accept dust under their hard dust limit
        # no matter how much fee is paid
        self.get_dust_rejection(self.nodes[2], Decimal("0.0009"), Decimal("5"))
        self.get_dust_rejection(self.nodes[1], Decimal("0.9"), Decimal("5"))

        # wait 15 seconds to sync mempools
        time.sleep(15)

        assert_equal(self.nodes[0].getmempoolinfo()['size'], 4) # 4 of 5
        assert_equal(self.nodes[1].getmempoolinfo()['size'], 1) # 1 of 5
        assert_equal(self.nodes[2].getmempoolinfo()['size'], 4) # 4 of 5
        assert_equal(self.nodes[3].getmempoolinfo()['size'], 5) # all

        # check each tx
        i = 0
        for checktx in [[0,1,2,3], [0], [0,1,3,4], [0,1,2,3,4]]:
            for idx in checktx:
                assert(txids[idx] in self.nodes[i].getrawmempool())
            i += 1

        # mining the 1 tx known to node 1
        self.nodes[1].generate(1)
        sync_blocks(self.nodes)

        assert_equal(self.nodes[0].getmempoolinfo()['size'], 3) # 3 of 4
        assert_equal(self.nodes[1].getmempoolinfo()['size'], 0) # none left
        assert_equal(self.nodes[2].getmempoolinfo()['size'], 3) # 3 of 4
        assert_equal(self.nodes[3].getmempoolinfo()['size'], 4) # all

        # check each tx
        i = 0
        for checktx in [[1,2,3], [], [1,3,4], [1,2,3,4]]:
            for idx in checktx:
                assert(txids[idx] in self.nodes[i].getrawmempool())
            i += 1

        # mine the 3 tx known to node 0
        self.nodes[0].generate(1)
        sync_blocks(self.nodes)

        # now only the last tx is left
        assert_equal(self.nodes[0].getmempoolinfo()['size'], 0) # none left
        assert_equal(self.nodes[1].getmempoolinfo()['size'], 0) # none left
        assert_equal(self.nodes[2].getmempoolinfo()['size'], 1) # all
        assert_equal(self.nodes[3].getmempoolinfo()['size'], 1) # all

        # check each tx on the remaining nodes
        for i in [2,3]:
            assert(txids[4] in self.nodes[i].getrawmempool())

        # after mining the last tx from node 2, all nodes should have empty mempools
        self.nodes[2].generate(1)
        self.sync_all()

        assert_equal(self.nodes[0].getmempoolinfo()['size'], 0)
        assert_equal(self.nodes[1].getmempoolinfo()['size'], 0)
        assert_equal(self.nodes[2].getmempoolinfo()['size'], 0)
        assert_equal(self.nodes[3].getmempoolinfo()['size'], 0)

        print("such success. wow!")

    def create_dusty_tx(self, n, dust, fee):
        minAmount =  5 * (dust + fee)
        avail = n.listunspent(0, 1000, [], True, {'minimumAmount': minAmount})[0]
        inputs = [ {'txid': avail['txid'], 'vout': avail['vout']} ]
        outputs = { self.recv_1 : avail['amount'] - fee - dust , self.recv_2: dust }
        rawtx = n.createrawtransaction(inputs, outputs)
        return n.signrawtransaction(rawtx)

    def send_dusty_tx(self, n, dust, fee):
        rawtx = self.create_dusty_tx(n, dust, fee)
        return n.sendrawtransaction(rawtx['hex'])

    def get_dust_rejection(self, n, dust, fee):
        rawtx = self.create_dusty_tx(n, dust, fee)
        assert_raises_jsonrpc(-26, "dust", n.sendrawtransaction, rawtx['hex'])

    def check_dust_config(self, n, soft, hard):
        networkinfo = n.getnetworkinfo()
        assert_equal(networkinfo["softdustlimit"], soft)
        assert_equal(networkinfo["harddustlimit"], hard)

if __name__ == '__main__':
    DustLimitTest().main()
