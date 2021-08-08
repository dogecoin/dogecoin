#!/usr/bin/env python3
# Copyright (c) 2021 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
"""Hard dust limit QA test.

# Tests nodes with differing -dustlimits
"""

from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import *
from decimal import Decimal

class HardDustLimitTest(BitcoinTestFramework):

    def __init__(self):
        super().__init__()
        self.setup_clean_chain = True
        self.num_nodes = 3

    def setup_network(self, split=False):
        self.nodes = []
        self.nodes.append(start_node(0, self.options.tmpdir, ["-dustlimit=0.1", "-debug"]))
        self.nodes.append(start_node(1, self.options.tmpdir, ["-dustlimit=1", "-debug"]))
        self.nodes.append(start_node(2, self.options.tmpdir, ["-dustlimit=0.01", "-debug"]))

        connect_nodes_bi(self.nodes,0,1)
        connect_nodes_bi(self.nodes,1,2)
        connect_nodes_bi(self.nodes,0,2)

        self.is_network_split=False
        self.sync_all()

    def run_test(self):

        self.fee = Decimal("0.001")
        self.seed = 1000
        self.coinselector = {'minimumAmount': self.fee * 10, 'maximumAmount': self.seed}

        # set up addresses
        n0a1 = self.nodes[0].getnewaddress()
        n0a2 = self.nodes[0].getnewaddress()
        n0a3 = self.nodes[0].getnewaddress()
        n1a1 = self.nodes[1].getnewaddress()
        n2a1 = self.nodes[2].getnewaddress()
        n2a2 = self.nodes[2].getnewaddress()
        n2a3 = self.nodes[2].getnewaddress()
        n2a4 = self.nodes[2].getnewaddress()

        # mine some blocks and prepare some coins
        self.nodes[2].generate(1)
        self.sync_all()
        self.nodes[0].generate(101)
        self.sync_all()
        self.nodes[0].sendtoaddress(n0a1, self.seed)
        self.nodes[0].sendtoaddress(n2a1, self.seed)
        self.sync_all()
        self.nodes[0].generate(1)
        self.sync_all()

        # create dusty transactions
        self.send_dusty_tx(self.nodes[2], n2a2, n0a2, Decimal("0.9"))
        self.send_dusty_tx(self.nodes[2], n2a3, n0a3, Decimal("0.09"))
        self.send_dusty_tx(self.nodes[0], n2a4, n1a1, Decimal("1"))

        # wait 10 seconds to sync mempools
        time.sleep(10)

        assert_equal(self.nodes[2].getmempoolinfo()['size'], 3)
        assert_equal(self.nodes[1].getmempoolinfo()['size'], 1)
        assert_equal(self.nodes[0].getmempoolinfo()['size'], 2)

    def send_dusty_tx(self, n, addr1, addr2, dust):
        avail = n.listunspent(0, 1000, [], True, self.coinselector)
        inputs = [ {'txid': avail[0]['txid'], 'vout': avail[0]['vout']}]
        outputs = { addr1 : avail[0]['amount'] - self.fee - dust , addr2: dust }
        rawtx = n.createrawtransaction(inputs, outputs)
        rawtx = n.signrawtransaction(rawtx)
        n.sendrawtransaction(rawtx['hex'])

if __name__ == '__main__':
    HardDustLimitTest().main()
