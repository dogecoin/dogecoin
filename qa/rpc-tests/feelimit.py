#!/usr/bin/env python3
# Copyright (c) 2021 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
"""Fee limit QA test.
# Tests nodes under fee limit, verifies fee rounding
"""

from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import *
from decimal import Decimal

class FeeLimitTest(BitcoinTestFramework):

    def __init__(self):
        super().__init__()
        self.setup_clean_chain = True
        self.num_nodes = 2

    def setup_network(self, split=False):
        self.nodes = []
        self.nodes.append(start_node(0, self.options.tmpdir, []))
        self.nodes.append(start_node(1, self.options.tmpdir, []))

        connect_nodes_bi(self.nodes,0,1)

        self.is_network_split=False
        self.sync_all()

    def run_test(self):
        # mine some blocks
        self.nodes[0].generate(101)
        self.sync_all()

        # Output address
        addr_to = self.nodes[1].getnewaddress()
        lower_fee_per_kb = Decimal("0.001")

        # Force generating a transaction with 1.14.5-like fees by manually building the tx
        utx = self.nodes[0].listunspent()[0]
        inputs  = [ {'txid' : utx['txid'], 'vout' : utx['vout']}]
        outputs = { addr_to : utx['amount'] - Decimal("1.0") }
        rawtx   = self.nodes[0].createrawtransaction(inputs, outputs)

        # Work out how big this transaction would be, then change the output to match
        tx_size = count_bytes(self.nodes[0].signrawtransaction(rawtx)['hex'])
        fee = lower_fee_per_kb * (tx_size + 1) / Decimal("1000")
        outputs = { addr_to : utx['amount'] - fee }
        rawtx   = self.nodes[0].createrawtransaction(inputs, outputs)
        signedtx = self.nodes[0].signrawtransaction(rawtx)
        self.nodes[0].sendrawtransaction(signedtx["hex"])

        # Sync all of the nodes
        self.sync_all()

        # Check if the TX made it to node 2
        assert_equal(self.nodes[1].getmempoolinfo()['size'], 1)
        assert_equal(self.nodes[0].getmempoolinfo()['size'], 1)

        # Force generating a transaction with fees which are too low to relay
        too_low_fee = lower_fee_per_kb * (tx_size - 4) / Decimal("1000")
        utx = self.nodes[0].listunspent()[0]
        inputs  = [ {'txid' : utx['txid'], 'vout' : utx['vout']}]
        outputs = { self.nodes[1].getnewaddress() : utx['amount'] - too_low_fee }
        rawtx   = self.nodes[0].createrawtransaction(inputs, outputs)
        signedtx = self.nodes[0].signrawtransaction(rawtx)
        self.nodes[0].sendrawtransaction(signedtx["hex"])

        # wait 10 seconds to sync mempools
        time.sleep(10)

        # Check the TX did not relay, so is only on node 0
        assert_equal(self.nodes[1].getmempoolinfo()['size'], 1)
        assert_equal(self.nodes[0].getmempoolinfo()['size'], 2)

if __name__ == '__main__':
    FeeLimitTest().main()
