#!/usr/bin/env python3
# Copyright (c) 2021 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
"""PayTxFee QA test.

# Tests wallet behavior of -paytxfee in relation to -mintxfee
"""

from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import *
from decimal import Decimal

class PayTxFeeTest(BitcoinTestFramework):

    def __init__(self):
        super().__init__()
        self.setup_clean_chain = True
        self.num_nodes = 4

    def setup_nodes(self, split=False):
        nodes = []

        # node 0 has txindex to track txs
        nodes.append(start_node(0, self.options.tmpdir,
            ["-debug", '-txindex']))

        # node 1 pays 0.1 DOGE on all txs due to implicit mintxfee = paytxfee
        nodes.append(start_node(1, self.options.tmpdir,
            ["-paytxfee=0.1", "-debug"]))

        # node 2 will always pay 1 DOGE on all txs because of explicit mintxfee
        nodes.append(start_node(2, self.options.tmpdir,
            ["-mintxfee=1", "-paytxfee=0.1", "-debug"]))

        # node 3 will always pay 0.1 DOGE on all txs despite explicit mintxfee of 0.01
        nodes.append(start_node(3, self.options.tmpdir,
            ["-mintxfee=0.01", "-paytxfee=0.1", "-debug"]))

        return nodes

    def run_test(self):

        seed = 1000 # the amount to seed wallets with
        amount = 995 # the amount to send back
        targetAddress = self.nodes[0].getnewaddress()

        # mine some blocks and prepare some coins
        self.nodes[0].generate(102)
        self.nodes[0].sendtoaddress(self.nodes[1].getnewaddress(), seed)
        self.nodes[0].sendtoaddress(self.nodes[2].getnewaddress(), seed)
        self.nodes[0].sendtoaddress(self.nodes[3].getnewaddress(), seed)
        self.nodes[0].generate(1)
        self.sync_all()

        # create transactions
        txid1 = self.nodes[1].sendtoaddress(targetAddress, amount)
        txid2 = self.nodes[2].sendtoaddress(targetAddress, amount)
        txid3 = self.nodes[3].sendtoaddress(targetAddress, amount)
        self.sync_all()

        # make sure correct fees were paid
        tx1 = self.nodes[0].getrawtransaction(txid1, True)
        tx2 = self.nodes[0].getrawtransaction(txid2, True)
        tx3 = self.nodes[0].getrawtransaction(txid3, True)

        assert_equal(tx1['vout'][0]['value'] + tx1['vout'][1]['value'], Decimal("999.9774"))
        assert_equal(tx2['vout'][0]['value'] + tx2['vout'][1]['value'], Decimal("999.774"))
        assert_equal(tx3['vout'][0]['value'] + tx3['vout'][1]['value'], Decimal("999.9774"))

        # mine a block
        self.nodes[0].generate(1);
        self.sync_all()

        # make sure all fees were mined
        block = self.nodes[0].getblock(self.nodes[0].getbestblockhash())
        coinbaseTx = self.nodes[0].getrawtransaction(block['tx'][0], True)

        assert_equal(coinbaseTx['vout'][0]['value'], Decimal("500000.2712"))

if __name__ == '__main__':
    PayTxFeeTest().main()
