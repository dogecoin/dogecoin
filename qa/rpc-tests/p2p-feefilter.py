#!/usr/bin/env python3
# Copyright (c) 2016 The Bitcoin Core developers
# Copyright (c) 2023 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
#

from test_framework.mininode import *
from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import *
import time

'''
FeeFilterTest -- test processing of feefilter messages
'''

def hashToHex(hash):
    return format(hash, '064x')

# Wait up to 60 secs to see if the testnode has received all the expected invs
def allInvsMatch(invsExpected, testnode):
    for x in range(60):
        with mininode_lock:
            if (sorted(invsExpected) == sorted(testnode.txinvs)):
                return True
        time.sleep(1)
    return False

# TestNode: bare-bones "peer".  Used to track which invs are received from a node
# and to send the node feefilter messages.
class TestP2PConn(P2PInterface):
    def __init__(self):
        super().__init__()
        self.txinvs = []

    def on_inv(self, message):
        for i in message.inv:
            if (i.type == 1):
                self.txinvs.append(hashToHex(i.hash))

    def clear_invs(self):
        with mininode_lock:
            self.txinvs = []

class FeeFilterTest(BitcoinTestFramework):

    def set_test_params(self):
        self.num_nodes = 2

    def run_test(self):
        node1 = self.nodes[1]
        node0 = self.nodes[0]
        # Get out of IBD
        node1.generate(1)
        #sync_blocks(self.nodes)

        self.nodes[0].add_p2p_connection(TestP2PConn())
        self.nodes[0].p2p.wait_for_verack()

        # Test that invs are received for all txs at feerate of 20000 sat/byte
        node1.settxfee(Decimal("0.20000000"))
        txids = [node1.sendtoaddress(node1.getnewaddress(), 1) for x in range(3)]
        assert(allInvsMatch(txids, test_node))
        test_node.clear_invs()

        # Set a filter of 15000 sat/byte
        test_node.send_filter(15000000)

        # Test that txs are still being received (paying 20000 sat/byte)
        txids = [node1.sendtoaddress(node1.getnewaddress(), 1) for x in range(3)]
        assert(allInvsMatch(txids, test_node))
        test_node.clear_invs()

        # Change tx fee rate to 10000 sat/byte and test they are no longer received
        node1.settxfee(Decimal("0.10000000"))
        [node1.sendtoaddress(node1.getnewaddress(), 1) for x in range(3)]
        sync_mempools(self.nodes) # must be sure node 0 has received all txs 

        # Send one transaction from node0 that should be received, so that we
        # we can sync the test on receipt (if node1's txs were relayed, they'd
        # be received by the time this node0 tx is received). This is
        # unfortunately reliant on the current relay behavior where we batch up
        # to 35 entries in an inv, which means that when this next transaction
        # is eligible for relay, the prior transactions from node1 are eligible
        # as well.
        node0.settxfee(Decimal("0.20000000"))
        txids = [node0.sendtoaddress(node0.getnewaddress(), 1)]
        assert(allInvsMatch(txids, test_node))
        test_node.clear_invs()

        # Remove fee filter and check that txs are received again
        test_node.send_filter(0)
        txids = [node1.sendtoaddress(node1.getnewaddress(), 1) for x in range(3)]
        assert(allInvsMatch(txids, test_node))
        test_node.clear_invs()

if __name__ == '__main__':
    FeeFilterTest().main()
