#!/usr/bin/env python3
# Copyright (c) 2020 The Bitcoin Core developers
# Copyright (c) 2024 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
"""
Test GETDATA processing behavior
"""

from collections import defaultdict

from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import *
from test_framework.mininode import *

class GetDataTestNode(SingleNodeConnCB):
    def __init__(self):
        SingleNodeConnCB.__init__(self)
        self.blocks = defaultdict(int) # track how many times we get a block
        self.lastpong = 0 # nonce of last pong
        self.nonce = 4919 # initial higher nonce

    def on_block(self, conn, message):
        message.block.calc_sha256()
        self.blocks[message.block.sha256] += 1

    def add_connection(self, conn):
        self.connection = conn

    def on_pong(self, conn, message):
        self.lastpong = message.nonce

    def wait_for_disconnect(self):
        if self.connection == None:
            return True
        def is_closed():
            return self.connection.state == "closed"
        return wait_until(is_closed, timeout=30)

    def wait_for_pong(self, nonce):
        def pong_received():
            return self.lastpong == nonce
        return wait_until(pong_received, timeout=10)

    def wait_for_block(self, blkhash):
        def block_received():
            return self.blocks[blkhash] == 1
        return wait_until(block_received, timeout=30)

    def disconnect(self):
        self.connection.disconnect_node()
        return self.wait_for_disconnect()

    def ping(self):
        self.nonce += 1
        self.connection.send_message(msg_ping(self.nonce))
        return self.wait_for_pong(self.nonce)


class GetDataTest(BitcoinTestFramework):

    def __init__(self):
        super().__init__()
        self.setup_clean_chain = True
        self.num_nodes = 1

    def setup_network(self, split=False):
        self.nodes = []
        self.nodes.append(start_node(0, self.options.tmpdir, []))

    def run_test(self):
        # Mine a block
        self.nodes[0].generate(1)
        best_block = int(self.nodes[0].getbestblockhash(), 16)

        # Connect test node after mining (so that we don't get any announces)
        self.testnode = GetDataTestNode()
        conn = NodeConn('127.0.0.1', p2p_port(0), self.nodes[0], self.testnode)
        self.testnode.add_connection(conn)
        NetworkThread().start()
        self.testnode.wait_for_verack()

        # Send invalid message
        invalid_getdata = msg_getdata()
        invalid_getdata.inv.append(CInv(t=0, h=0))  # INV type 0 is invalid.
        self.testnode.connection.send_message(invalid_getdata)

        # Verify that node responds to later ping
        assert(self.testnode.ping())

        # Should not have received a block message for the best block yet
        assert(self.testnode.blocks[best_block] != 1)

        # Try to get the block
        good_getdata = msg_getdata()
        good_getdata.inv.append(CInv(t=2, h=best_block))
        self.testnode.connection.send_message(good_getdata)

        # Should still respond to ping
        assert(self.testnode.ping())

        # Should have received the block
        assert(self.testnode.wait_for_block(best_block))


if __name__ == '__main__':
    GetDataTest().main()
