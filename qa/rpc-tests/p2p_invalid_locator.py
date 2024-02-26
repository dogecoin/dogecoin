#!/usr/bin/env python3
# Copyright (c) 2015-2017 The Bitcoin Core developers
# Copyright (c) 2018-2022 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
"""
Test node responses to invalid locators.
"""

from test_framework.mininode import *
from test_framework.util import *
from test_framework.test_framework import BitcoinTestFramework

class TestNode(SingleNodeConnCB):
    def __init__(self):
        SingleNodeConnCB.__init__(self)
        self.last_headers = None
        self.last_block = None
        self.last_header_received = None

    def on_block(self, conn, message):
        self.last_block = message.block
        self.last_block.calc_sha256()

    def on_headers(self, conn, message):
        self.last_headers = message
        if len(message.headers):
            message.headers[-1].calc_sha256()
            self.last_header_received = message.headers[-1]

    def on_close(self, conn):
        self.disconnected = True

    def wait_for_verack(self):
        while True:
            with mininode_lock:
                if self.verack_received:
                    return
            time.sleep(0.05)

    def wait_for_block(self, blockhash, timeout=60):
        test_function = lambda: self.last_block != None and self.last_block.sha256 == blockhash
        assert(wait_until(test_function, timeout=timeout))
        return

    def wait_for_header(self, blockhash, timeout=60):
        test_function = lambda: self.last_header_received != None and self.last_header_received.sha256 == blockhash
        assert(wait_until(test_function, timeout=timeout))
        return

    # wait for the socket to be in a closed state
    def wait_for_disconnect(self, timeout=60):
        if self.connection == None:
            return True
        sleep_time = 0.05
        is_closed = self.connection.state == "closed"
        while not is_closed and timeout > 0:
            time.sleep(sleep_time)
            timeout -= sleep_time
            is_closed = self.connection.state == "closed"
        return is_closed


class InvalidLocatorTest(BitcoinTestFramework):
    def __init__(self):
        self.num_nodes = 1
        self.setup_clean_chain = False
        self.connections = []
        self.test_nodes = []

    def setup_network(self):
        self.nodes = []
        self.nodes.append(start_node(0, self.options.tmpdir, ["-debug=net"]))

        for i in range(4):
            self.test_nodes.append(TestNode())
            self.connections.append(NodeConn('127.0.0.1', p2p_port(0), self.nodes[0], self.test_nodes[i]))
            self.test_nodes[i].add_connection(self.connections[i])

        NetworkThread().start()

    def run_test(self):
        node = self.nodes[0]  # convenience reference to the node
        node.generate(1)  # Get node out of IBD

        # Test max locator size
        block_count = node.getblockcount()
        for msg in [msg_getheaders(), msg_getblocks()]:
            # Wait for disconnect when sending MAX_LOCATOR_SZ + 1 (102) hashes in locator
            test_node = self.test_nodes.pop()
            test_node.wait_for_verack()
            msg.locator.vHave = [int(node.getblockhash(i - 1), 16) for i in range(block_count, block_count - (MAX_LOCATOR_SZ + 1), -1)]
            test_node.connection.send_message(msg)
            assert_equal(test_node.wait_for_disconnect(), True)

            # Wait for response when sending MAX_LOCATOR_SZ (101) hashes in locator
            test_node = self.test_nodes.pop()
            test_node.wait_for_verack()
            msg.locator.vHave = [int(node.getblockhash(i - 1), 16) for i in range(block_count, block_count - (MAX_LOCATOR_SZ), -1)]
            test_node.connection.send_message(msg)
            if type(msg) == msg_getheaders:
                test_node.wait_for_header(int(node.getbestblockhash(), 16))
            else:
                test_node.wait_for_block(int(node.getbestblockhash(), 16))

        [ c.disconnect_node() for c in self.connections ]

if __name__ == '__main__':
    InvalidLocatorTest().main()
