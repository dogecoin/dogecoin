#!/usr/bin/env python3
# Copyright (c) 2021 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

#
# Exercise setmaxconnections RPC command
#

from test_framework.mininode import *
from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import *

class TestNode(NodeConnCB):
    def __init__(self):
        NodeConnCB.__init__(self)
        self.connection = None
        self.peer_disconnected = False
        self.ping_counter = 0

    def add_connection(self, conn):
        self.connection = conn

    def close(self):
        self.connection.handle_close()
        self.peer_disconnected = True

class SetMaxConnectionCountTest (BitcoinTestFramework):

    def __init__(self):
        super().__init__()
        self.setup_clean_chain = True
        self.nodes = []
        self.client_nodes = []
        self.num_nodes = 1

    def create_testnode(self, node_idx=0):
        node = TestNode()
        conn = NodeConn('127.0.0.1', p2p_port(node_idx), self.nodes[node_idx], node)
        node.add_connection(conn)
        return node

    def setup_network(self):
        self.nodes.append(start_node(0, self.options.tmpdir, ["-debug", "-nmaxconnections=0"]))
        self.is_network_split = False
        NetworkThread().start()

    def wait_for_verack(self, nodes, timeout_seconds=5):
        def nodes_veracked():
            for node in nodes:
                if not node.verack_received:
                    return False

            return True
        wait_until(nodes_veracked, timeout=timeout_seconds)

    def connect_nodes(self, nodes, count):
        for i in range(0, count):
            nodes.append(self.create_testnode())

        self.wait_for_verack(nodes)

    def run_test(self):
        self.test_rpc_argument_validation()
        self.test_node_connection_changes(5)
        self.test_node_connection_changes(10)
        self.test_node_connection_changes(3)

    def test_rpc_argument_validation(self):
        first_node = self.nodes[0]

        try:
            first_node.setmaxconnections()
            raise AssertionError("Must check for no parameter provided")
        except JSONRPCException as e:
            assert("1. \"maxconnectioncount\"" in e.error['message'])

        try:
            first_node.setmaxconnections("good doge bad doge")
            raise AssertionError("Must check for no numeric parameter provided")
        except JSONRPCException as e:
            assert("JSON value is not an integer as expected" in e.error['message'])

        try:
            first_node.setmaxconnections(-1)
            raise AssertionError("Must check for parameter value >= 0")
        except JSONRPCException as e:
            assert("maxconnectioncount must be >= 0" in e.error['message'])

        try:
            first_node.setmaxconnections(0)
            assert(True)
        except JSONRPCException as e:
            raise AssertionError("Must allow parameter value >= 0")

    def test_node_connection_changes(self, extras):
        first_node = self.nodes[0]

        # 9 is 8 outgoing connections plus 1 feeler
        first_node.setmaxconnections(9 + extras)
        client_nodes = []

        self.connect_nodes(client_nodes, extras)
        x = first_node.getconnectioncount()
        assert(x == extras)

        # attempt to add more nodes
        self.connect_nodes(client_nodes, 3)

        # the new nodes should not increase the connection count
        x = first_node.getconnectioncount()
        assert(x == extras)

        for node in client_nodes:
            node.close()

        first_node.setmaxconnections(0)
        x = first_node.getconnectioncount()
        assert(x == 0)

if __name__ == '__main__':
    SetMaxConnectionCountTest().main()
