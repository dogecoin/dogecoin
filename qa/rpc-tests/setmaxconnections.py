#!/usr/bin/env python3
# Copyright (c) 2021 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

#
# Exercise setmaxconnections RPC command
#
# from the constants MAX_ADDNODE_CONNECTIONS and PROTECTED_INBOUND_PEERS in src/net.h
MAX_ADDNODE_CONNECTIONS = 8
PROTECTED_INBOUND_PEERS = 4 + 8 + 4 + 4
MINIMUM_CONNECTIONS = MAX_ADDNODE_CONNECTIONS
MINIMUM_CONNECTIONS_INTERNAL = MAX_ADDNODE_CONNECTIONS + PROTECTED_INBOUND_PEERS

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

        # these numbers must meet or exceed PROTECTED_INBOUND_CONNECTIONS
        # otherwise there aren't enough nodes to disconnect
        self.test_node_connection_changes(20)
        self.test_node_connection_changes(30)

        # max_count has to be at least 20
        # min_count can be closer to 20
        self.test_node_disconnections(40, 20)

    def test_rpc_argument_validation(self):
        first_node = self.nodes[0]

        try:
            first_node.setmaxconnections()
            raise AssertionError("Must check for no parameter provided")
        except JSONRPCException as e:
            assert("1. maxconnectioncount" in e.error['message'])

        try:
            first_node.setmaxconnections("good doge bad doge")
            raise AssertionError("Must check for no numeric parameter provided")
        except JSONRPCException as e:
            assert("JSON value is not an integer as expected" in e.error['message'])

        try:
            first_node.setmaxconnections(-1)
            raise AssertionError(f"Must check for parameter value >= {MINIMUM_CONNECTIONS}")
        except JSONRPCException as e:
            assert(f"maxconnectioncount must be >= {MINIMUM_CONNECTIONS}" in e.error['message'])

        try:
            first_node.setmaxconnections(7)
            raise AssertionError(f"Must check for parameter value >= {MINIMUM_CONNECTIONS}")
        except JSONRPCException as e:
            assert(f"maxconnectioncount must be >= {MINIMUM_CONNECTIONS}" in e.error['message'])

        try:
            first_node.setmaxconnections(MINIMUM_CONNECTIONS)
            assert(True)
        except JSONRPCException as e:
            raise AssertionError(f"Must allow parameter value >= {MINIMUM_CONNECTIONS}")

    def wait_for_n_disconnections(self, nodes, count, timeout):
        def disconnected():
            closed_conns = [node.peer_disconnected for node in nodes]
            print(f'Len {len(closed_conns)}, waiting for {count} of {len(nodes)} => {len(closed_conns) >= count}')
            return len(closed_conns) >= count
        return wait_until(disconnected, timeout=timeout)

    def test_node_connection_changes(self, extras):
        first_node = self.nodes[0]

        # MINIMUM_CONNECTIONS outgoing connections plus 1 feeler
        max_connections = 1 + MINIMUM_CONNECTIONS + extras
        first_node.setmaxconnections(max_connections)
        assert(first_node.getnetworkinfo()["maxconnections"] == max_connections)
        client_nodes = []

        self.connect_nodes(client_nodes, extras)
        x = first_node.getconnectioncount()
        assert(x <= extras)

        # attempt to add more nodes
        self.connect_nodes(client_nodes, 3)

        # the new nodes should not increase the connection count
        x = first_node.getconnectioncount()
        assert(x <= extras)

        first_node.setmaxconnections(MINIMUM_CONNECTIONS)


        disconnectable_connections=max_connections - MINIMUM_CONNECTIONS_INTERNAL
        assert(self.wait_for_n_disconnections(client_nodes, count=disconnectable_connections, timeout=30))

        # disconnect to clean up for the next test
        for node in client_nodes:
            node.close()

    def test_node_disconnections(self, max_count, min_count):
        first_node = self.nodes[0]

        attempted_nodes = []

        # MINIMUM_CONNECTIONS outgoing connections plus 1 feeler
        # plus 20 connections protected from eviction
        first_node.setmaxconnections(20 + 1 + MINIMUM_CONNECTIONS + max_count)
        client_nodes = []

        self.connect_nodes(client_nodes, max_count)
        x = first_node.getconnectioncount()
        assert(x <= max_count)

        first_node.setmaxconnections(min_count)

        def nodes_disconnected():
            disc_count = 0

            for node in attempted_nodes:
                if node.peer_disconnected:
                    disc_count += 1
                else:
                    node.sync_with_ping(0.1)

            if disc_count < max_count - min_count:
                return False
            return True
        wait_until(nodes_disconnected, timeout=30)

        # try asserting this two ways, for debugging the test
        x = first_node.getconnectioncount()
        assert(x < max_count)
        actual_min = max(min_count, MINIMUM_CONNECTIONS_INTERNAL)
        assert(x == actual_min)

        # disconnect to clean up for the next test
        for node in attempted_nodes:
            node.close()

if __name__ == '__main__':
    SetMaxConnectionCountTest().main()
