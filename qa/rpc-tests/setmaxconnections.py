#!/usr/bin/env python3
# Copyright (c) 2021 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

#
# Exercise setmaxconnections RPC command
#
# from the constants MAX_OUTBOUND_CONNECTIONS, MAX_ADDNODE_CONNECTIONS and
# PROTECTED_INBOUND_PEERS in src/net.h, and the single feeler connection
MAX_OUTBOUND_CONNECTIONS = 8
MAX_FEELER_CONNECTIONS = 1
MAX_ADDNODE_CONNECTIONS = 8
PROTECTED_INBOUND_PEERS = 4 + 8 + 4 + 4
MINIMUM_CONNECTIONS = MAX_ADDNODE_CONNECTIONS
# CConnman::SetMaxConnections() never goes below this
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

    def on_close(self, conn):
        self.peer_disconnected = True

    def close(self):
        self.connection.handle_close()

def inbound_capacity(max_connections):
    """
    How many inbound peers the node accepts without evicting one, once
    setmaxconnections(max_connections) has been applied.
    """
    effective = max(max_connections, MINIMUM_CONNECTIONS_INTERNAL)
    return effective - (MAX_OUTBOUND_CONNECTIONS + MAX_FEELER_CONNECTIONS)

class SetMaxConnectionCountTest (BitcoinTestFramework):
    whitelist_local_peers = False  # connects mininode peers from 127.0.0.1

    def __init__(self):
        super().__init__()
        self.setup_clean_chain = True
        self.nodes = []
        self.client_nodes = []
        self.num_nodes = 1
        self.network_thread = None

    def setup_network(self):
        self.nodes.append(start_node(0, self.options.tmpdir, ["-debug"]))
        self.is_network_split = False

    def connect_nodes(self, nodes, count):
        """
        Open count more connections and wait until each has either completed
        its handshake or been closed by the node.
        """
        # The network thread exits when its socket map is empty, so a batch
        # that follows a full cleanup needs a new one, started only once the
        # old one has finished.
        if self.network_thread is not None and not mininode_socket_map:
            self.network_thread.join()

        new_nodes = [self.create_testnode() for _ in range(count)]
        nodes.extend(new_nodes)

        if self.network_thread is None or not self.network_thread.is_alive():
            self.network_thread = NetworkThread()
            self.network_thread.start()

        def settled():
            return all(n.verack_received or n.peer_disconnected for n in new_nodes)
        assert wait_until(settled, timeout=10)

    def create_testnode(self, node_idx=0):
        node = TestNode()
        conn = NodeConn('127.0.0.1', p2p_port(node_idx), self.nodes[node_idx], node)
        node.add_connection(conn)
        return node

    def count_disconnected(self, nodes):
        return len([n for n in nodes if n.peer_disconnected])

    def wait_for_connection_count(self, expected):
        node = self.nodes[0]
        if not wait_until(lambda: node.getconnectioncount() == expected, timeout=10):
            raise AssertionError("expected %d connections, node has %d" % (expected, node.getconnectioncount()))

    def wait_for_disconnections(self, nodes, expected):
        if not wait_until(lambda: self.count_disconnected(nodes) == expected, timeout=10):
            raise AssertionError("expected %d disconnected peers, saw %d" % (expected, self.count_disconnected(nodes)))

    def run_test(self):
        self.test_rpc_argument_validation()

        # Once the node is at its inbound capacity each further connection
        # evicts an older one at accept time. Lowering the limit afterwards
        # evicts down to the internal minimum.
        self.test_node_connection_changes(20)
        self.test_node_connection_changes(30)

        # Lowering the limit below the number of connected peers evicts
        # unprotected peers until the internal minimum is reached.
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

    def test_node_connection_changes(self, extras):
        first_node = self.nodes[0]
        attempts = 3

        # MINIMUM_CONNECTIONS outgoing connections plus 1 feeler
        max_connections = 1 + MINIMUM_CONNECTIONS + extras
        first_node.setmaxconnections(max_connections)
        capacity = inbound_capacity(max_connections)
        client_nodes = []

        self.connect_nodes(client_nodes, extras)
        assert(extras <= capacity)
        self.wait_for_connection_count(extras)
        assert_equal(self.count_disconnected(client_nodes), 0)

        # attempt to add more nodes; they must not increase the connection
        # count beyond the capacity, and the excess evicts existing peers
        self.connect_nodes(client_nodes, attempts)
        count = min(extras + attempts, capacity)
        self.wait_for_connection_count(count)
        self.wait_for_disconnections(client_nodes, extras + attempts - count)

        # lowering the limit evicts unprotected peers, but never below the
        # internal minimum
        first_node.setmaxconnections(MINIMUM_CONNECTIONS)
        count = min(count, MINIMUM_CONNECTIONS_INTERNAL)
        self.wait_for_connection_count(count)
        self.wait_for_disconnections(client_nodes, extras + attempts - count)

        # disconnect to clean up for the next test
        for node in client_nodes:
            if not node.peer_disconnected:
                node.close()
        self.wait_for_connection_count(0)

    def test_node_disconnections(self, max_count, min_count):
        first_node = self.nodes[0]

        # MINIMUM_CONNECTIONS outgoing connections plus 1 feeler
        # plus 20 connections protected from eviction
        first_node.setmaxconnections(20 + 1 + MINIMUM_CONNECTIONS + max_count)
        client_nodes = []

        self.connect_nodes(client_nodes, max_count)
        self.wait_for_connection_count(max_count)
        assert_equal(self.count_disconnected(client_nodes), 0)

        first_node.setmaxconnections(min_count)

        # the node evicts down to the larger of the requested count and its
        # internal minimum; the rest of the peers are protected from eviction
        actual_min = max(min_count, MINIMUM_CONNECTIONS_INTERNAL)
        assert(actual_min < max_count)
        self.wait_for_connection_count(actual_min)
        self.wait_for_disconnections(client_nodes, max_count - actual_min)

        # disconnect to clean up for the next test
        for node in client_nodes:
            if not node.peer_disconnected:
                node.close()
        self.wait_for_connection_count(0)

if __name__ == '__main__':
    SetMaxConnectionCountTest().main()
