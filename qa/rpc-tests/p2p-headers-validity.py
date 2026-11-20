#!/usr/bin/env python3
# Copyright (c) 2026 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
"""
p2p-headers-validity.py

Verify that a single headers message which delivers many low-work side-fork
headers scores the peer once per accepted header, not once per message.

A headers message can carry up to MAX_HEADERS_RESULTS (2000) entries. The
per-peer side-fork quota is 128, so one large batch must raise banscore.
"""

import time

from test_framework.mininode import *
from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import *
from test_framework.blocktools import create_block, create_coinbase

# Must exceed MAX_LOW_WORK_SIDEFORK_HEADERS_PER_PEER (128). Kept well below
# the main-chain length so the fork stays low-work.
SIDEFORK_BATCH = 200
# Main chain must have more work than the side-fork chain.
MAIN_CHAIN_BLOCKS = 250


class TestNode(SingleNodeConnCB):
    def __init__(self):
        SingleNodeConnCB.__init__(self)
        self.connection = None
        self.ping_counter = 1
        self.last_pong = msg_pong()

    def add_connection(self, conn):
        self.connection = conn

    def on_pong(self, conn, message):
        self.last_pong = message

    def send_message(self, message):
        self.connection.send_message(message)

    def sync_with_ping(self, timeout=30):
        self.connection.send_message(msg_ping(nonce=self.ping_counter))
        received_pong = False
        sleep_time = 0.05
        while not received_pong and timeout > 0:
            time.sleep(sleep_time)
            timeout -= sleep_time
            with mininode_lock:
                if self.last_pong.nonce == self.ping_counter:
                    received_pong = True
        self.ping_counter += 1
        return received_pong


class HeadersValidityTest(BitcoinTestFramework):
    def __init__(self):
        super().__init__()
        self.setup_clean_chain = True
        self.num_nodes = 1

    def setup_network(self):
        self.nodes = [start_node(0, self.options.tmpdir, ["-debug"])]

    def run_test(self):
        # Build a long enough main chain that a SIDEFORK_BATCH-long fork from
        # an early ancestor still has less work than the tip.
        self.nodes[0].generate(MAIN_CHAIN_BLOCKS)
        assert_equal(self.nodes[0].getblockcount(), MAIN_CHAIN_BLOCKS)

        test_node = TestNode()
        conn = NodeConn('127.0.0.1', p2p_port(0), self.nodes[0], test_node)
        test_node.add_connection(conn)
        NetworkThread().start()
        test_node.wait_for_verack()

        assert_equal(self.nodes[0].getpeerinfo()[0]['banscore'], 0)

        fork_hash = self.nodes[0].getblockhash(1)
        fork_header = self.nodes[0].getblockheader(fork_hash)
        prev = int(fork_hash, 16)
        n_time = fork_header['time'] + 1

        headers_msg = msg_headers()
        for height in range(2, 2 + SIDEFORK_BATCH):
            block = create_block(prev, create_coinbase(height), n_time)
            block.solve()
            headers_msg.headers.append(CBlockHeader(block))
            prev = block.sha256
            n_time += 1

        test_node.send_message(headers_msg)
        assert test_node.sync_with_ping()

        # If scoring were per-message the peer counter would be 1 and banscore
        # would stay 0. Counting every header in the batch exceeds the per-peer
        # quota (128) and applies 20 misbehavior points.
        assert_equal(self.nodes[0].getpeerinfo()[0]['banscore'], 20)


if __name__ == '__main__':
    HeadersValidityTest().main()
