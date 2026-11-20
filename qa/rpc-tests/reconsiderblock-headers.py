#!/usr/bin/env python3
# Copyright (c) 2026 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
"""
Test that reconsiderblock keeps gFailedBlocks consistent.

invalidateblock adds a block to gFailedBlocks. reconsiderblock clears the
failure flags but, without the fix, leaves the block in the set.
AcceptBlockHeader walks the set for headers whose parent is not yet fully
validated and asserts that each matching entry still has BLOCK_FAILED_VALID,
so a stale entry aborts the node.

Two cases are checked, one per upstream change:
  A. invalidateblock X; reconsiderblock X
     (cleared in the descendant loop, bitcoin/bitcoin#11531)
  B. invalidateblock X; reconsiderblock a descendant of X
     (X is cleared in the ancestor walk, bitcoin/bitcoin#13199)
After each case, two new headers are sent on top of the tip. The second
header's parent is header-only, so AcceptBlockHeader reaches the
gFailedBlocks loop.
"""

from test_framework.mininode import *
from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import *
from test_framework.blocktools import create_block, create_coinbase


class ReconsiderBlockHeadersTest(BitcoinTestFramework):
    def __init__(self):
        super().__init__()
        self.setup_clean_chain = True
        self.num_nodes = 1

    def setup_network(self):
        self.nodes = [start_node(0, self.options.tmpdir, ["-debug"])]
        self.test_node = SingleNodeConnCB()
        self.connection = NodeConn('127.0.0.1', p2p_port(0), self.nodes[0], self.test_node)
        self.test_node.add_connection(self.connection)
        NetworkThread().start()
        self.test_node.wait_for_verack()

    def send_two_headers(self):
        node = self.nodes[0]
        tip = node.getblock(node.getbestblockhash())
        prev_hash = int(tip["hash"], 16)
        height = tip["height"]
        block_time = tip["time"]
        headers = []
        for _ in range(2):
            height += 1
            block_time += 1
            block = create_block(prev_hash, create_coinbase(height), block_time)
            block.solve()
            headers.append(CBlockHeader(block))
            prev_hash = block.sha256
        msg = msg_headers()
        msg.headers = headers
        self.test_node.send_and_ping(msg)

        # The node survived, and it accepted both headers as a headers-only tip.
        tips = node.getchaintips()
        assert({"height": height, "hash": "%064x" % prev_hash, "branchlen": 2, "status": "headers-only"}
               in [{k: t[k] for k in ("height", "hash", "branchlen", "status")} for t in tips])

    def run_test(self):
        node = self.nodes[0]
        node.generate(20)

        print("Case A: invalidateblock X; reconsiderblock X")
        tip = node.getbestblockhash()
        x = node.getblockhash(15)
        node.invalidateblock(x)
        assert_equal(node.getblockcount(), 14)
        node.reconsiderblock(x)
        assert_equal(node.getbestblockhash(), tip)
        self.send_two_headers()

        print("Case B: invalidateblock X; reconsiderblock a descendant of X")
        node.generate(5)  # connects past the header-only branch from case A
        tip = node.getbestblockhash()
        x = node.getblockhash(18)
        y = node.getblockhash(21)
        node.invalidateblock(x)
        assert_equal(node.getblockcount(), 17)
        node.reconsiderblock(y)
        assert_equal(node.getbestblockhash(), tip)
        self.send_two_headers()

        self.connection.disconnect_node()


if __name__ == '__main__':
    ReconsiderBlockHeadersTest().main()
