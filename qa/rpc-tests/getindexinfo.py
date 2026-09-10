#!/usr/bin/env python3
# Copyright (c) 2026 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

from test_framework.test_framework import BitcoinTestFramework
from test_framework.mininode import wait_until
from test_framework.util import (
    assert_equal,
    start_node,
)


class GetIndexInfoTest(BitcoinTestFramework):
    def __init__(self):
        super().__init__()
        self.setup_clean_chain = False
        self.num_nodes = 1

    def setup_network(self, split=False):
        self.nodes = []
        self.nodes.append(start_node(0, self.options.tmpdir, []))
        self.is_network_split = False

    def run_test(self):
        node = self.nodes[0]

        # Without any indices running the RPC returns an empty object.
        assert_equal(node.getindexinfo(), {})

        # Restart with txindex and blockfilterindex enabled.
        self.stop_node(0)
        self.nodes[0] = start_node(0, self.options.tmpdir, ["-txindex", "-blockfilterindex"])
        node = self.nodes[0]

        wait_until(lambda: all(i["synced"] for i in node.getindexinfo().values()), timeout=120)

        index_info = node.getindexinfo()
        best_height = node.getblockcount()

        assert_equal(index_info["txindex"]["synced"], True)
        assert_equal(index_info["txindex"]["best_block_height"], best_height)

        assert_equal(index_info["basic block filter index"]["synced"], True)
        assert_equal(index_info["basic block filter index"]["best_block_height"], best_height)

        assert_equal(node.getindexinfo("txindex"), {"txindex": index_info["txindex"]})
        assert_equal(node.getindexinfo("foo"), {})


if __name__ == '__main__':
    GetIndexInfoTest().main()
