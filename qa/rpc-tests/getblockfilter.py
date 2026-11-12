#!/usr/bin/env python3
# Copyright (c) 2018 The Bitcoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

"""Test the getblockfilter RPC."""

from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import (
    assert_equal,
    assert_is_hex_string,
    assert_raises_jsonrpc,
    connect_nodes_bi,
    start_nodes,
    sync_blocks,
)


class GetBlockFilterTest(BitcoinTestFramework):
    def __init__(self):
        super().__init__()
        self.setup_clean_chain = True
        self.num_nodes = 2

    def setup_network(self):
        self.nodes = start_nodes(
            self.num_nodes,
            self.options.tmpdir,
            extra_args=[["-blockfilterindex"], []],
        )
        self.is_network_split = True

    def run_test(self):
        self.nodes[0].generate(3)
        self.nodes[1].generate(4)

        assert_equal(self.nodes[0].getblockcount(), 3)
        chain0_hashes = [self.nodes[0].getblockhash(height) for height in range(4)]

        connect_nodes_bi(self.nodes, 0, 1)
        self.is_network_split = False
        sync_blocks(self.nodes)

        assert_equal(self.nodes[0].getblockcount(), 4)
        chain1_hashes = [self.nodes[0].getblockhash(height) for height in range(5)]

        for block_hash in chain1_hashes:
            result = self.nodes[0].getblockfilter(block_hash, "basic")
            assert_is_hex_string(result["filter"])
            assert_is_hex_string(result["header"])

        for block_hash in chain0_hashes:
            result = self.nodes[0].getblockfilter(block_hash, "basic")
            assert_is_hex_string(result["filter"])
            assert_is_hex_string(result["header"])

        bad_block_hash = "0123456789abcdef" * 4
        assert_raises_jsonrpc(-5, "Block not found", self.nodes[0].getblockfilter, bad_block_hash, "basic")

        genesis_hash = self.nodes[0].getblockhash(0)
        assert_raises_jsonrpc(-5, "Unknown filtertype", self.nodes[0].getblockfilter, genesis_hash, "unknown")


if __name__ == '__main__':
    GetBlockFilterTest().main()