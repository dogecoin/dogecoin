#!/usr/bin/env python3
# Copyright (c) 2019 The Bitcoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
"""Test bitcoind aborts if can't disconnect a block.

- Start a single node and generate 3 blocks.
- Delete the undo data.
- Mine a fork that requires disconnecting the tip.
- Verify that bitcoind AbortNode's.
"""

from test_framework.test_framework import BitcoinTestFramework
from test_framework.mininode import wait_until
from test_framework.util import (
    assert_debug_log,
    assert_start_raises_init_error,
    connect_nodes,
    get_datadir_path,
    is_node_stopped,
)
import logging
import os

class AbortNodeTest(BitcoinTestFramework):

    def __init__(self):
        super().__init__()
        self.log = logging.getLogger("AbortNodeTest")
        self.setup_clean_chain = True
        self.num_nodes = 2

    def setup_network(self):
        self.nodes = self.setup_nodes()
        # We'll connect the nodes later

    def run_test(self):
        self.nodes[0].generate(3)
        datadir = get_datadir_path(self.options.tmpdir, 0)

        # Deleting the undo file will result in reorg failure
        os.unlink(os.path.join(datadir, 'regtest', 'blocks', 'rev00000.dat'))

        # Connecting to a node with a more work chain will trigger a reorg
        # attempt.
        self.nodes[1].generate(3)
        with assert_debug_log(self.options.tmpdir, 0, ["Failed to disconnect block"]):
            connect_nodes(self.nodes[0], 1)
            self.nodes[1].generate(1)

            # Check that node0 aborted
            self.log.info("Waiting for crash")
            assert wait_until(lambda: is_node_stopped(0), timeout=60)
        self.log.info("Node crashed - now verifying restart fails")
        assert_start_raises_init_error(0, self.options.tmpdir)

if __name__ == '__main__':
    AbortNodeTest().main()
