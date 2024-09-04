#!/usr/bin/env python3

# Copyright (c) 2017 The Bitcoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
"""Test the RPC call related to the uptime command.
Test corresponds to code in rpc/server.cpp.
"""

import time

from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import *

class UptimeTest(BitcoinTestFramework):
    def __init__(self):
        super().__init__()

        self.num_nodes = 1
        self.setup_clean_chain = True

    def setup_network(self, split=False):
        self.nodes = []
        self.nodes.append(start_node(0, self.options.tmpdir, []))
        self.is_network_split=False
        self.sync_all()

    def run_test(self):
        self._test_uptime()

    def _test_uptime(self):
        wait_time = 10
        self.nodes[0].setmocktime(int(time.time() + wait_time))
        assert(self.nodes[0].uptime() >= wait_time)


if __name__ == '__main__':
    UptimeTest().main()