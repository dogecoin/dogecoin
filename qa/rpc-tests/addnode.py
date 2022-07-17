#!/usr/bin/env python3
# Copyright (c) 2021-2022 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
"""Addnode QA test.
# Tests the addnode command
"""

from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import *

class AddnodeTest(BitcoinTestFramework):

    def __init__(self):
        super().__init__()
        self.setup_clean_chain = True
        self.num_nodes = 1
        self.counter = 0

    def setup_network(self, split=False):
        self.nodes = []
        self.nodes.append(start_node(0, self.options.tmpdir, []))

        self.is_network_split=False
        self.sync_all()

    def get_next_dummy_address(self):
        self.counter += 1
        assert self.counter < 256 ** 2  # Don't allow the returned ip addresses to wrap.
        return f"123.123.{self.counter // 256}.{self.counter % 256}:{10000 + self.counter}"

    def run_test(self):
        node = self.nodes[0]
        # mine a block
        node.generate(1)

        # add one address
        first_addr = self.get_next_dummy_address()
        node.addnode(first_addr, 'add')

        # try adding it again
        try:
            node.addnode(first_addr, 'add')
            raise AssertionError("Must reject an existing addnode")
        except JSONRPCException as e:
            assert("Error: Unable to add node" in e.error["message"])

        # add 799 valid addresses - must not throw an error
        for i in range((8 * 100) - 1):
            node.addnode(self.get_next_dummy_address(), 'add')

        # add one more address
        try:
            node.addnode(self.get_next_dummy_address(), 'add')
            raise AssertionError("Must reject when more than 800 addnode entries exist")
        except JSONRPCException as e:
            assert("Error: Unable to add node" in e.error["message"])

        # try adding a large string domain
        try:
            node.addnode(f'{"manybigstr" * 25}.domain:10020', 'add')
            raise AssertionError("Must reject large strings")
        except JSONRPCException as e:
            assert("Error: Node address is invalid" in e.error["message"])

if __name__ == '__main__':
    AddnodeTest().main()
