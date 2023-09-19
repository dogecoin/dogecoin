#!/usr/bin/env python3
# Copyright (c) 2023 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
#

from test_framework.mininode import * #NodeConnCB, NODE_NETWORK, NodeConn, wait_until
from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import *

'''
GetMockTime -- test getmocktime
'''

def wait_until_mocktime(node, time, timeout):
    def mocktime_is_set():
        return node.getmocktime() == time
    return wait_until(mocktime_is_set, timeout=timeout)

def wait_until_mocktime_change(node, time, timeout):
    def mocktime_has_changed():
        return node.getmocktime() != time
    return wait_until(mocktime_has_changed, timeout=timeout)

class GetMockTime(BitcoinTestFramework):
    def __init__(self):
        super().__init__()
        self.setup_clean_chain = True
        self.num_nodes = 1
        self.is_network_split = False

    def run_test(self):
        self.test_getmocktime()

    def setup_network(self):
        # set up full nodes
        self.nodes = []
        # Node 0 is going to be our testsubject
        self.nodes.append(start_node(0, self.options.tmpdir, ["-debug=net", "-debug=mempool", "-peertimeout=999999999"]))

    def test_getmocktime(self):
        assert wait_until_mocktime(self.nodes[0], 0, timeout=60)

        self.nodes[0].setmocktime(-0) # negative zero
        assert wait_until_mocktime(self.nodes[0], -0, timeout=60)

        self.nodes[0].setmocktime(123)
        assert wait_until_mocktime(self.nodes[0], 123, timeout=60)

        self.nodes[0].setmocktime(9223372036854775807) # int64_t max
        assert wait_until_mocktime(self.nodes[0], 9223372036854775807, timeout=60)

        try: # overflow int64_t max
            self.nodes[0].setmocktime(9223372036854775808) # int64_t max + 1
        except JSONRPCException as e:
            assert("JSON integer out of range" in e.error["message"])

        self.nodes[0].setmocktime(-9223372036854775808) # int64_t min
        assert wait_until_mocktime(self.nodes[0], -9223372036854775808, timeout=60)

        try: # overflow int64_t min
            self.nodes[0].setmocktime(-9223372036854775809) # int64_t min + 1
        except JSONRPCException as e:
            assert("JSON integer out of range" in e.error["message"])

        self.nodes[0].setmocktime(0x7fffffffffffffff) # hex
        assert wait_until_mocktime(self.nodes[0], 0x7fffffffffffffff, timeout=60)

        self.nodes[0].setmocktime(1658308113) # 4314284 timestamp
        assert wait_until_mocktime(self.nodes[0], 1658308113, timeout=60)
        assert not wait_until_mocktime_change(self.nodes[0], 1658308113, timeout=5) # this must time out

        self.nodes[0].setmocktime(1658308119) # 4314285 timestamp
        assert wait_until_mocktime(self.nodes[0], 1658308119, timeout=60)
        assert not wait_until_mocktime_change(self.nodes[0], 1658308119, timeout=5) # this must time out

        self.nodes[0].setmocktime(0) # zero
        assert wait_until_mocktime(self.nodes[0], 0, timeout=60)

if __name__ == '__main__':
    GetMockTime().main()
