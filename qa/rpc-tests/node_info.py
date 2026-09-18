#!/usr/bin/env python3
# Copyright (c) 2026 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

#
# Contract tests for the node's informational and network control RPC calls, so
# that changes to the shape of their results are noticed rather than discovered
# by whatever was parsing them. Tests correspond to code in rpc/blockchain.cpp,
# rpc/mining.cpp, rpc/misc.cpp and rpc/net.cpp.
#
#   - getdifficulty
#   - getnetworkhashps
#   - getmemoryinfo
#   - getnettotals
#   - getaddednodeinfo
#   - setnetworkactive
#

import time
from decimal import Decimal

from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import (
    assert_equal,
    assert_greater_than,
    assert_greater_than_or_equal,
    assert_raises_jsonrpc,
    connect_nodes_bi,
    p2p_port,
    start_node,
)

# rpc/protocol.h
RPC_MISC_ERROR = -1
RPC_CLIENT_NODE_NOT_ADDED = -24


def wait_until(predicate, timeout=30):
    """Poll predicate until it holds, so that the tests do not depend on how
    quickly the connection manager gets around to acting on an RPC call."""
    deadline = time.time() + timeout
    while time.time() < deadline:
        if predicate():
            return True
        time.sleep(0.25)
    return False


class NodeInfoTest(BitcoinTestFramework):
    def __init__(self):
        super().__init__()
        self.setup_clean_chain = False
        self.num_nodes = 2

    def setup_network(self, split=False):
        self.nodes = [
            start_node(0, self.options.tmpdir),
            start_node(1, self.options.tmpdir),
        ]
        connect_nodes_bi(self.nodes, 0, 1)
        self.is_network_split = False
        self.sync_all()

    def run_test(self):
        self._test_getdifficulty()
        self._test_getnetworkhashps()
        self._test_getmemoryinfo()
        self._test_getnettotals()
        self._test_getaddednodeinfo()
        self._test_setnetworkactive()

    def _test_getdifficulty(self):
        node = self.nodes[0]

        difficulty = node.getdifficulty()
        assert isinstance(difficulty, Decimal)
        assert_greater_than(difficulty, 0)

        # the same number the other two RPCs that report it are reading
        assert_equal(difficulty, node.getblockchaininfo()['difficulty'])
        assert_equal(difficulty, node.getmininginfo()['difficulty'])

        assert_raises_jsonrpc(RPC_MISC_ERROR, "getdifficulty", node.getdifficulty, 1)

    def _test_getnetworkhashps(self):
        node = self.nodes[0]

        # the cached chain is mined a minute apart, so there is a time span to
        # divide the work over and the estimate comes out above zero
        hashps = node.getnetworkhashps()
        assert_greater_than(hashps, 0)

        # and it is the number getmininginfo reports, which calls straight through
        assert_equal(hashps, node.getmininginfo()['networkhashps'])

        # an explicit window is accepted, as is -1 for "since the last
        # difficulty change", and a window longer than the chain is clamped
        assert_greater_than(node.getnetworkhashps(10), 0)
        assert_greater_than(node.getnetworkhashps(-1), 0)
        assert_equal(node.getnetworkhashps(node.getblockcount() * 10),
                     node.getnetworkhashps(node.getblockcount()))

        # the second argument estimates at the time a given block was found, and
        # the genesis block has no predecessor to measure against
        assert_equal(node.getnetworkhashps(120, 0), 0)
        assert_greater_than(node.getnetworkhashps(120, node.getblockcount() - 1), 0)

        assert_raises_jsonrpc(RPC_MISC_ERROR, "getnetworkhashps",
                              node.getnetworkhashps, 120, 0, 0)

    def _test_getmemoryinfo(self):
        node = self.nodes[0]

        info = node.getmemoryinfo()
        assert_equal(set(info.keys()), {'locked'})

        locked = info['locked']
        assert_equal(set(locked.keys()),
                     {'used', 'free', 'total', 'locked', 'chunks_used', 'chunks_free'})
        for key, value in locked.items():
            assert isinstance(value, int), "%s should be numeric" % key
            assert_greater_than_or_equal(value, 0)

        # the arena is accounted for in full, and no more can be locked than
        # is managed in the first place
        assert_equal(locked['total'], locked['used'] + locked['free'])
        assert_greater_than_or_equal(locked['total'], locked['locked'])

        assert_raises_jsonrpc(RPC_MISC_ERROR, "getmemoryinfo", node.getmemoryinfo, 1)

    def _test_getnettotals(self):
        node = self.nodes[0]

        totals = node.getnettotals()
        assert_equal(set(totals.keys()),
                     {'totalbytesrecv', 'totalbytessent', 'timemillis', 'uploadtarget'})
        assert_greater_than(totals['totalbytesrecv'], 0)
        assert_greater_than(totals['totalbytessent'], 0)
        assert_greater_than(totals['timemillis'], 0)

        target = totals['uploadtarget']
        assert_equal(set(target.keys()),
                     {'timeframe', 'target', 'target_reached',
                      'serve_historical_blocks', 'bytes_left_in_cycle',
                      'time_left_in_cycle'})

        # with no -maxuploadtarget set there is no limit to reach, so historical
        # blocks stay on offer and the cycle counters are not counting anything
        assert_equal(target['target'], 0)
        assert_equal(target['target_reached'], False)
        assert_equal(target['serve_historical_blocks'], True)
        assert_equal(target['bytes_left_in_cycle'], 0)
        assert_equal(target['time_left_in_cycle'], 0)
        assert_greater_than(target['timeframe'], 0)

        # the byte counters are cumulative, so traffic only ever adds to them
        node.generate(1)
        self.sync_all()
        later = node.getnettotals()
        assert_greater_than_or_equal(later['totalbytesrecv'], totals['totalbytesrecv'])
        assert_greater_than_or_equal(later['totalbytessent'], totals['totalbytessent'])
        assert_greater_than_or_equal(later['timemillis'], totals['timemillis'])

        assert_raises_jsonrpc(RPC_MISC_ERROR, "getnettotals", node.getnettotals, 1)

    def _test_getaddednodeinfo(self):
        node = self.nodes[0]

        # nothing has been added yet: the peer connected in setup_network was
        # connected directly, not through addnode
        assert_equal(node.getaddednodeinfo(), [])

        # asking after a node that was never added is an error rather than an
        # empty answer, so that a typo does not look like a disconnected peer
        assert_raises_jsonrpc(RPC_CLIENT_NODE_NOT_ADDED, "Node has not been added",
                              node.getaddednodeinfo, "127.0.0.1:1")

        added = "127.0.0.1:" + str(p2p_port(1))
        node.addnode(added, "add")

        info = node.getaddednodeinfo()
        assert_equal(len(info), 1)
        assert_equal(set(info[0].keys()), {'addednode', 'connected', 'addresses'})
        assert_equal(info[0]['addednode'], added)

        # the node is reported back by name whether or not it is connected yet
        assert_equal(node.getaddednodeinfo(added), info)

        # and once the connection manager has got around to dialling it, the
        # resolved address it connected out to is listed
        assert wait_until(lambda: node.getaddednodeinfo()[0]['connected']), \
            "added node never reported as connected"
        addresses = node.getaddednodeinfo()[0]['addresses']
        assert_equal(len(addresses), 1)
        assert_equal(set(addresses[0].keys()), {'address', 'connected'})
        assert_equal(addresses[0]['address'], added)
        assert_equal(addresses[0]['connected'], "outbound")

        # a onetry attempt is a one-off dial and is documented as not being
        # tracked as an added node
        node.addnode("127.0.0.1:" + str(p2p_port(0)), "onetry")
        assert_equal([entry['addednode'] for entry in node.getaddednodeinfo()], [added])

        node.addnode(added, "remove")
        assert_equal(node.getaddednodeinfo(), [])

        assert_raises_jsonrpc(RPC_MISC_ERROR, "getaddednodeinfo",
                              node.getaddednodeinfo, added, 1)

    def _test_setnetworkactive(self):
        node = self.nodes[0]

        assert_equal(node.getnetworkinfo()['networkactive'], True)
        assert_greater_than(node.getconnectioncount(), 0)

        # the call answers with the state it has just put the node into, and
        # turning networking off drops the peers that were connected
        assert_equal(node.setnetworkactive(False), False)
        assert_equal(node.getnetworkinfo()['networkactive'], False)
        assert wait_until(lambda: node.getconnectioncount() == 0), \
            "peers still connected after networking was disabled"

        # asking again for a state it is already in is not an error
        assert_equal(node.setnetworkactive(False), False)

        assert_equal(node.setnetworkactive(True), True)
        assert_equal(node.getnetworkinfo()['networkactive'], True)

        assert_raises_jsonrpc(RPC_MISC_ERROR, "setnetworkactive",
                              node.setnetworkactive)
        assert_raises_jsonrpc(RPC_MISC_ERROR, "setnetworkactive",
                              node.setnetworkactive, True, True)

        # leave the network as run_test found it
        connect_nodes_bi(self.nodes, 0, 1)
        self.sync_all()


if __name__ == '__main__':
    NodeInfoTest().main()
