#!/usr/bin/env python3
# Copyright (c) 2022 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
#

from test_framework.mininode import * #NodeConnCB, NODE_NETWORK, NetworkThread, NodeConn, wait_until, CAddress, msg_addr, msg_ping, msg_pong
from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import *
import time

'''
AddrTest -- test processing of addr messages
'''

class AddrTestNode(SingleNodeConnCB):
    def __init__(self, name):
        SingleNodeConnCB.__init__(self)
        self.ports_received = []

    def add_connection(self, conn):
        self.connection = conn

    def getaddr(self):
        self.connection.send_message(msg_getaddr())

    def on_addr(self, conn, message):
        for addr in message.addrs:
            self.ports_received.append(addr.port)

    def on_getaddr(self, conn, message):
        self.getaddr_msg_received += 1

    def wait_for_disconnect(self):
        if self.connection == None:
            return True
        def is_closed():
            return self.connection.state == "closed"
        return wait_until(is_closed, timeout=30)

    def disconnect(self):
        self.connection.disconnect_node()
        return self.wait_for_disconnect()

class AddrTest(BitcoinTestFramework):
    def __init__(self):
        super().__init__()
        self.setup_clean_chain = True
        self.num_nodes = 1
        self.counter = 0
        self.mocktime = int(time.time())
        self.start_port = 10000

    def run_test(self):
        self.nodes[0].generate(1)

        self.rate_limiting_test() # run this first so that we can test the
                                  # initial state of 1 token
        self.simple_relay_test()
        self.oversized_addr_test()

        self.send_node.disconnect()
        for recv_node in self.recv_nodes:
            recv_node.disconnect()

    def setup_network(self):
        self.nodes = []
        self.nodes.append(start_node(0, self.options.tmpdir, ["-debug=net", "-peertimeout=999999999"]))

        self.send_node = self.create_testnode()
        self.recv_nodes = []
        for i in range(4):
            self.recv_nodes.append(self.create_testnode())

        NetworkThread().start()
        self.send_node.wait_for_verack()
        for recv_node in self.recv_nodes:
            recv_node.wait_for_verack()

    def create_testnode(self, send_getaddr=False, node_idx=0):
        node = AddrTestNode(send_getaddr)
        conn = NodeConn('127.0.0.1', p2p_port(node_idx), self.nodes[node_idx], node)
        node.add_connection(conn)
        return node

    def index_to_port(self, idx):
        return self.start_port + idx

    def last_port_sent(self):
        assert self.counter > 0
        return self.index_to_port(self.counter - 1)

    def have_received_port(self, port):
        for peer in self.recv_nodes:
            if port in peer.ports_received:
                return True
        return False

    def wait_for_specific_port(self, port):
        def port_received():
            return self.have_received_port(port)
        return wait_until(port_received, timeout=600)

    def create_addr_msg(self, num, services):
        addrs = []
        for i in range(num):
            addr = CAddress()
            addr.time = self.mocktime + random.randrange(-100, 100)
            addr.nServices = services
            assert self.counter < 256 ** 2  # Don't allow the returned ip addresses to wrap.
            addr.ip = f"123.123.{self.counter // 256}.{self.counter % 256}"
            addr.port = self.index_to_port(self.counter)
            self.counter += 1
            addrs.append(addr)

        msg = msg_addr()
        msg.addrs = addrs
        return msg

    def send_addr_msg(self, msg):
        self.send_node.connection.send_message(msg)
        time.sleep(0.5) # sleep half a second to prevent mocktime racing the msg

        # invoke m_next_addr_send timer:
        # `addr` messages are sent on an exponential distribution with mean interval of 30s.
        # Setting the mocktime 600s forward gives a probability of (1 - e^-(600/30)) that
        # the event will occur (i.e. this fails once in ~500 million repeats).
        self.mocktime += 60 * 10
        self.nodes[0].setmocktime(self.mocktime)

        time.sleep(0.5) # sleep half a second to prevent pings racing mocktime
        for peer in self.recv_nodes:
            peer.sync_with_ping()

    def create_and_send_addr_msg(self, num, services=NODE_NETWORK):
        self.send_addr_msg(self.create_addr_msg(num, services))

    def simple_relay_test(self):
        # send a message with 2 addresses
        self.create_and_send_addr_msg(2)

        # make sure we received the last addr record
        assert self.wait_for_specific_port(self.last_port_sent())

    def oversized_addr_test(self):
        # create message with 1010 entries and
        # confirm that the node discarded the entries

        # to make sure we are not rate-limited, add 1001 / 0.1 seconds
        # to mocktime to allocate the maximum non-burst amount of tokens
        self.mocktime += 10010
        self.nodes[0].setmocktime(self.mocktime)

        # send one valid message, keep track of the port it contains
        valid_port_before = self.index_to_port(self.counter)
        self.create_and_send_addr_msg(1)

        # send a too large message that will be ignored
        self.create_and_send_addr_msg(1010)

        # finish with a valid message, keep track of the port it contains
        valid_port_after = self.index_to_port(self.counter)
        self.create_and_send_addr_msg(1)

        # wait until both valid addresses were propagated
        assert self.wait_for_specific_port(valid_port_before)
        assert self.wait_for_specific_port(valid_port_after)

        # make sure that all addresses from the invalid message were discarded
        # by making sure that none of them were propagated
        for port in range(valid_port_before+1, valid_port_after):
            assert not self.have_received_port(port)

    def rate_limiting_test(self):
        # send 1 addr on connect
        self.create_and_send_addr_msg(1)

        # because we set mocktime after sending the message now have
        # 600 * 0.1 = 60 tokens, minus the one we just sent.

        # send 69 tokens
        first_port = self.index_to_port(self.counter)
        self.create_and_send_addr_msg(69)

        # check that we have a peer with 60 processed addrs
        # and 10 rate limited addrs
        peerinfo = self.nodes[0].getpeerinfo()
        sendingPeer = None
        for info in peerinfo:
            if info["addr_processed"] == 60:
                sendingPeer = info
        assert not sendingPeer is None
        assert sendingPeer["addr_rate_limited"] == 10

if __name__ == '__main__':
    AddrTest().main()
