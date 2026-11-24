#!/usr/bin/env python3
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
"""Exercise the deferred orphan work set path in ProcessMessages.

Sends two orphan children of one parent, then the parent. Accepting the parent queues
both children; one is handled inline, the other on a later ProcessMessages pass, which
touches the orphan maps. Run against a --enable-debug build to check locking there.
"""
from test_framework.mininode import *
from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import *

class TestNode(NodeConnCB):
    def __init__(self):
        NodeConnCB.__init__(self)
        self.connection = None
        self.ping_counter = 1
        self.last_pong = msg_pong()
    def add_connection(self, conn):
        self.connection = conn
    def on_pong(self, conn, message):
        self.last_pong = message
    def wait_for_verack(self):
        return wait_until(lambda: self.verack_received, timeout=10)
    def sync_with_ping(self, timeout=30):
        self.connection.send_message(msg_ping(nonce=self.ping_counter))
        ok = wait_until(lambda: self.last_pong.nonce == self.ping_counter, timeout=timeout)
        self.ping_counter += 1
        return ok

class OrphanWorkSetTest(BitcoinTestFramework):
    def __init__(self):
        super().__init__()
        self.setup_clean_chain = True
        self.num_nodes = 1

    def setup_network(self):
        self.nodes = [start_node(0, self.options.tmpdir, ["-debug"])]
        self.peer = TestNode()
        self.peer.add_connection(NodeConn('127.0.0.1', p2p_port(0), self.nodes[0], self.peer))
        NetworkThread().start()
        self.peer.wait_for_verack()

    def make_family(self, nchildren):
        node = self.nodes[0]
        u = node.listunspent()[0]
        addrs = [node.getnewaddress() for _ in range(nchildren)]
        share = (Decimal(u['amount']) - 1) / nchildren
        share = share.quantize(Decimal('0.00000001'), rounding=ROUND_DOWN)
        parent_raw = node.createrawtransaction([{'txid': u['txid'], 'vout': u['vout']}], {a: share for a in addrs})
        parent = node.signrawtransaction(parent_raw)['hex']
        ptx = FromHex(CTransaction(), parent); ptx.rehash()
        children = []
        for n in range(nchildren):
            spk = bytes_to_hex_str(ptx.vout[n].scriptPubKey)
            prev = [{'txid': ptx.hash, 'vout': n, 'scriptPubKey': spk, 'amount': share}]
            raw = node.createrawtransaction([{'txid': ptx.hash, 'vout': n}], {node.getnewaddress(): share - 1})
            signed = node.signrawtransaction(raw, prev)
            assert signed['complete']
            ctx = FromHex(CTransaction(), signed['hex']); ctx.rehash()
            children.append(ctx)
        return ptx, children

    def run_test(self):
        node = self.nodes[0]
        node.generate(101)
        for rnd in range(5):
            parent, children = self.make_family(4)
            for c in children:
                self.peer.connection.send_message(msg_tx(c))
            self.peer.sync_with_ping()
            mp = node.getrawmempool()
            assert all(c.hash not in mp for c in children)
            self.peer.connection.send_message(msg_tx(parent))
            self.peer.sync_with_ping()
            wanted = [parent.hash] + [c.hash for c in children]
            assert wait_until(lambda: all(t in node.getrawmempool() for t in wanted), timeout=30)
            node.generate(1)
            self.log_round = rnd
        print("all orphan families resolved through the deferred work set")

if __name__ == '__main__':
    OrphanWorkSetTest().main()
