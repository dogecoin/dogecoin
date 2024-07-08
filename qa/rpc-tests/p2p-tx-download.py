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
TxDownload -- test transaction download logic
'''

GETDATA_TX_INTERVAL = 30  # seconds
TX_EXPIRY_INTERVAL = 10 * GETDATA_TX_INTERVAL # 5 minutes
INBOUND_PEER_TX_DELAY = 2  # seconds
OVERLOADED_PEER_DELAY = 2 # seconds
MAX_GETDATA_IN_FLIGHT = 100
MAX_PEER_TX_ANNOUNCEMENTS = 5000

MAX_GETDATA_INBOUND_WAIT = GETDATA_TX_INTERVAL + INBOUND_PEER_TX_DELAY

class TxDownloadTestNode(SingleNodeConnCB):
    def __init__(self):
        SingleNodeConnCB.__init__(self)
        self.tx_getdata_received = []

    def add_connection(self, conn):
        self.connection = conn

    def send_tx_inv(self, hashes):
        invs = []
        for hash in hashes:
            invs.append(CInv(1, hash))
        self.connection.send_message(msg_inv(invs))

    def send_tx_notfound(self, hashes):
        vec = []
        for hash in hashes:
            vec.append(CInv(1, hash))
        self.connection.send_message(msg_notfound(vec))

    def on_getdata(self, conn, message):
        for entry in message.inv:
            if entry.type == 1:
                self.tx_getdata_received.append(entry.hash)

    def wait_for_disconnect(self):
        if self.connection == None:
            return True
        def is_closed():
            return self.connection.state == "closed"
        return wait_until(is_closed, timeout=30)

    def wait_until_numgetdata(self, num):
        def has_num():
            return len(self.tx_getdata_received) == num
        return wait_until(has_num, timeout=60)

    def disconnect(self):
        self.connection.disconnect_node()
        return self.wait_for_disconnect()

class TxDownloadTest(BitcoinTestFramework):
    def __init__(self):
        super().__init__()
        self.setup_clean_chain = True
        self.num_nodes = 2
        self.counter = 0
        self.mocktime = int(time.time())
        self.is_network_split = False

        # very fake txid that can be incremented
        self.fake_txid = 0xdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef

        # valid regtest address that no one has the key to
        self.tgtAddr = "mkwDHkWXF8x6aFtdGVm5E9PVC7yPY8cb4r"

    def run_test(self):
        self.nodes[1].generate(1)
        self.sync_all()
        self.nodes[0].generate(100)
        self.sync_all()

        self.test_tx_request()
        self.test_invblock_resolution()
        self.test_disconnect_fallback()
        self.test_notfound_fallback()
        self.test_max_announcements()
        self.test_inflight_throttling()
        self.test_expiry_fallback()

    def setup_network(self):
        # set up full nodes
        self.nodes = []
        # Node 0 is going to be our testsubject that is connected to a bunch of nasty peers
        self.nodes.append(start_node(0, self.options.tmpdir, ["-debug=net", "-debug=mempool", "-peertimeout=999999999"]))
        # Node 1 is going to be our honest peer
        self.nodes.append(start_node(1, self.options.tmpdir, ["-debug=net", "-debug=mempool", "-peertimeout=999999999"]))

        # Connect Node 0 to Node 1 as outgoing
        connect_nodes(self.nodes[0], 1)
        self.sync_all()

        # create a single control peer that is only used for ping sync
        self.control_peer = self.create_testnode()
        NetworkThread().start()
        self.control_peer.wait_for_verack()

    def create_testnode(self, node_idx=0):
        node = TxDownloadTestNode()
        conn = NodeConn('127.0.0.1', p2p_port(node_idx), self.nodes[node_idx], node)
        node.add_connection(conn)
        return node

    def connect_incoming_peers(self, num):
        peers = []
        for _ in range(num):
            peer = self.create_testnode()
            peer.wait_for_verack()
            peers.append(peer)
        return peers

    def disconnect_incoming_peers(self, peers):
        for peer in peers:
            if not peer.disconnect():
                return False
        return True

    def any_received_getdata(self, hash, peers):
        for peer in peers:
            if hash in peer.tx_getdata_received:
                return True
        return False

    def all_received_getdata(self, hash, peers):
        for peer in peers:
            if not hash in peer.tx_getdata_received:
                return False
        return True

    def wait_for_getdata(self, hashes, peers):
        def getdata_received():
            for hash in hashes:
                if not self.all_received_getdata(hash, peers):
                    return False
            return True
        return wait_until(getdata_received, timeout=10)

    def wait_for_mocktime(self, node):
        def mocktime_is_good():
            return node.getmocktime() >= self.mocktime
        return wait_until(mocktime_is_good, timeout=10)

    def find_winning_peer(self, peers, hash):
        # detect which peer won a race for getting a getdata hash
        selected = None
        fallback = None
        for peer in peers:
            if hash in peer.tx_getdata_received:
                selected = peer
            else:
                fallback = peer

        assert selected is not None
        assert fallback is not None

        return selected, fallback

    def forward_mocktime(self, delta_time):
        self.mocktime += delta_time
        for node in self.nodes:
            node.setmocktime(self.mocktime)
            if not self.wait_for_mocktime(node):
                return False
        # sync the control peer with ping so that we're 100% sure we have
        # entered a new message handling loop
        self.control_peer.sync_with_ping()
        return True

    def forward_mocktime_step2(self, iterations):
        # forward mocktime in steps of 2 seconds to allow the nodes
        # time to recognize they have to do something
        for i in range(iterations):
            if not self.forward_mocktime(2):
                return False
        return True

    def next_fake_txid(self):
        self.fake_txid += 1
        return self.fake_txid

    def test_tx_request(self):
        txid = self.next_fake_txid()
        assert self.forward_mocktime(1)

        # use 4 peers
        peers = self.connect_incoming_peers(4)
        for peer in peers:
            peer.send_tx_inv([txid])

        # use 2 more peers that do not send invs
        otherpeers = self.connect_incoming_peers(2)

        # To make sure we eventually ask the tx from all 4 peers that announced
        # to us, we're now jumping 4 * (2 + 2 + 30) = 136 seconds to the future
        warp = 4 * MAX_GETDATA_INBOUND_WAIT
        self.forward_mocktime_step2(warp//2)

        # All peers that sent the inv should now have received a getdata request
        assert self.wait_for_getdata([txid], peers)

        # Make sure the other peers did not receive the getdata because they
        # didn't indicate they have the tx
        assert not self.any_received_getdata(txid, otherpeers)

        # cleanup
        assert self.disconnect_incoming_peers(peers)
        assert self.disconnect_incoming_peers(otherpeers)
        assert self.forward_mocktime(TX_EXPIRY_INTERVAL)

    def test_invblock_resolution(self):
        inputs = [self.nodes[1].listunspent()[0]]
        outputs = { self.tgtAddr: inputs[0]['amount'] - 1 }
        unsigned_tx = self.nodes[1].createrawtransaction(inputs, outputs)
        tx_hex = self.nodes[1].signrawtransaction(unsigned_tx)['hex']
        tx = FromHex(CTransaction(), tx_hex)
        tx.rehash()
        txid = int(tx.hash, 16)

        assert self.forward_mocktime(1)

        # make sure that node 1 is outbound for node 0
        assert self.nodes[0].getpeerinfo()[0]['inbound'] == False

        # use 8 peers that only inv but never respond to getdata
        peers = self.connect_incoming_peers(8)
        for peer in peers:
            peer.send_tx_inv([txid])

        # send from our honest node last
        self.nodes[1].sendrawtransaction(tx_hex)

        # We jump forward 2x max inbound wait time to make sure
        # that we get to the point where we re-evaluate the transaction in 2
        # second steps
        warp = 2 * MAX_GETDATA_INBOUND_WAIT
        assert self.forward_mocktime_step2(warp//2)

        assert tx.hash in self.nodes[0].getrawmempool()

        assert self.disconnect_incoming_peers(peers)
        assert self.forward_mocktime(TX_EXPIRY_INTERVAL)

    def test_inflight_throttling(self):
        # First, forward time by 2x inflight timeout, so that we have clean
        # registers for each peer
        self.forward_mocktime(2 * TX_EXPIRY_INTERVAL)

        # now send MAX_GETDATA_IN_FLIGHT (=100) invs with 1 peer
        peer = self.connect_incoming_peers(1)[0]
        invs = []
        for i in range(MAX_GETDATA_IN_FLIGHT):
            txid = self.next_fake_txid()
            invs.append(txid)

        peer.send_tx_inv(invs)

        # warp forward 3 seconds in steps of 1 second
        warp = INBOUND_PEER_TX_DELAY + 1
        for _ in range(warp):
            assert self.forward_mocktime(1)
            peer.sync_with_ping()

        # test that we got all the getdata
        assert peer.wait_until_numgetdata(MAX_GETDATA_IN_FLIGHT)

        peer.send_tx_inv([self.next_fake_txid()])

        # warp forward 3 seconds again
        warp = INBOUND_PEER_TX_DELAY + 1
        for _ in range(warp):
            assert self.forward_mocktime(1)
            peer.sync_with_ping()

        # test that we haven't received the getdata request yet
        assert len(peer.tx_getdata_received) == MAX_GETDATA_IN_FLIGHT

        # additionally warp the overloaded peer delay time second margin
        warp = OVERLOADED_PEER_DELAY
        for _ in range(warp):
            assert self.forward_mocktime(1)
            peer.sync_with_ping()

        # test that we now received the getdata
        assert peer.wait_until_numgetdata(MAX_GETDATA_IN_FLIGHT + 1)

        # cleanup
        assert self.disconnect_incoming_peers([peer])
        assert self.forward_mocktime(TX_EXPIRY_INTERVAL)

    def test_expiry_fallback(self):
        # create 2 new peers
        peers = self.connect_incoming_peers(2)

        txid = self.next_fake_txid()
        assert self.forward_mocktime(1)

        for peer in peers:
            peer.send_tx_inv([txid])

        # warp forward 2 + 2 (margin) = 4 seconds in steps of 2
        warp = INBOUND_PEER_TX_DELAY + 2
        assert self.forward_mocktime_step2(warp//2)

        winner, loser = self.find_winning_peer(peers, txid)

        # expire the request from the winning peer by doing nothing
        assert self.forward_mocktime_step2(MAX_GETDATA_INBOUND_WAIT//2)

        # the losing peer is now the fallback and received a getdata message
        assert self.wait_for_getdata([txid], [loser])

        #cleanup
        assert self.disconnect_incoming_peers(peers)
        assert self.forward_mocktime(TX_EXPIRY_INTERVAL)

    def test_notfound_fallback(self):
        # use 2 peers to concurrently send 2 invs
        peers = self.connect_incoming_peers(2)
        txid = self.next_fake_txid()
        assert self.forward_mocktime(1)

        for peer in peers:
            peer.send_tx_inv([txid])

        # warp forward 2 + 2 (margin) = 4 seconds in steps of 2
        warp = INBOUND_PEER_TX_DELAY + 2
        assert self.forward_mocktime_step2(warp//2)

        winner, loser = self.find_winning_peer(peers, txid)

        # send a reject message from the peer that won the race
        winner.send_tx_notfound([txid])

        # warp forward the max wait time in steps of 2
        assert self.forward_mocktime_step2(MAX_GETDATA_INBOUND_WAIT//2)

        # the losing peer is now the fallback and received a getdata message
        assert self.wait_for_getdata([txid], [loser])

        #cleanup
        assert self.disconnect_incoming_peers(peers)
        assert self.forward_mocktime(TX_EXPIRY_INTERVAL)

    def test_disconnect_fallback(self):
        # use 2 peers to concurrently send 2 invs
        peers = self.connect_incoming_peers(2)
        txid = self.next_fake_txid()
        assert self.forward_mocktime(1)

        for peer in peers:
            peer.send_tx_inv([txid])

        # warp forward 2 + 2 (margin) = 4 seconds in steps of 2
        warp = INBOUND_PEER_TX_DELAY + 2
        assert self.forward_mocktime_step2(warp//2)

        winner, loser = self.find_winning_peer(peers, txid)

        # drop connection from the peer that won the race
        assert winner.disconnect()

        # warp forward the max wait time in steps of 2
        assert self.forward_mocktime_step2(MAX_GETDATA_INBOUND_WAIT//2)

        # the losing peer is now the fallback and received a getdata message
        assert self.wait_for_getdata([txid], [loser])

        #cleanup
        assert self.disconnect_incoming_peers(peers)
        assert self.forward_mocktime(TX_EXPIRY_INTERVAL)

    def test_max_announcements(self):
        # create a test node
        peer = self.connect_incoming_peers(1)[0]

        assert self.forward_mocktime(1)

        hashes = []
        for _ in range(MAX_PEER_TX_ANNOUNCEMENTS):
            hashes.append(self.next_fake_txid())

        peer.send_tx_inv(hashes)

        # wait the maximum time before expiry minus 2 seconds to receive all
        # getdata requests with this peer
        warp = MAX_GETDATA_INBOUND_WAIT - 2
        assert self.forward_mocktime_step2(warp//2)
        assert peer.wait_until_numgetdata(MAX_PEER_TX_ANNOUNCEMENTS)
        peer.sync_with_ping()

        # send one more and wait the maximum time - this should never come back.
        extratx = self.next_fake_txid()
        peer.send_tx_inv([extratx])
        assert self.forward_mocktime_step2(MAX_GETDATA_INBOUND_WAIT//2)
        assert not self.any_received_getdata(extratx, [peer])

        #cleanup
        assert self.disconnect_incoming_peers([peer])
        assert self.forward_mocktime(TX_EXPIRY_INTERVAL)

if __name__ == '__main__':
    TxDownloadTest().main()
