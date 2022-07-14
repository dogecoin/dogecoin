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
TXID_RELAY_DELAY = 2 # seconds
MAX_GETDATA_IN_FLIGHT = 100

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
        self.test_max_inflight()
        self.test_disconnect_fallback()
        self.test_notfound_fallback()

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

        # set up incoming (non-honest) peers
        self.incoming_peers = []
        for i in range(8):
            self.incoming_peers.append(self.create_testnode())

        NetworkThread().start()
        for peer in self.incoming_peers:
            peer.wait_for_verack()

    def create_testnode(self, node_idx=0):
        node = TxDownloadTestNode()
        conn = NodeConn('127.0.0.1', p2p_port(node_idx), self.nodes[node_idx], node)
        node.add_connection(conn)
        return node

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
        # give the nodes some time to process the new mocktime
        # can be removed when we have getmocktime
        time.sleep(0.1)

    def forward_mocktime_step2(self, iterations):
        # forward mocktime in steps of 2 seconds to allow the nodes
        # time to recognize they have to do something
        for i in range(iterations):
            self.forward_mocktime(2)

    def next_fake_txid(self):
        self.fake_txid += 1
        return self.fake_txid

    def test_tx_request(self):
        txid = self.next_fake_txid()
        self.forward_mocktime(0)

        # use incoming peers 0 and 1
        peerset = self.incoming_peers[0:4]
        for peer in peerset:
            peer.send_tx_inv([txid])

        # To make sure we eventually ask the tx from all 4 nodes that announced
        # to us, we're now jumping 4 * (2 + 2 + 30) = 136 seconds to the future
        warp = 4 * (INBOUND_PEER_TX_DELAY + TXID_RELAY_DELAY + GETDATA_TX_INTERVAL)
        self.forward_mocktime_step2(warp//2)

        # All peers that sent the inv should now have received a getdata request
        assert self.wait_for_getdata([txid], peerset)

        # Make sure the other peers did not receive the getdata because they
        # didn't indicate they have the tx
        assert not self.any_received_getdata(txid, self.incoming_peers[4:8])

    def test_invblock_resolution(self):
        inputs = [self.nodes[1].listunspent()[0]]
        outputs = { self.tgtAddr: inputs[0]['amount'] - 1 }
        unsigned_tx = self.nodes[1].createrawtransaction(inputs, outputs)
        tx_hex = self.nodes[1].signrawtransaction(unsigned_tx)['hex']
        tx = FromHex(CTransaction(), tx_hex)
        tx.rehash()
        txid = int(tx.hash, 16)

        self.forward_mocktime(0)

        # make sure that node 1 is outbound for node 0
        assert self.nodes[0].getpeerinfo()[0]['inbound'] == False

        # use all peers that only inv but never respond to getdata
        for peer in self.incoming_peers:
            peer.send_tx_inv([txid])

        # send from our honest node last
        self.nodes[1].sendrawtransaction(tx_hex)

        # We jump forward 2x (2 + 2) + 30 + 2 (margin) = 40 seconds to make sure
        # that we get to the point where we re-evaluate the transaction in 2
        # second steps
        warp = 2 * (INBOUND_PEER_TX_DELAY + TXID_RELAY_DELAY) + GETDATA_TX_INTERVAL + 2
        self.forward_mocktime_step2(warp//2)

        assert tx.hash in self.nodes[0].getrawmempool()

    def test_max_inflight(self):
        # First, forward time by 2x inflight timeout, so that we have clean
        # registers for each peer
        self.forward_mocktime(2 * TX_EXPIRY_INTERVAL)

        # now send MAX_GETDATA_IN_FLIGHT (=100) invs with peer 0
        peer = self.incoming_peers[0]
        invd = []
        for i in range(MAX_GETDATA_IN_FLIGHT):
            txid = self.next_fake_txid()
            peer.send_tx_inv([txid])
            invd.append(txid)

        # warp forward 2 + 2 + 2 (margin) = 6 seconds in steps of 2
        warp = INBOUND_PEER_TX_DELAY + TXID_RELAY_DELAY + 2
        self.forward_mocktime_step2(warp//2)

        # test that we got all the getdatas
        assert self.wait_for_getdata(invd, [peer])

        # send one more inv with our now maxed out peer
        txid_failed = self.next_fake_txid()
        peer.send_tx_inv([txid_failed])
        # and send one inv with another peer
        txid_success = self.next_fake_txid()
        self.incoming_peers[1].send_tx_inv([txid_success])

        # warp forward 2 + 2 + 2 (margin) = 6 seconds in steps of 2
        warp = INBOUND_PEER_TX_DELAY + TXID_RELAY_DELAY + 2
        self.forward_mocktime_step2(warp//2)

        # test that we got a getdata for the successful tx with peer 1
        assert self.wait_for_getdata([txid_success], [self.incoming_peers[1]])
        # test that we did not get a getdata for the failed txid with peer 0
        assert not self.any_received_getdata(txid_failed, [peer])

        # clear out the inflight register by expiring all requests
        self.forward_mocktime(TX_EXPIRY_INTERVAL)

        # send one inv with 4 txs
        txids = []
        for i in range(4):
            txids.append(self.next_fake_txid())
        peer.send_tx_inv(txids)

        # warp forward 2 + 2 + 2 (margin) = 6 seconds in steps of 2
        warp = INBOUND_PEER_TX_DELAY + TXID_RELAY_DELAY + 2
        self.forward_mocktime_step2(warp//2)

        # test that we got a getdata for the final inv with peer 0
        assert self.wait_for_getdata(txids, [peer])

    def test_notfound_fallback(self):
        # use peer 4 and 5 to concurrently send 2 invs
        peers = self.incoming_peers[4:6]
        txid = self.next_fake_txid()
        self.forward_mocktime(1)

        for peer in peers:
            peer.send_tx_inv([txid])

        # warp forward 2 + 2 + 2 (margin) = 6 seconds in steps of 2
        warp = INBOUND_PEER_TX_DELAY + TXID_RELAY_DELAY + 2
        self.forward_mocktime_step2(warp//2)

        winner, loser = self.find_winning_peer(peers, txid)

        # send a reject message from the peer that won the race
        winner.send_tx_notfound([txid])

        # warp forward 30 + 2 + 2 + 2 (margin) = 36 seconds in steps of 2
        warp = GETDATA_TX_INTERVAL + INBOUND_PEER_TX_DELAY + TXID_RELAY_DELAY + 2
        self.forward_mocktime_step2(warp//2)

        # the losing peer is now the fallback and received a getdata message
        assert self.wait_for_getdata([txid], [loser])

    def test_disconnect_fallback(self):
        # use peer 6 and 7 to concurrently send 2 invs
        peers = self.incoming_peers[6:8]
        txid = self.next_fake_txid()
        self.forward_mocktime(1)

        for peer in peers:
            peer.send_tx_inv([txid])

        # warp forward 2 + 2 + 2 (margin) = 6 seconds in steps of 2
        warp = INBOUND_PEER_TX_DELAY + TXID_RELAY_DELAY + 2
        self.forward_mocktime_step2(warp//2)

        winner, loser = self.find_winning_peer(peers, txid)

        # drop connection from the peer that won the race
        assert winner.disconnect()

        # warp forward 30 + 2 + 2 + 2 (margin) = 36 seconds in steps of 2
        warp = GETDATA_TX_INTERVAL + INBOUND_PEER_TX_DELAY + TXID_RELAY_DELAY + 2
        self.forward_mocktime_step2(warp//2)

        # the losing peer is now the fallback and received a getdata message
        assert self.wait_for_getdata([txid], [loser])

if __name__ == '__main__':
    TxDownloadTest().main()
