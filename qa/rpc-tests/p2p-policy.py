#!/usr/bin/env python3
# Copyright (c) 2021 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
"""P2P Policies QA test

# Tests relay and mempool acceptance policies from p2p perspective
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
        self.txinvs = {}
        self.rejects = []

    def add_connection(self, conn):
        self.connection = conn

    # Track transaction invs for wait_for_tx_inv
    def on_inv(self, conn, message):
        for i in message.inv:
            if (i.type == 1):
                self.txinvs[format(i.hash, '064x')] = True

    # Track pongs for sync_with_ping
    def on_pong(self, conn, message):
        self.last_pong = message

    # Track reject messages
    def on_reject(self, conn, message):
        self.rejects.append(message)

    # wait for verack to make sure the node accepts our connection attempt
    def wait_for_verack(self):
        def veracked():
            return self.verack_received
        return wait_until(veracked, timeout=10)

    # Wait until we have received an inv of a specific tx
    def wait_for_tx_inv(self, hash, timeout=30):
        def have_received_tx_inv():
            try:
                return self.txinvs[hash]
            except KeyError as e:
                return False
        return wait_until(have_received_tx_inv, timeout=timeout)

    # Send a ping message and wait until we get the pong message back
    def sync_with_ping(self, timeout=30):
        def received_pong():
            return (self.last_pong.nonce == self.ping_counter)
        self.connection.send_message(msg_ping(nonce=self.ping_counter))
        success = wait_until(received_pong, timeout=timeout)
        self.ping_counter += 1
        return success

class P2PPolicyTests(BitcoinTestFramework):

    def __init__(self):
        super().__init__()
        self.setup_clean_chain = True
        self.num_nodes = 1
        self.utxo = []

        # a private key and corresponding address and p2pkh output script
        self.srcPrivKey = "cRhVU6TU1qHfRg3ee59yqg7ifhREKPLPPk8eccrrAEEY74bY1dCY"
        self.srcAddr = "mmMP9oKFdADezYzduwJFcLNmmi8JHUKdx9"
        self.srcOutScript = "76a91440015860f45d48eeeb2224dce3ad94ba91763e1e88ac"

        # valid regtest address that no one has the key to
        self.tgtAddr = "mkwDHkWXF8x6aFtdGVm5E9PVC7yPY8cb4r"

    def create_testnode(self, node_idx=0):
        node = TestNode()
        conn = NodeConn('127.0.0.1', p2p_port(node_idx), self.nodes[node_idx], node)
        node.add_connection(conn)
        return node

    def setup_network(self):
        self.nodes = []

        # a Dogecoin Core node that behaves similar to mainnet policies
        self.nodes.append(start_node(0, self.options.tmpdir, ["-debug", "-acceptnonstdtxn=0"]))

        # custom testnodes
        self.sendNode = self.create_testnode() # to send tx from
        self.recvNode = self.create_testnode() # to check relay from

        # start networking and handshake the mininodes
        NetworkThread().start()
        self.sendNode.wait_for_verack()
        self.recvNode.wait_for_verack()

    def run_test(self):
        self.nodes[0].generate(101)

        ### test constants ###
        koinu = Decimal("0.00000001")          # 1 Koinu expressed in DOGE
        ten = Decimal("10.0")                  # uniform 10 DOGE seed moneys

        ### parameters from fee policy ###
        relay_fee = Decimal("0.001")           # DEFAULT_MIN_RELAY_TX_FEE
        soft_dust_limit = Decimal("0.01")      # DEFAULT_DUST_LIMIT

        relay_fee_per_byte = relay_fee / 1000

        # create a bunch of UTXO with seed money from the Dogecoin Core wallet
        for i in range(10):
            inputs = [self.nodes[0].listunspent()[0]]
            outputs = { self.srcAddr : ten }
            tx = self.nodes[0].createrawtransaction(inputs, outputs)
            signed = self.nodes[0].signrawtransaction(tx)
            txid = self.nodes[0].sendrawtransaction(signed['hex'], True)
            self.utxo.append(txid)
        self.nodes[0].generate(1)

        # test legacy output of 1 DOGE output and 1 DOGE fee
        output = { self.tgtAddr : 1, self.srcAddr: 8 }
        self.run_relay_test(output, None)

        # test exact relay fee rate
        output = { self.tgtAddr: ten - relay_fee_per_byte * 192}
        tx = self.run_relay_test(output, None)

        # test too low relay fee rate
        output = { self.tgtAddr: ten - relay_fee_per_byte * 191 + koinu }
        tx = self.run_relay_test(output, 66) # 66 = too low fee

        # test exact dust limit
        change = ten - soft_dust_limit - relay_fee_per_byte * 226
        output = { self.tgtAddr : soft_dust_limit, self.srcAddr: change}
        self.run_relay_test(output, None)

        # test soft dust limit with sufficient fee
        amount = soft_dust_limit - koinu
        change = ten - amount - relay_fee_per_byte * 226 - soft_dust_limit
        output = { self.tgtAddr : amount, self.srcAddr: change }
        self.run_relay_test(output, None)

        # test soft dust limit with insufficient fee
        amount = soft_dust_limit - koinu
        change = ten - amount - relay_fee_per_byte * 225 - soft_dust_limit + koinu
        output = { self.tgtAddr : amount, self.srcAddr: change }
        self.run_relay_test(output, 66)

        # test a 1 koinu output with sufficient fee
        amount = koinu
        change = ten - amount - relay_fee_per_byte * 226 - soft_dust_limit
        output = { self.tgtAddr : amount, self.srcAddr: change }
        self.run_relay_test(output, 64) # 64 = dust

        # test a 1 koinu output with insufficient fee
        amount = koinu
        change = ten - amount - relay_fee_per_byte * 225 - soft_dust_limit + koinu
        output = { self.tgtAddr : amount, self.srcAddr: change }
        self.run_relay_test(output, 64)


    # test mempool acceptance and relay outcomes
    def run_relay_test(self, output, expected_reject_code):
        num_rejects = len(self.sendNode.rejects)

        tx = self.spend_utxo(output)
        self.sendNode.sync_with_ping(timeout=10)

        if (expected_reject_code is None):
            # test that the tx got relayed
            assert_equal(self.recvNode.wait_for_tx_inv(tx.hash), True)
            assert_equal(len(self.sendNode.rejects), num_rejects)
        else:
            # test that there was a rejection received with the correct code
            assert_greater_than(len(self.sendNode.rejects), num_rejects)
            assert_equal(self.sendNode.rejects[-1].code, expected_reject_code)

        return tx

    # spend seed money with a key not in the Dogecoin Core wallet.
    def spend_utxo(self, output):
        # construct the transaction using Dogecoin Core raw tx APIs
        input = [{ "txid": self.utxo.pop(), "vout": 0, "scriptPubKey": self.srcOutScript }]
        rawtx = self.nodes[0].createrawtransaction(input, output)
        signed_tx = self.nodes[0].signrawtransaction(rawtx, input, [self.srcPrivKey])

        # import the signed tx into a format the mininode client understands
        # and send the tx from there rather than from Dogecoin Core, to test
        # mempool acceptance as it would happen on mainnet: through relay
        tx = FromHex(CTransaction(), signed_tx['hex'])
        tx.rehash()
        self.sendNode.connection.send_message(msg_tx(tx))

        return tx

if __name__ == '__main__':
    P2PPolicyTests().main()
