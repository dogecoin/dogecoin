#!/usr/bin/env python3
# Copyright (c) 2014-2016 The Bitcoin Core developers
# Copyright (c) 2022 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

#
# Test RPC calls related to blockchain state. Tests correspond to code in
# rpc/blockchain.cpp.
#

from decimal import Decimal

from test_framework.test_framework import BitcoinTestFramework
from test_framework.authproxy import JSONRPCException
from test_framework.util import (
    assert_equal,
    assert_greater_than,
    assert_greater_than_or_equal,
    assert_raises,
    assert_raises_jsonrpc,
    assert_is_hex_string,
    assert_is_hash_string,
    find_output,
    start_nodes,
    start_node,
    connect_nodes_bi,
)


class BlockchainTest(BitcoinTestFramework):
    """
    Test blockchain-related RPC calls:

        - gettxoutsetinfo
        - gettxout
        - verifychain

    """

    # rpc/protocol.h
    RPC_MISC_ERROR = -1

    def __init__(self):
        super().__init__()
        self.setup_clean_chain = False
        self.num_nodes = 2

    def setup_network(self, split=False):
        self.nodes = []
        self.nodes.append(start_node(0, self.options.tmpdir, ["-prune=1"]))
        self.nodes.append(start_node(1, self.options.tmpdir, ["-prune=2200"]))
        connect_nodes_bi(self.nodes, 0, 1)
        self.is_network_split = False
        self.sync_all()

    def run_test(self):
        self._test_gettxoutsetinfo()
        self._test_getblockheader()
        self._test_getblockchaininfo()
        self._test_verifychain_args()
        self.nodes[0].verifychain(4, 0)
        # last, because it spends and mines and so moves the chain past the
        # heights and output counts the tests above pin down
        self._test_gettxout()

    # PL backported this entire test from upstream 0.16 to 1.14.3
    def _test_getblockchaininfo(self):

        keys = [
            'bestblockhash',
            'bip9_softforks',
            'blocks',
            'chain',
            'chainwork',
            'difficulty',
            'headers',
            'initialblockdownload',
            'mediantime',
            'pruned',
            'size_on_disk',
            'softforks',
            'verificationprogress',
            'warnings',
        ]
        res = self.nodes[0].getblockchaininfo()

        # result should have these additional pruning keys if manual pruning is enabled
        assert_equal(sorted(res.keys()), sorted(['pruneheight', 'automatic_pruning'] + keys))

        # size_on_disk should be > 0
        assert_greater_than(res['size_on_disk'], 0)

        # pruneheight should be greater or equal to 0
        assert_greater_than_or_equal(res['pruneheight'], 0)

        # check other pruning fields given that prune=1
        assert res['pruned']
        assert not res['automatic_pruning']

        res = self.nodes[1].getblockchaininfo()
        # result should have these additional pruning keys if prune=2200
        assert_equal(sorted(res.keys()), sorted(['pruneheight', 'automatic_pruning', 'prune_target_size'] + keys))

        # check related fields
        assert res['pruned']
        assert_equal(res['pruneheight'], 0)
        assert res['automatic_pruning']
        assert_equal(res['prune_target_size'], 2306867200)
        assert_greater_than(res['size_on_disk'], 0)

    def _test_gettxoutsetinfo(self):
        node = self.nodes[0]
        res = node.gettxoutsetinfo()

        assert_equal(res['total_amount'], Decimal('60000000.00000000'))
        assert_equal(res['transactions'], 120)
        assert_equal(res['height'], 120)
        assert_equal(res['txouts'], 120)
        assert_equal(res['bytes_serialized'], 8520),
        assert_equal(len(res['bestblock']), 64)
        assert_equal(len(res['hash_serialized']), 64)

    def _test_gettxout(self):
        # everything here is asked of node 0 alone: both nodes in this test are
        # pruned, so neither offers NODE_NETWORK and they cannot serve blocks to
        # one another, which makes syncing them impossible as well as pointless
        node = self.nodes[0]

        # pay ourselves, so that there is an output in the set whose value and
        # script are known, and confirm it
        address = node.getnewaddress()
        amount = Decimal("1000.00000000")
        txid = node.sendtoaddress(address, amount)
        node.generate(1)
        n = find_output(node, txid, amount)

        out = node.gettxout(txid, n)
        assert_equal(sorted(out.keys()),
                     sorted(['bestblock', 'confirmations', 'value',
                             'scriptPubKey', 'version', 'coinbase']))
        assert_equal(out['bestblock'], node.getbestblockhash())
        assert_equal(out['confirmations'], 1)
        assert_equal(out['value'], amount)
        assert_equal(out['coinbase'], False)

        script = out['scriptPubKey']
        assert_equal(sorted(script.keys()),
                     sorted(['asm', 'hex', 'reqSigs', 'type', 'addresses']))
        assert_equal(script['type'], 'pubkeyhash')
        assert_equal(script['reqSigs'], 1)
        assert_equal(script['addresses'], [address])
        assert_is_hex_string(script['hex'])

        # the confirmation count and the block reported follow the chain as it grows
        node.generate(2)
        out = node.gettxout(txid, n)
        assert_equal(out['confirmations'], 3)
        assert_equal(out['bestblock'], node.getbestblockhash())

        # a coinbase output is reported as one
        coinbase_txid = node.getblock(node.getblockhash(1))['tx'][0]
        assert_equal(node.gettxout(coinbase_txid, 0)['coinbase'], True)

        # an output that is not in the set answers with null rather than failing,
        # whether the transaction is unknown or the vout is out of range
        assert_equal(node.gettxout("00" * 32, 0), None)
        assert_equal(node.gettxout(txid, 1000), None)
        assert_equal(node.gettxout(txid, -1), None)

        # an unconfirmed spend is reflected only in the view that includes the
        # mempool: there the spent output is gone and the new one is already
        # visible with no confirmations, while the set on disk still has neither
        raw = node.createrawtransaction([{"txid": txid, "vout": n}],
                                        {node.getnewaddress(): amount - Decimal("1.00000000")})
        signed = node.signrawtransaction(raw)
        assert_equal(signed['complete'], True)
        spend_txid = node.sendrawtransaction(signed['hex'])

        assert_equal(node.gettxout(txid, n, True), None)
        assert_equal(node.gettxout(txid, n, False)['value'], amount)

        spent_to = node.gettxout(spend_txid, 0, True)
        assert_equal(spent_to['confirmations'], 0)
        assert_equal(spent_to['value'], amount - Decimal("1.00000000"))
        assert_equal(node.gettxout(spend_txid, 0, False), None)

        # include_mempool defaults to true
        assert_equal(node.gettxout(txid, n), None)

        # and once it is mined the set on disk agrees with what the mempool view
        # was already reporting
        node.generate(1)
        assert_equal(node.gettxout(txid, n, False), None)
        assert_equal(node.gettxout(spend_txid, 0, False)['confirmations'], 1)

        # txid and vout are both required, and there is no fourth argument
        assert_raises_jsonrpc(self.RPC_MISC_ERROR, "gettxout", node.gettxout, txid)
        assert_raises_jsonrpc(self.RPC_MISC_ERROR, "gettxout", node.gettxout, txid, n, True, 1)

    def _test_getblockheader(self):
        node = self.nodes[0]

        assert_raises(
            JSONRPCException, lambda: node.getblockheader('nonsense'))

        besthash = node.getbestblockhash()
        secondbesthash = node.getblockhash(119)
        header = node.getblockheader(besthash)

        assert_equal(header['hash'], besthash)
        assert_equal(header['height'], 120)
        assert_equal(header['confirmations'], 1)
        assert_equal(header['previousblockhash'], secondbesthash)
        assert_is_hex_string(header['chainwork'])
        assert_is_hash_string(header['hash'])
        assert_is_hash_string(header['previousblockhash'])
        assert_is_hash_string(header['merkleroot'])
        assert_is_hash_string(header['bits'], length=None)
        assert isinstance(header['time'], int)
        assert isinstance(header['mediantime'], int)
        assert isinstance(header['nonce'], int)
        assert isinstance(header['version'], int)
        assert isinstance(int(header['versionHex'], 16), int)
        assert isinstance(header['difficulty'], Decimal)

    def _test_verifychain_args(self):
        node = self.nodes[0]

        try:
            node.verifychain(-1)
            raise AssertionError("Must check for validity of checklevel parameter")
        except JSONRPCException as e:
            assert("Error: checklevel must be >= 0 and <= 4" in e.error["message"])

        try:
            node.verifychain(5)
            raise AssertionError("Must check for validity of checklevel parameter")
        except JSONRPCException as e:
            assert("Error: checklevel must be >= 0 and <= 4" in e.error["message"])

        try:
            node.verifychain(0, -100)
            raise AssertionError("Must check for validity of nblocks parameter")
        except JSONRPCException as e:
            assert("Error: nblocks must be >= 0" in e.error["message"])

        try:
            node.verifychain(0, -1000)
            raise AssertionError("Must check for validity of nblocks parameter")
        except JSONRPCException as e:
            assert("Error: nblocks must be >= 0" in e.error["message"])

if __name__ == '__main__':
    BlockchainTest().main()
