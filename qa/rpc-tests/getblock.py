#!/usr/bin/env python3
# Copyright (c) 2023 The Dogecoin Core Developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

#
# Test getblock rpc call
#
from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import (
    assert_equal,
    assert_raises_jsonrpc,
    start_node
)
import json
import os
import logging

TESTSDIR = os.path.dirname(os.path.realpath(__file__))

class GetBlockTest(BitcoinTestFramework):

    start_height = 241
    max_stat_pos = 2

    def add_options(self, parser):
        parser.add_option('--gen-test-data', dest='gen_test_data',
                            default=False, action='store_true',
                            help='Generate test data')
        parser.add_option('--test-data', dest='test_data',
                            default='data/getblock.json',
                            action='store', metavar='FILE',
                            help='Test data file')

    def __init__(self):
        super().__init__()
        self.setup_clean_chain = True
        self.num_nodes = 1
        self.log = logging.getLogger("GetBlockTest")

    def setup_network(self, split=False):
        self.nodes = []
        self.nodes.append(start_node(0, self.options.tmpdir, []))
        self.is_network_split=False
        self.sync_all()

    def get_block_data(self, verbosity):
        block_data = [self.nodes[0].getblock(blockhash=self.nodes[0].getblockhash(self.start_height + i), verbosity=verbosity) for i in range(self.max_stat_pos+1)]
        # Convert Decimal class to string
        if verbosity >= 1:
            for data in block_data:
                data['difficulty'] = str(data['difficulty'])
        if verbosity >= 2:
            for data in block_data:
                for tx in data['tx']:
                    for vout in tx['vout']:
                        vout['value'] = str(vout['value'])
        return block_data

    def generate_test_data(self, filename):
        mocktime = 1525107225
        self.nodes[0].setmocktime(mocktime)
        self.nodes[0].generate(241)

        address = self.nodes[0].getnewaddress()
        self.nodes[0].sendtoaddress(address=address, amount=100000, subtractfeefromamount=True)
        self.nodes[0].generate(1)
        self.sync_all()

        self.nodes[0].sendtoaddress(address=address, amount=100000, subtractfeefromamount=True)
        self.nodes[0].sendtoaddress(address=address, amount=100000, subtractfeefromamount=False)
        self.nodes[0].settxfee(amount=0.003)
        self.nodes[0].sendtoaddress(address=address, amount=10000, subtractfeefromamount=True)
        self.sync_all()
        self.nodes[0].generate(1)

        self.expected_data_0 = self.get_block_data(0)
        self.expected_data_1 = self.get_block_data(1)
        self.expected_data_2 = self.get_block_data(2)

        blocks = []
        tip = self.nodes[0].getbestblockhash()
        blockhash = None
        height = 0
        while tip != blockhash:
            blockhash = self.nodes[0].getblockhash(height)
            blocks.append(self.nodes[0].getblock(blockhash, False))
            height += 1

        to_dump = {
            'blocks': blocks,
            'mocktime': int(mocktime),
            'data_0': self.expected_data_0,
            'data_1': self.expected_data_1,
            'data_2': self.expected_data_2,
        }
        with open(filename, 'w', encoding="utf8") as f:
            json.dump(to_dump, f, sort_keys=True, indent=2)

    def load_test_data(self, filename):
        with open(filename, 'r', encoding="utf8") as f:
            d = json.load(f)
            blocks = d['blocks']
            mocktime = d['mocktime']
            self.expected_data_0 = d['data_0']
            self.expected_data_1 = d['data_1']
            self.expected_data_2 = d['data_2']

        # Set the timestamps from the file so that the nodes can get out of Initial Block Download
        self.nodes[0].setmocktime(mocktime)
        self.sync_all()

        for b in blocks:
            self.nodes[0].submitblock(b)


    def run_test(self):
        test_data = os.path.join(TESTSDIR, self.options.test_data)
        if self.options.gen_test_data:
            self.generate_test_data(test_data)
        else:
            self.load_test_data(test_data)

        self.sync_all()

        data_0 = self.get_block_data(0)
        data_1 = self.get_block_data(1)
        data_2 = self.get_block_data(2)
        data_true = self.get_block_data(True)
        data_false = self.get_block_data(False)

        # Test verbosity 1 (true) and 2 heights
        assert_equal(data_1[0]['height'], self.start_height)
        assert_equal(data_1[self.max_stat_pos]['height'], self.start_height + self.max_stat_pos)
        assert_equal(data_2[0]['height'], self.start_height)
        assert_equal(data_2[self.max_stat_pos]['height'], self.start_height + self.max_stat_pos)
        assert_equal(data_true[0]['height'], self.start_height)
        assert_equal(data_true[self.max_stat_pos]['height'], self.start_height + self.max_stat_pos)

        for i in range(self.max_stat_pos+1):
            # Make sure all valid data is included but nothing else is (only for verbosity 1 and 2)
            expected_keys_1 = self.expected_data_1[i].keys()
            assert_equal(set(data_1[i].keys()), set(expected_keys_1))
            expected_keys_2 = self.expected_data_2[i].keys()
            assert_equal(set(data_2[i].keys()), set(expected_keys_2))

            self.log.info('Checking block %d\n' % (i))
            assert_equal(data_0[i], self.expected_data_0[i])
            assert_equal(data_1[i], self.expected_data_1[i])
            assert_equal(data_2[i], self.expected_data_2[i])

            # Test backward-compatibility (0=false, 1=true)
            assert_equal(data_0[i], data_false[i])
            assert_equal(data_1[i], data_true[i])

        # Test invalid parameters
        assert_raises_jsonrpc(-5, 'Block not found', self.nodes[0].getblock,
                                blockhash='0')
        assert_raises_jsonrpc(-5, 'Block not found', self.nodes[0].getblock,
                                blockhash='-1')

        # Mainchain's genesis block shouldn't be found on regtest
        assert_raises_jsonrpc(-5, 'Block not found', self.nodes[0].getblock,
                                blockhash='1a91e3dace36e2be3bf030a65679fe821aa1d6ef92e7c9902eb318182c355691')

        # Invalid number of args
        assert_raises_jsonrpc(-1, 'getblock "blockhash" ( verbosity )', self.nodes[0].getblock)


if __name__ == '__main__':
    GetBlockTest().main()
