#!/usr/bin/env python3
# Copyright (c) 2017-2019 The Bitcoin Core developers
# Copyright (c) 2023 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

#
# Test getblockstats rpc call
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

class GetblockstatsTest(BitcoinTestFramework):

    start_height = 241
    max_stat_pos = 2

    def add_options(self, parser):
        parser.add_option('--gen-test-data', dest='gen_test_data',
                            default=False, action='store_true',
                            help='Generate test data')
        parser.add_option('--test-data', dest='test_data',
                            default='data/getblockstats.json',
                            action='store', metavar='FILE',
                            help='Test data file')

    def __init__(self):
        super().__init__()
        self.setup_clean_chain = True
        self.num_nodes = 1
        self.log = logging.getLogger("GetblockstatsTest")

    def setup_network(self, split=False):
        self.nodes = []
        self.nodes.append(start_node(0, self.options.tmpdir, []))
        self.is_network_split=False
        self.sync_all()

    def get_stats(self):
        return [self.get_stats_for_height(self.start_height + i) for i in range(self.max_stat_pos+1)]

    def get_stats_for_height(self, height, stats=None):
        blockhash = self.nodes[0].getblockhash(height)
        if stats == None:
            return self.nodes[0].getblockstats(hash=blockhash)
        return self.nodes[0].getblockstats(hash=blockhash, stats=stats)

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

        self.expected_stats = self.get_stats()

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
            'stats': self.expected_stats,
        }
        with open(filename, 'w', encoding="utf8") as f:
            json.dump(to_dump, f, sort_keys=True, indent=2)

    def load_test_data(self, filename):
        with open(filename, 'r', encoding="utf8") as f:
            d = json.load(f)
            blocks = d['blocks']
            mocktime = d['mocktime']
            self.expected_stats = d['stats']

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
        stats = self.get_stats()

        # Make sure all valid statistics are included but nothing else is
        expected_keys = self.expected_stats[0].keys()
        assert_equal(set(stats[0].keys()), set(expected_keys))

        assert_equal(stats[0]['height'], self.start_height)
        assert_equal(stats[self.max_stat_pos]['height'], self.start_height + self.max_stat_pos)

        for i in range(self.max_stat_pos+1):
            self.log.info('Checking block %d\n' % (i))
            assert_equal(stats[i], self.expected_stats[i])

            # Check selecting block by hash too
            blockhash = self.expected_stats[i]['blockhash']
            stats_by_hash = self.nodes[0].getblockstats(hash=blockhash)
            assert_equal(stats_by_hash, self.expected_stats[i])

        # Make sure each stat can be queried on its own
        for stat in expected_keys:
            for i in range(self.max_stat_pos+1):
                result = self.get_stats_for_height(self.start_height + i, [stat])
                assert_equal(list(result.keys()), [stat])
                if result[stat] != self.expected_stats[i][stat]:
                    self.log.info('result[%s] (%d) failed, %r != %r' % (
                        stat, i, result[stat], self.expected_stats[i][stat]))
                assert_equal(result[stat], self.expected_stats[i][stat])

        # Make sure only the selected statistics are included (more than one)
        some_stats = {'minfee', 'maxfee'}
        stats = self.get_stats_for_height(1, list(some_stats))
        assert_equal(set(stats.keys()), some_stats)

        blockhashone = self.nodes[0].getblockhash(1)

        # Make sure not valid stats aren't allowed
        inv_sel_stat = 'asdfghjkl'
        inv_stats = [
            [inv_sel_stat],
            ['minfee' , inv_sel_stat],
            [inv_sel_stat, 'minfee'],
            ['minfee', inv_sel_stat, 'maxfee'],
        ]
        for inv_stat in inv_stats:
            assert_raises_jsonrpc(-8, 'Invalid selected statistic %s' % inv_sel_stat,
                                    self.nodes[0].getblockstats, hash=blockhashone, stats=inv_stat)

        # Make sure we aren't always returning inv_sel_stat as the culprit stat
        assert_raises_jsonrpc(-8, 'Invalid selected statistic aaa%s' % inv_sel_stat,
                                self.nodes[0].getblockstats, hash=blockhashone, stats=['minfee' , 'aaa%s' % inv_sel_stat])
        # Mainchain's genesis block shouldn't be found on regtest
        assert_raises_jsonrpc(-5, 'Block not found', self.nodes[0].getblockstats,
                                hash='1a91e3dace36e2be3bf030a65679fe821aa1d6ef92e7c9902eb318182c355691')

        # Invalid number of args
        assert_raises_jsonrpc(-1, 'getblockstats hash ( stats )', self.nodes[0].getblockstats)

        # Cannot pass a height
        assert_raises_jsonrpc(-8, 'hash must be hexadecimal string', self.nodes[0].getblockstats, hash=1)


if __name__ == '__main__':
    GetblockstatsTest().main()
