#!/usr/bin/env python3
# Copyright (c) 2014-2016 The Bitcoin Core developers
# Copyright (c) 2022 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import *


class RescanTest(BitcoinTestFramework):

    def __init__(self):
        super().__init__()
        self.setup_clean_chain = True
        self.num_nodes = 3

    # only sync the first two nodes; use the third for prune testing
    def sync_all(self):
        syncable = self.nodes[:2]
        sync_blocks(syncable)
        sync_mempools(syncable)

    def setup_network(self, split=False):
        self.nodes = start_nodes(self.num_nodes - 1, self.options.tmpdir, [['-spendzeroconfchange=0'], None])
        self.nodes.append(start_node(2, self.options.tmpdir, ["-prune=1"]))
        connect_nodes_bi(self.nodes,0,1)
        self.is_network_split=False
        self.sync_all()

    def run_test(self):
        print("Mining blocks...")
        self.nodes[0].generate(101)

        self.sync_all()

        # address
        address1 = self.nodes[0].getnewaddress()
        # pubkey
        address2 = self.nodes[0].getnewaddress()
        address2_pubkey = self.nodes[0].validateaddress(address2)['pubkey']                 # Using pubkey
        # privkey
        address3 = self.nodes[0].getnewaddress()
        address3_privkey = self.nodes[0].dumpprivkey(address3)                              # Using privkey

        self.sync_all()

        # Node 1 sync test
        assert_equal(self.nodes[1].getblockcount(), 101)

        # Send funds to self
        txnid1 = self.nodes[0].sendtoaddress(address1, 10)
        rawtxn1 = self.nodes[0].gettransaction(txnid1)['hex']

        txnid2 = self.nodes[0].sendtoaddress(address2, 5)
        rawtxn2 = self.nodes[0].gettransaction(txnid2)['hex']

        txnid3 = self.nodes[0].sendtoaddress(address3, 2.5)
        rawtxn3 = self.nodes[0].gettransaction(txnid3)['hex']

        self.nodes[0].generate(1)

        # Import with affiliated address with no rescan
        self.nodes[1].importaddress(address2, "add2", False)
        balance2 = self.nodes[1].getbalance("add2", 0, True)
        assert_equal(balance2, Decimal('0'))

        self.nodes[1].rescan()
        balance2 = self.nodes[1].getbalance("add2", 0, True)
        assert_equal(balance2, Decimal('5'))

        # Import with private key with no rescan
        self.nodes[1].importprivkey(address3_privkey, "add3", False)

        # add more blocks
        self.nodes[1].generate(102)
        balance4 = self.nodes[1].getbalance("add3", 0, False)
        assert_equal(balance4, Decimal('0'))
        self.nodes[1].rescan(200)
        balance4 = self.nodes[1].getbalance("add3", 0, False)
        assert_equal(balance4, Decimal('0'))
        result = self.nodes[1].rescan(2)
        balance4 = self.nodes[1].getbalance("add3", 0, True)
        assert_equal(balance4, Decimal('2.5'))

        assert_equal(result["before"], {
            "balance": Decimal('21000000'),
            "txcount": 103
        })

        assert_equal(result["after"], {
            "balance": Decimal('21000002.5'),
            "txcount": 104
        })

        assert_equal(result["blocks_scanned"], 202)
        assert("time_elapsed" in result)

        # 2100000 from mining, 7.5 otherwise
        # note that 5 are from a watch-only address
        balance4 = self.nodes[1].getbalance("*", 0, True)
        assert_equal(balance4, Decimal('21000007.5'))

        try:
            self.nodes[1].rescan(-100)
            raise AssertionError("rescan should throw JSON exception given a negative block height")
        except JSONRPCException as e:
            assert("Block height out of range" in e.error["message"])

        try:
            currentheight = self.nodes[1].getblockcount()
            self.nodes[1].rescan(currentheight + 1 )
            raise AssertionError("rescan should throw JSON exception given a block height too high")
        except JSONRPCException as e:
            assert("Block height out of range" in e.error["message"])

        try:
            pruned_node = self.nodes[2]
            pruned_node.rescan(1)
            raise AssertionError("rescan should throw JSON error when used on a pruned node")
        except JSONRPCException as e:
            assert("Currently works only on non-pruned nodes" in e.error["message"])



if __name__ == '__main__':
    RescanTest().main()
