#!/usr/bin/env python3
# Copyright (c) 2014-2016 The Bitcoin Core developers
# Copyright (c) 2022-2024 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

#
# Test RPC calls related to scanning of utxo stored in db. 
# Tests correspond to code in rpc/blockchain.cpp.
#

from decimal import Decimal

from test_framework.test_framework import BitcoinTestFramework
from test_framework.authproxy import JSONRPCException
from test_framework.util import (
    assert_equal,
    start_node,
    connect_nodes_bi,
    enable_mocktime,
    get_mocktime,
    set_node_times
)

class UtxoScanTest(BitcoinTestFramework):
    """
    
        Test utxo-related RPC call: 
        - getutxoforkey
    
    """

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
        
        self._test_getutxoforkey()
        
    def _test_getutxoforkey(self):
        
        # check for invalid private key input
        try:
            self.nodes[0].getutxoforkey("")
        except JSONRPCException as e:
            assert("Invalid private key encoding" in e.error["message"])

        address = self.nodes[0].getnewaddress()
        privkey = self.nodes[0].dumpprivkey(address)

        amount = 42.001
        txid = self.nodes[1].sendtoaddress(address, amount)
        
        # At this point nodes are not yet synchronized and calling 
        # getutxoforkey with private key on node0 will throw exception.

        try: 
            res = self.nodes[0].getutxoforkey(privkey)
        except JSONRPCException as e:
            assert("Unable to find utxo amount")      
        
        # Node1 mines a block per the timing algorithm in test_framework/util.py 
        enable_mocktime()
        block_time = get_mocktime() - (121 * 60)
        set_node_times(self.nodes, block_time)        
        self.nodes[1].generate(1)

        # call this RPC on node1
        res_node1 = self.nodes[1].getutxoforkey(privkey)

        # verify that txid, utxo amount and new block height for tx 
        # on Node1 with 42.001 Doges is the same as in this RPC response        
        assert_equal(res_node1['amount'], Decimal("42.001"))
        assert_equal(res_node1['height'], 121)
        assert_equal(res_node1['txid'], txid)

        self.sync_all()

        # call this RPC on node0
        res_node0 = self.nodes[0].getutxoforkey(privkey)
       
        # After nodes have been syncronized, verify that txid, utxo amount and
        # new block height for tx on node0 with 42.001 Doges is the same 
        # as in RPC response on node0       
        assert_equal(res_node0['amount'], Decimal("42.001"))
        assert_equal(res_node0['height'], 121)
        assert_equal(res_node0['txid'], txid)

if __name__ == '__main__':
    UtxoScanTest().main()
