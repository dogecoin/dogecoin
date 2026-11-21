#!/usr/bin/env python3
# Copyright (c) 2014-2016 The Bitcoin Core developers
# Copyright (c) 2022-2024 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

#
# Test RPC calls related to scanning of utxo stored in db. 
# Tests correspond to code in rpc/blockchain.cpp.
#
from random import (seed, randrange)
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
        self.num_nodes = 3

    def setup_network(self, split=False):
        self.nodes = []
        self.nodes.append(start_node(0, self.options.tmpdir, ["-prune=1"]))
        self.nodes.append(start_node(1, self.options.tmpdir, ["-prune=2200"]))
        self.nodes.append(start_node(2, self.options.tmpdir))
        connect_nodes_bi(self.nodes, 0, 1)
        connect_nodes_bi(self.nodes, 1, 2)
        connect_nodes_bi(self.nodes, 0, 2)
                      
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

        # specify number of random test amounts to generate
        data_size = 10

        amounts = [None]*data_size
        seed(None)
        for i in range(data_size): 
            amounts[i] = randrange(1, 5555)
    
        # Arrays to store RPC response-related data
        txids = [None]*data_size
        res_rpcs = [None]*data_size
        heights = [None]*data_size

        address = self.nodes[0].getnewaddress()
        privkey = self.nodes[0].dumpprivkey(address)        
        
        # test simple RPC call without utxo which will
        # throw exception
        try: 
            res = self.nodes[0].getutxoforkey(privkey)
        except JSONRPCException as e:
            assert("Unable to find utxo amount")      
        
        # Load addresses and privkeys arrays
        # Values are stored in reverse order so amounts could later be sent to an address
        # created on a different node  
        addresses = [None]*data_size
        privkeys = [None]*data_size
        n = data_size - 1    
        for i in range(data_size):
            # Alternate nodes when getting new address to get a better mix of
            # addresses generated from each node 
            addresses[n - i] = self.nodes[i%self.num_nodes].getnewaddress()
            privkeys[n - i] = self.nodes[i%self.num_nodes].dumpprivkey(addresses[n - i])
    
        enable_mocktime()
        block_time = get_mocktime() - (121 * 60)
        for i in range(data_size):            
            # Try to send to an address on a different node 
            txids[i] = self.nodes[i%self.num_nodes].sendtoaddress(addresses[i], amounts[i])
            
            set_node_times(self.nodes, block_time)
            # node[i%self.num_nodes] mines a block per the timing algorithm in test_framework/util.py          
            self.nodes[i%self.num_nodes].generate(1)
            heights[i] = 121 + i
            block_time += 60
            self.sync_all()

        for i in range(data_size):
            # Call getutxoforkey RPC with stored privkeys[i] on node[i%self.num_nodes] 
            # and store RPC response in res_rpcs array   
            try: 
                res_rpcs[i] = self.nodes[i%self.num_nodes].getutxoforkey(privkeys[i])
            except JSONRPCException as e:
                assert("Unable to find utxo amount")            
        
        # If utxo amount exists, verify that txid, utxo amount and block height for tx 
        # obtained during iteration for each node is the same as in RPC response for 
        # getutxoforkey from each call.                
        for i in range(data_size):
            if (res_rpcs[i] != None):
                assert_equal(Decimal(res_rpcs[i]["amount"]), Decimal(amounts[i]))
                assert_equal(res_rpcs[i]["height"], heights[i])
                assert_equal(res_rpcs[i]["txid"], txids[i])

if __name__ == '__main__':
    UtxoScanTest().main()
