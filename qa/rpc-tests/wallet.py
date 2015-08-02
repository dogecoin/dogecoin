#!/usr/bin/env python2
# Copyright (c) 2014 The Bitcoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

#
# Exercise the wallet.  Ported from wallet.sh.  
# Does the following:
#   a) creates 3 nodes, with an empty chain (no blocks).
#   b) node0 mines a block
#   c) node1 mines 61 blocks, so now nodes 0 and 1 have 500.000 doge, node2 has none. 
#   d) node0 sends 210.000 doge to node2, in two transactions (110.000 doge, then 100.000 doge).
#   e) node0 mines a block, collects the fee on the second transaction
#   f) node1 mines 30 blocks, to mature node0's just-mined block
#   g) check that node0 has 1.000.000-210.000, node2 has 210.000
#   h) node0 should now have 2 unspent outputs;  send these to node2 via raw tx broadcast by node1
#   i) have node1 mine a block
#   j) check balances - node0 should have 0, node2 should have 1.000.000
#   k) test ResendWalletTransactions - create transactions, startup fourth node, make sure it syncs
#

from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import *

class WalletTest (BitcoinTestFramework):

    def setup_chain(self):
        print("Initializing test directory "+self.options.tmpdir)
        initialize_chain_clean(self.options.tmpdir, 4)

    def setup_network(self, split=False):
        self.nodes = start_nodes(3, self.options.tmpdir)
        connect_nodes_bi(self.nodes,0,1)
        connect_nodes_bi(self.nodes,1,2)
        connect_nodes_bi(self.nodes,0,2)
        self.is_network_split=False
        self.sync_all()

    def run_test (self):
        print "Mining blocks..."

        self.nodes[0].generate(1)

        walletinfo = self.nodes[0].getwalletinfo()
        assert_equal(walletinfo['immature_balance'], 500000)
        assert_equal(walletinfo['balance'], 0)

        self.sync_all()
        self.nodes[1].generate(61)
        self.sync_all()

        assert_equal(self.nodes[0].getbalance(), 500000)
        assert_equal(self.nodes[1].getbalance(), 500000)
        assert_equal(self.nodes[2].getbalance(), 0)

        # Send 210.000 DOGE from 0 to 2 using sendtoaddress call.
        # Second transaction will be child of first, and will require a fee
        self.nodes[0].sendtoaddress(self.nodes[2].getnewaddress(), 110000)
        self.nodes[0].sendtoaddress(self.nodes[2].getnewaddress(), 100000)

        walletinfo = self.nodes[0].getwalletinfo()
        assert_equal(walletinfo['immature_balance'], 0)

        # Have node0 mine a block, thus it will collect its own fee.
        self.nodes[0].generate(1)
        self.sync_all()

        # Have node1 generate 60 blocks (so node0 can recover the fee)
        self.nodes[1].generate(60)
        self.sync_all()

        # node0 should end up with 1.000.000 doge in block rewards plus fees, but
        # minus the 210.000 plus fees sent to node2
        assert_equal(self.nodes[0].getbalance(), 1000000-210000)
        assert_equal(self.nodes[2].getbalance(), 210000)

        # Node0 should have two unspent outputs.
        # Create a couple of transactions to send them to node2, submit them through 
        # node1, and make sure both node0 and node2 pick them up properly: 
        node0utxos = self.nodes[0].listunspent(1)
        assert_equal(len(node0utxos), 2)

        # create both transactions
        txns_to_send = []
        for utxo in node0utxos: 
            inputs = []
            outputs = {}
            inputs.append({ "txid" : utxo["txid"], "vout" : utxo["vout"]})
            outputs[self.nodes[2].getnewaddress("from1")] = utxo["amount"]
            raw_tx = self.nodes[0].createrawtransaction(inputs, outputs)
            txns_to_send.append(self.nodes[0].signrawtransaction(raw_tx))

        # Have node 1 (miner) send the transactions
        self.nodes[1].sendrawtransaction(txns_to_send[0]["hex"], True)
        self.nodes[1].sendrawtransaction(txns_to_send[1]["hex"], True)

        # Have node1 mine a block to confirm transactions:
        self.nodes[1].generate(1)
        self.sync_all()

        assert_equal(self.nodes[0].getbalance(), 0)
        assert_equal(self.nodes[2].getbalance(), 1000000)
        assert_equal(self.nodes[2].getbalance("from1"), 1000000-210000)

        # Send 100000 DOGE normal
        address = self.nodes[0].getnewaddress("test")
        self.nodes[2].settxfee(1)
        txid = self.nodes[2].sendtoaddress(address, 100000, "", "", False)
        self.nodes[2].generate(1)
        self.sync_all()
        assert_equal(self.nodes[2].getbalance(), 899999)
        assert_equal(self.nodes[0].getbalance(), 100000)

        # Send 100000 DOGE with subtract fee from amount
        txid = self.nodes[2].sendtoaddress(address, 100000, "", "", True)
        self.nodes[2].generate(1)
        self.sync_all()
        assert_equal(self.nodes[2].getbalance(), 799999)
        assert_equal(self.nodes[0].getbalance(), 199999)

        # Sendmany 100000 DOGE
        txid = self.nodes[2].sendmany('from1', {address: 100000}, 0, "", [])
        self.nodes[2].generate(1)
        self.sync_all()
        assert_equal(self.nodes[2].getbalance(), 699998)
        assert_equal(self.nodes[0].getbalance(), 299999)

        # Sendmany 100000 with subtract fee from amount
        txid = self.nodes[2].sendmany('from1', {address: 100000}, 0, "", [address])
        self.nodes[2].generate(1)
        self.sync_all()
        assert_equal(self.nodes[2].getbalance(), 599998)
        assert_equal(self.nodes[0].getbalance(), 399998)

        # Test ResendWalletTransactions:
        # Create a couple of transactions, then start up a fourth
        # node (nodes[3]) and ask nodes[0] to rebroadcast.
        # EXPECT: nodes[3] should have those transactions in its mempool.
        txid1 = self.nodes[0].sendtoaddress(self.nodes[1].getnewaddress(), 1)
        txid2 = self.nodes[1].sendtoaddress(self.nodes[0].getnewaddress(), 1)
        sync_mempools(self.nodes)

        self.nodes.append(start_node(3, self.options.tmpdir))
        connect_nodes_bi(self.nodes, 0, 3)
        sync_blocks(self.nodes)

        relayed = self.nodes[0].resendwallettransactions()
        assert_equal(set(relayed), set([txid1, txid2]))
        sync_mempools(self.nodes)

        assert(txid1 in self.nodes[3].getrawmempool())
        
        #check if we can list zero value tx as available coins
        #1. create rawtx
        #2. hex-changed one output to 0.0 
        #3. sign and send
        #4. check if recipient (node0) can list the zero value tx
        usp = self.nodes[1].listunspent()
        inputs = [{"txid":usp[0]['txid'], "vout":usp[0]['vout']}]
        outputs = {self.nodes[1].getnewaddress(): 499998, self.nodes[0].getnewaddress(): 11.11}
        
        rawTx = self.nodes[1].createrawtransaction(inputs, outputs).replace("c0833842", "00000000") #replace 11.11 with 0.0 (int32)
        decRawTx = self.nodes[1].decoderawtransaction(rawTx)
        signedRawTx = self.nodes[1].signrawtransaction(rawTx)
        decRawTx = self.nodes[1].decoderawtransaction(signedRawTx['hex'])
        zeroValueTxid= decRawTx['txid']
        sendResp = self.nodes[1].sendrawtransaction(signedRawTx['hex'])
        
        self.sync_all()
        self.nodes[1].generate(1) #mine a block
        self.sync_all()
        
        unspentTxs = self.nodes[0].listunspent() #zero value tx must be in listunspents output
        found = False
        for uTx in unspentTxs:
            if uTx['txid'] == zeroValueTxid:
                found = True
                assert_equal(uTx['amount'], Decimal('0.00000000'));
        assert(found)
        
        #do some -walletbroadcast tests
        stop_nodes(self.nodes)
        wait_bitcoinds()
        self.nodes = start_nodes(3, self.options.tmpdir, [["-walletbroadcast=0"],["-walletbroadcast=0"],["-walletbroadcast=0"]])
        connect_nodes_bi(self.nodes,0,1)
        connect_nodes_bi(self.nodes,1,2)
        connect_nodes_bi(self.nodes,0,2)
        self.sync_all()

        txIdNotBroadcasted  = self.nodes[0].sendtoaddress(self.nodes[2].getnewaddress(), 20000);
        txObjNotBroadcasted = self.nodes[0].gettransaction(txIdNotBroadcasted)
        self.nodes[1].generate(1) #mine a block, tx should not be in there
        self.sync_all()
        assert_equal(self.nodes[2].getbalance(), 599998); #should not be changed because tx was not broadcasted
        
        #now broadcast from another node, mine a block, sync, and check the balance
        self.nodes[1].sendrawtransaction(txObjNotBroadcasted['hex'])
        self.nodes[1].generate(1)
        self.sync_all()
        txObjNotBroadcasted = self.nodes[0].gettransaction(txIdNotBroadcasted)
        assert_equal(self.nodes[2].getbalance(), 619998); #should not be
        
        #create another tx
        txIdNotBroadcasted  = self.nodes[0].sendtoaddress(self.nodes[2].getnewaddress(), 20000);
        
        #restart the nodes with -walletbroadcast=1
        stop_nodes(self.nodes)
        wait_bitcoinds()
        self.nodes = start_nodes(3, self.options.tmpdir)
        connect_nodes_bi(self.nodes,0,1)
        connect_nodes_bi(self.nodes,1,2)
        connect_nodes_bi(self.nodes,0,2)
        sync_blocks(self.nodes)
        
        self.nodes[0].generate(1)
        sync_blocks(self.nodes)
        
        #tx should be added to balance because after restarting the nodes tx should be broadcastet
        assert_equal(self.nodes[2].getbalance(), 639998); #should not be
        
if __name__ == '__main__':
    WalletTest ().main ()
