#!/usr/bin/env python3
# Copyright (c) 2023 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

#
# Test -walletnotify
#

from test_framework.mininode import wait_until
from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import *

import os # for os.remove

class WalletNotifyTest(BitcoinTestFramework):

    def __init__(self):
        super().__init__()
        self.num_nodes = 4
        self.setup_clean_chain = False

    notify_filename = None  # Set by setup_network
    current_line = 0
    notifs = []
    utxo_used = []

    def setup_network(self):
        self.nodes = []
        self.notify_filename = os.path.join(self.options.tmpdir, "notify.txt")
        with open(self.notify_filename, 'w', encoding='utf8'):
            pass  # Just open then close to create zero-length file
        self.nodes.append(start_node(0, self.options.tmpdir,
                            ["-walletnotify=echo %s %i >> \"" + self.notify_filename + "\""]))
        self.nodes.append(start_node(1, self.options.tmpdir,["-debug", "-acceptnonstdtxn=0"]))
        self.nodes.append(start_node(2, self.options.tmpdir,["-debug", "-acceptnonstdtxn=0", "-minrelaytxfee=0.00000001"]))
        self.nodes.append(start_node(3, self.options.tmpdir,["-debug", "-acceptnonstdtxn=0", "-blockmaxsize=80"]))
        connect_nodes(self.nodes[1], 0)
        connect_nodes(self.nodes[2], 1)
        connect_nodes(self.nodes[3], 1)

        self.is_network_split = False
        self.sync_all()

    def get_notifications(self):
        with open(self.notify_filename, 'r', encoding='utf8') as f:
            notif_text = f.read()
        if len(notif_text) == 0:
            return [];
        return notif_text.split("\n")[:-1] # take out the last entry due to trailing \n

    def wait_for_notifications(self, num, exact):
        def notifications_received():
            self.notifs = self.get_notifications()
            if exact:
                return len(self.notifs) == self.current_line + num
            return len(self.notifs) >= self.current_line + num
        return wait_until(notifications_received, timeout=30)

    def send_raw_tx(self, node, addr, fee, reuse_last_utxo=False):
        if reuse_last_utxo:
            input = self.utxo_used[-1]
        else:
            input = [node.listunspent()[0]]
            self.utxo_used.append(input)

        output = { addr : input[0]['amount'] - fee }
        rawtx = node.createrawtransaction(input, output)
        signedtx = node.signrawtransaction(rawtx)
        return node.sendrawtransaction(signedtx['hex'])

    def run_test(self):
        # mine 62 blocks from node 1, send 1000 doge to node 2
        self.nodes[1].generate(61)
        self.nodes[1].sendtoaddress(self.nodes[2].getnewaddress(), 1000)
        self.nodes[1].generate(1)
        self.sync_all()

        # make sure there are no notifications yet
        assert len(self.get_notifications()) == 0

        # send a tx to node0's wallet
        address = self.nodes[0].getnewaddress()
        txid = self.nodes[1].sendtoaddress(address, 1337)
        self.sync_all()

        # check that we got a notification for the unconfirmed transaction
        assert self.wait_for_notifications(1, True)
        assert self.notifs[self.current_line] == "{} {}".format(txid, 0)
        assert self.nodes[0].gettransaction(txid)['confirmations'] == 0
        self.current_line += 1


        # mine a block to confirm the tx
        self.nodes[1].generate(1)
        self.sync_all()

        # check that we got a notification for the confirmed transaction
        assert self.wait_for_notifications(1, True)
        height = self.nodes[1].getblockchaininfo()['blocks']
        assert self.notifs[self.current_line] == "{} {}".format(txid, height)
        assert self.nodes[0].gettransaction(txid)['confirmations'] == 1
        self.current_line += 1

        # rollback the chain and mine 3 empty blocks with node 3
        reset_hash = self.nodes[1].getblockhash(height)
        self.nodes[3].invalidateblock(reset_hash)
        self.nodes[3].generate(3)
        self.sync_all()

        # assure that we didn't mine any transaction
        checkblock = self.nodes[3].getbestblockhash()
        for _ in range(3):
            blk = self.nodes[3].getblock(checkblock, 1)
            assert len(blk['tx']) == 1
            checkblock = blk['previousblockhash']

        # we should now receive 2 notifications:
        # - from the transaction being put into the mempool (AcceptToMemoryPool)
        # - from the transaction no longer being in the best chain (DisconnectTip)
        assert self.wait_for_notifications(2, True)
        assert self.notifs[self.current_line] == "{} {}".format(txid, 0)
        assert self.notifs[self.current_line + 1] == "{} {}".format(txid, 0)
        self.current_line += 2

        # mine the same transaction again and make sure it's in the mempool by
        # force submitting it on the miner node.
        try:
            self.nodes[1].sendrawtransaction(self.nodes[0].gettransaction(txid)['hex'], True)
        except:
            pass
        self.nodes[1].generate(1)
        self.sync_all()

        # we should now have received one more notification.
        assert self.wait_for_notifications(1, True)
        mined_in = self.nodes[0].gettransaction(txid)['blockhash']
        height = self.nodes[0].getblock(mined_in)['height']
        assert self.notifs[self.current_line] == "{} {}".format(txid, height)
        assert self.nodes[0].gettransaction(txid)['confirmations'] >= 1
        self.current_line += 1

        # send a transaction from node 2 with low fee, and mine it straight
        txid = self.send_raw_tx(self.nodes[2], address, Decimal("0.00000200"))
        self.nodes[2].generate(1)
        self.sync_all()

        # we should now have received only one notification because we wouldn't
        # have accepted this transaction in mempool
        assert self.wait_for_notifications(1, True)
        height = self.nodes[0].getblockchaininfo()['blocks']
        assert self.notifs[self.current_line] == "{} {}".format(txid, height)
        assert self.nodes[0].gettransaction(txid)['confirmations'] == 1
        self.current_line += 1

        # invalidate the last block on node 2
        reset_hash = self.nodes[2].getblockhash(height)
        self.nodes[2].invalidateblock(reset_hash)

        # stop, remove the mempool, zap the wallet, and start again
        self.nodes[2].stop()
        dogecoind_processes[2].wait()
        os.remove(log_filename(self.options.tmpdir, 2, "mempool.dat"))
        self.nodes[2] = start_node(2, self.options.tmpdir,["-debug", "-acceptnonstdtxn=0", "-minrelaytxfee=1", "-zapwallettxes"])

        txid_ds = self.send_raw_tx(self.nodes[2], address, Decimal("1"))

        # mine 3 blocks on node 2, reconnect and sync
        self.nodes[2].generate(3)
        connect_nodes(self.nodes[2], 1)
        self.sync_all()

        # we should now receive 4 notifications:
        # - from the prior transaction being put into the mempool (AcceptToMemoryPool)
        # - from the prior transaction no longer being in the best chain (DisconnectTip)
        # - from the prior transaction being conflicted (MemPoolConflictRemovalTracker)
        # - for the new transaction that double-spent the previous transaction.
        mined_in = self.nodes[0].gettransaction(txid_ds)['blockhash']
        height = self.nodes[0].getblock(mined_in)['height']
        assert self.wait_for_notifications(4, True)
        for i in range(0,4):
            assert self.notifs[self.current_line + i] in [
                "{} {}".format(txid, 0),
                "{} {}".format(txid_ds, height)
            ]
        assert self.nodes[0].gettransaction(txid)['confirmations'] == -3
        assert len(self.nodes[0].gettransaction(txid)['walletconflicts']) == 1
        assert self.nodes[0].gettransaction(txid_ds)['confirmations'] == 3
        self.current_line += 4

        # mine 10 more blocks
        self.nodes[1].generate(10)
        self.sync_all()

        # check that we got no more notifications
        assert self.wait_for_notifications(0, True)
        assert self.nodes[0].gettransaction(txid_ds)['confirmations'] == 13


if __name__ == '__main__':
    WalletNotifyTest().main()
