#!/usr/bin/env python3
# Copyright (c) 2026 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
"""Test that removeprunedfunds does not leave dangling wtxOrdered entries.

Removed transactions must also be removed from the ordered transaction
index, otherwise later wallet activity and listtransactions can access
freed memory (see bitcoin#9729 / bitcoin#13437).

Without the fix, expect the node to crash (or list removed txs) before
assertions; the harness may die on the UAF.

A restart checks that removals were deleted from the wallet on disk, not
only dropped in memory.

Also checks the mapTxSpends coupling: removing a transaction that spends
an output another wallet transaction also spends, then re-adding the
spender, must not drop the remaining spender. The mempool is cleared by
deleting mempool.dat and starting with -walletbroadcast=0. Raising
-minrelaytxfee empties the mempool but then rejects the original
transaction on the way back in (reject code 66, "insufficient priority").
"""

import os

from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import *


class RemovePrunedFundsOrderedTest(BitcoinTestFramework):

    def __init__(self):
        super().__init__()
        self.setup_clean_chain = True
        self.num_nodes = 1

    def setup_network(self, split=False):
        self.nodes = start_nodes(self.num_nodes, self.options.tmpdir, [["-walletrbf=1"]])
        self.is_network_split = False

    def listed_txids(self, node):
        return set(tx["txid"] for tx in node.listtransactions("*", 1000, 0, True))

    def run_test(self):
        node = self.nodes[0]
        node.generate(101)

        # --- original ordered-index regression ---
        removed = []
        for i in range(10):
            removed.append(node.sendtoaddress(node.getnewaddress(), 3))
        node.generate(1)

        for txid in removed:
            node.removeprunedfunds(txid)

        # New wallet activity to reuse freed memory, checking the
        # ordered index each round.
        for i in range(20):
            node.sendtoaddress(node.getnewaddress("label-%d-%s" % (i, "x" * 80)), 1)
            listed = self.listed_txids(node)
            for txid in removed:
                assert txid not in listed
        node.generate(1)
        for i in range(3):
            listed = self.listed_txids(node)
            for txid in removed:
                assert txid not in listed

        # --- mapTxSpends coupling: two wallet txs spend the same output ---
        dest = node.getnewaddress()
        spender1 = node.sendtoaddress(dest, 10)
        spender1_hex = node.gettransaction(spender1)["hex"]
        decoded = node.decoderawtransaction(spender1_hex)
        shared_inputs = [(vin["txid"], vin["vout"]) for vin in decoded["vin"]]
        spender2 = node.bumpfee(spender1)["txid"]
        assert len(node.gettransaction(spender1)["walletconflicts"]) > 0
        assert spender1 in self.listed_txids(node)
        assert spender2 in self.listed_txids(node)

        # Drop mempool copies so the original can be re-submitted after removal.
        # There is no -persistmempool. Shutdown writes mempool.dat, startup
        # loads it, and ReacceptWalletTransactions submits unconfirmed wallet
        # txs again. -minrelaytxfee=1 keeps both out, then sendrawtransaction
        # of the original hits the same relay check. Delete the dump and
        # disable wallet rebroadcast instead; the RPC submit path still
        # accepts a fee that met the default relay policy.
        stop_node(node, 0)
        mempool_path = log_filename(self.options.tmpdir, 0, "mempool.dat")
        if os.path.isfile(mempool_path):
            os.remove(mempool_path)
        self.nodes[0] = start_node(0, self.options.tmpdir, ["-walletrbf=1", "-walletbroadcast=0"])
        node = self.nodes[0]
        assert_equal(len(node.getrawmempool()), 0)
        for txid in removed:
            assert txid not in self.listed_txids(node)
        assert spender1 in self.listed_txids(node)
        assert spender2 in self.listed_txids(node)

        node.removeprunedfunds(spender1)
        listed = self.listed_txids(node)
        assert spender1 not in listed
        assert spender2 in listed
        # Survivor must still mark the shared inputs spent. Erasing mapTxSpends
        # by outpoint (instead of by this hash) would make them look unspent.
        unspent = set((u["txid"], u["vout"]) for u in node.listunspent(0))
        for prev in shared_inputs:
            assert prev not in unspent

        node.sendrawtransaction(spender1_hex, True)
        listed = self.listed_txids(node)
        assert spender1 in listed
        assert spender2 in listed
        unspent = set((u["txid"], u["vout"]) for u in node.listunspent(0))
        for prev in shared_inputs:
            assert prev not in unspent

        # The re-added spender must have its own spend record. Removing the
        # other spender now must leave the shared inputs spent, and must not
        # resurrect or crash on the ordered index.
        node.removeprunedfunds(spender2)
        listed = self.listed_txids(node)
        assert spender1 in listed
        assert spender2 not in listed
        unspent = set((u["txid"], u["vout"]) for u in node.listunspent(0))
        for prev in shared_inputs:
            assert prev not in unspent


if __name__ == '__main__':
    RemovePrunedFundsOrderedTest().main()
