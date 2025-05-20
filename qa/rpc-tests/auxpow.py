#!/usr/bin/env python3
# Copyright (c) 2015-2018 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

#
# Test AuxPOW RPC interface and constraints
#

from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import *
from test_framework import scrypt_auxpow

class AuxPOWTest (BitcoinTestFramework):
    REWARD = 500000 # reward per block
    CHAIN_ID = "62"
    DIGISHIELD_START = 10 # nHeight when digishield starts
    AUXPOW_START = 20 # nHeight when auxpow starts
    MATURITY_HEIGHT = 60 # number of blocks for mined transactions to mature

    def setup_chain(self):
        print("Initializing test directory " + self.options.tmpdir)
        initialize_chain_clean(self.options.tmpdir, 2)

    def setup_network(self, split=False):
        self.nodes = start_nodes(2, self.options.tmpdir)
        connect_nodes_bi(self.nodes,0,1)
        self.is_network_split=False
        self.sync_all()

    def run_test(self):
        print("Mining blocks...")

        # 1. mine an auxpow block before auxpow is allowed, expect: fail
        try:
            scrypt_auxpow.mineScryptAux(self.nodes[0], "00", True)
        except JSONRPCException as ex:
            if ex.error['message'] == "getauxblock method is not yet available":
                pass
            else:
                raise ex
        self.sync_all()

        # 2. mine a non-auxpow block, just to ensure that this node
        # can mine at all, expect: success
        self.nodes[0].generate(1)
        self.sync_all()

        # 3. mine blocks until we're in digishield era
        self.nodes[1].generate(self.DIGISHIELD_START - 1 - 1)
        self.sync_all()

        # 4. mine an auxpow block before auxpow is allowed, attempt 2
        # expect: fail
        try:
            scrypt_auxpow.mineScryptAux(self.nodes[0], "00", True)
        except JSONRPCException as ex:
            if ex.error['message'] == "getauxblock method is not yet available":
                pass
            else:
                raise ex
        self.sync_all()

        # 5. mine blocks until we're in auxpow era
        self.nodes[1].generate(self.AUXPOW_START - self.DIGISHIELD_START)
        self.sync_all()

        # 6. mine a valid auxpow block, expect: success
        assert scrypt_auxpow.mineScryptAux(self.nodes[0], "00", True) is True

        # 7. mine an auxpow block with high pow, expect: fail
        assert scrypt_auxpow.mineScryptAux(self.nodes[0], "00", False) is False

        # 8. mine a valid auxpow block with the parent chain being us
        # expect: fail
        assert scrypt_auxpow.mineScryptAux(self.nodes[0], self.CHAIN_ID, True) is False
        self.sync_all()

        # 9. mine enough blocks to mature all node 0 rewards
        self.nodes[1].generate(self.MATURITY_HEIGHT)
        self.sync_all()

        # node 0 should have block rewards for 2 blocks,
        # One from step 2 and one from step 6.
        assert_equal(self.nodes[0].getbalance(), self.REWARD * 2)

if __name__ == '__main__':
    AuxPOWTest ().main ()
