#!/usr/bin/env python3
# Copyright (c) 2020 The Bitcoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
"""Test error messages for 'getaddressinfo' and 'validateaddress' RPC commands."""

from test_framework.test_framework import BitcoinTestFramework

from test_framework.util import assert_raises_rpc_error

BECH32_VALID = 'dcrt1qtmp74ayg7p24uslctssvjm06q5phz4yrrsejrj'
BECH32_INVALID_BECH32 = 'dcrt1p0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vqhzy0as'
BECH32_INVALID_BECH32M = 'dcrt1qw508d6qejxtdg4y5r3zarvary0c5xw7k5c6ejn'
BECH32_INVALID_VERSION = 'dcrt130xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7vq72wpd7'
BECH32_INVALID_SIZE = 'dcrt1s0xlxvlhemja6c4dqv22uapctqupfhlxm9h8z3k2e72q4k9hcz7v8n0nx0muaewav254t575s'
BECH32_INVALID_V0_SIZE = 'dcrt1qw508d6qejxtdg4y5r3zarvary0c5xw7kqq2z6vya'
BECH32_INVALID_PREFIX = 'dc1pw508d6qejxtdg4y5r3zarvary0c5xw7kw508d6qejxtdg4y5r3zarvary0c5xw7k52pzt8'

BASE58_VALID = 'ngLGV6xaxmFHZFB6g6DX7YjMjDY8dnvuTw'
BASE58_INVALID_PREFIX = '17VZNX1SN5NtKa8UQFxwQbFeFc3iqRYhem'

INVALID_ADDRESS = 'asfah14i8fajz0123f'

class InvalidAddressErrorMessageTest(BitcoinTestFramework):
    def set_test_params(self):
        self.setup_clean_chain = True
        self.num_nodes = 1

    def skip_test_if_missing_module(self):
        self.skip_if_no_wallet()

    def test_validateaddress(self):
        node = self.nodes[0]

        # Bech32
        info = node.validateaddress(BECH32_INVALID_SIZE)
        assert not info['isvalid']

        info = node.validateaddress(BECH32_INVALID_PREFIX)
        assert not info['isvalid']

        info = node.validateaddress(BECH32_INVALID_BECH32)
        assert not info['isvalid']

        info = node.validateaddress(BECH32_INVALID_BECH32M)
        assert not info['isvalid']

        info = node.validateaddress(BECH32_INVALID_V0_SIZE)
        assert not info['isvalid']

        info = node.validateaddress(BECH32_VALID)
        assert info['isvalid']
        assert 'error' not in info

        # Base58
        info = node.validateaddress(BASE58_INVALID_PREFIX)
        assert not info['isvalid']

        info = node.validateaddress(BASE58_VALID)
        assert info['isvalid']
        assert 'error' not in info

        # Invalid address format
        info = node.validateaddress(INVALID_ADDRESS)
        assert not info['isvalid']

    def test_getaddressinfo(self):
        node = self.nodes[0]

        assert_raises_rpc_error(-5, "Invalid address", node.getaddressinfo, BECH32_INVALID_SIZE)

        assert_raises_rpc_error(-5, "Invalid address", node.getaddressinfo, BECH32_INVALID_PREFIX)

        assert_raises_rpc_error(-5, "Invalid address", node.getaddressinfo, BASE58_INVALID_PREFIX)

        assert_raises_rpc_error(-5, "Invalid address", node.getaddressinfo, INVALID_ADDRESS)

    def run_test(self):
        self.test_validateaddress()
        self.test_getaddressinfo()


if __name__ == '__main__':
    InvalidAddressErrorMessageTest().main()
