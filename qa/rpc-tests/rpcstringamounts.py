#!/usr/bin/env python3
# Copyright (c) 2024 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
"""Test the -rpcstringamounts option.

A Dogecoin amount needs more precision than an IEEE-754 double has once it exceeds 2^53 koinu (about
90,071,992.55 DOGE), so a client that parses JSON numbers as doubles corrupts large amounts - which is how a
transaction ends up appearing to have a negative fee. See https://github.com/dogecoin/dogecoin/issues/1577.

-rpcstringamounts emits the same digits as a JSON string, which every JSON parser hands back untouched.

The test framework parses JSON numbers with decimal.Decimal and leaves JSON strings as str, so the wire type is
visible here as the Python type.
"""

from decimal import Decimal

from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import assert_equal, assert_greater_than, connect_nodes_bi, start_nodes


class RpcStringAmountsTest(BitcoinTestFramework):
    def __init__(self):
        super().__init__()
        self.num_nodes = 2
        self.setup_clean_chain = True

    def setup_nodes(self):
        # node 0 keeps the default JSON-number output; node 1 opts in to strings
        return start_nodes(self.num_nodes, self.options.tmpdir,
                           extra_args=[[], ['-rpcstringamounts']])

    def setup_network(self):
        self.nodes = self.setup_nodes()
        connect_nodes_bi(self.nodes, 0, 1)
        self.is_network_split = False
        self.sync_all()

    def assert_number(self, value, what):
        assert isinstance(value, Decimal), \
            "%s should be a JSON number, got %r (%s)" % (what, value, type(value).__name__)

    def assert_string(self, value, what):
        assert isinstance(value, str), \
            "%s should be a JSON string, got %r (%s)" % (what, value, type(value).__name__)

    def run_test(self):
        self.nodes[0].generate(101)
        self.sync_all()

        # Each node reports its own balance in its own configured form. The two wallets hold different amounts, so
        # only the JSON type is comparable here.
        balance_number = self.nodes[0].getbalance()
        balance_string = self.nodes[1].getbalance()
        self.assert_number(balance_number, "default getbalance")
        self.assert_string(balance_string, "-rpcstringamounts getbalance")
        assert_greater_than(balance_number, 0)

        # Chain-wide values are identical on both nodes, so these compare the digits as well as the type.
        txid = self.nodes[0].sendtoaddress(self.nodes[0].getnewaddress(), 1)
        self.nodes[0].generate(1)
        self.sync_all()

        vout_number = self.nodes[0].getrawtransaction(txid, 1)['vout'][0]['value']
        vout_string = self.nodes[1].getrawtransaction(txid, 1)['vout'][0]['value']
        self.assert_number(vout_number, "default getrawtransaction vout value")
        self.assert_string(vout_string, "-rpcstringamounts getrawtransaction vout value")
        assert_equal(Decimal(vout_string), vout_number)

        # gettxoutsetinfo total_amount goes through the arith_uint256 overload
        total_number = self.nodes[0].gettxoutsetinfo()['total_amount']
        total_string = self.nodes[1].gettxoutsetinfo()['total_amount']
        self.assert_number(total_number, "default gettxoutsetinfo total_amount")
        self.assert_string(total_string, "-rpcstringamounts gettxoutsetinfo total_amount")
        assert_equal(Decimal(total_string), total_number)

        # An amount in string form is still accepted as an input, so a value read out of one call can be handed
        # straight back to another without converting it through a float.
        address = self.nodes[1].getnewaddress()
        roundtrip_txid = self.nodes[0].sendtoaddress(address, "1.23456789")
        self.nodes[0].generate(1)
        self.sync_all()
        sent_vouts = self.nodes[1].getrawtransaction(roundtrip_txid, 1)['vout']
        assert any(Decimal(v['value']) == Decimal("1.23456789") for v in sent_vouts), \
            "string input amount did not round-trip: %r" % (sent_vouts,)

        self.log_amounts(vout_number, vout_string)

    def log_amounts(self, number, string):
        print("  default          -> %r (%s)" % (number, type(number).__name__))
        print("  -rpcstringamounts -> %r (%s)" % (string, type(string).__name__))


if __name__ == '__main__':
    RpcStringAmountsTest().main()
