#!/usr/bin/env python3
# Copyright (c) 2024 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
"""Test reporting of unrecognised command line and configuration file arguments.

Dogecoin Core accepts any -option it is given and silently ignores the ones no code reads, so a typo looks exactly
like a setting that took effect. See https://github.com/dogecoin/dogecoin/issues/1313.

Unrecognised arguments are reported under -debug=args. This test checks both directions: that a bogus argument is
reported, and - just as important - that a node started with a spread of real arguments reports nothing, so the
list of known arguments cannot quietly rot.
"""

import os

from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import start_nodes, stop_nodes, initialize_datadir


class UnrecognisedArgsTest(BitcoinTestFramework):
    def __init__(self):
        super().__init__()
        self.num_nodes = 1
        self.setup_clean_chain = True

    def setup_nodes(self):
        return start_nodes(self.num_nodes, self.options.tmpdir,
                           extra_args=[['-debug=args', '-totallybogusoption=1']])

    def setup_network(self):
        self.nodes = self.setup_nodes()

    def debug_log(self, node_index=0):
        path = os.path.join(self.options.tmpdir, "node%d" % node_index, "regtest", "debug.log")
        with open(path, 'r', encoding='utf8') as handle:
            return handle.read()

    def unrecognised_in_log(self, node_index=0):
        return [line.split("Unrecognised argument ", 1)[1].split(" ")[0]
                for line in self.debug_log(node_index).splitlines()
                if "Unrecognised argument " in line]

    def run_test(self):
        # the node was started with -totallybogusoption
        reported = self.unrecognised_in_log()
        assert "-totallybogusoption" in reported, \
            "expected -totallybogusoption to be reported, got %r" % (reported,)

        # an argument the daemon really supports must never be reported
        assert "-debug" not in reported, "-debug was wrongly reported as unrecognised"

        # restart with a spread of real arguments and nothing bogus: nothing should be reported
        stop_nodes(self.nodes)
        datadir = initialize_datadir(self.options.tmpdir, 0)
        os.remove(os.path.join(datadir, "regtest", "debug.log"))

        # a config file entry goes through a different code path than the command line, so cover both
        with open(os.path.join(datadir, "dogecoin.conf"), 'a', encoding='utf8') as handle:
            handle.write("maxconnections=8\n")

        self.nodes = start_nodes(self.num_nodes, self.options.tmpdir,
                                 extra_args=[['-debug=args', '-checkmempool=1', '-dbcache=50',
                                              '-par=2', '-maxorphantx=10', '-datacarrier=1']])
        clean = self.unrecognised_in_log()
        assert clean == [], "real arguments were reported as unrecognised: %r" % (clean,)

        # ...and a bogus entry in the config file is reported too
        stop_nodes(self.nodes)
        os.remove(os.path.join(datadir, "regtest", "debug.log"))
        with open(os.path.join(datadir, "dogecoin.conf"), 'a', encoding='utf8') as handle:
            handle.write("totallybogusconfoption=7\n")

        self.nodes = start_nodes(self.num_nodes, self.options.tmpdir, extra_args=[['-debug=args']])
        from_conf = self.unrecognised_in_log()
        assert from_conf == ["-totallybogusconfoption"], \
            "expected only the bogus config entry, got %r" % (from_conf,)

        print("  reported from command line: -totallybogusoption")
        print("  reported from config file:  -totallybogusconfoption")
        print("  real arguments reported:    none")


if __name__ == '__main__':
    UnrecognisedArgsTest().main()
