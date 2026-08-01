#!/usr/bin/env python3
# Copyright (c) 2026 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

"""Test BIP157 compact block filter P2P message handling."""

import struct
import time

from test_framework.mininode import *
from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import bytes_to_hex_str, hex_str_to_bytes, p2p_port, start_node


NODE_COMPACT_FILTERS = 1 << 6
CFCHECKPT_INTERVAL = 1000


class msg_getcfilters(object):
    command = b"getcfilters"

    def __init__(self, filter_type=0, start_height=0, stop_hash=0):
        self.filter_type = filter_type
        self.start_height = start_height
        self.stop_hash = stop_hash

    def serialize(self):
        return struct.pack("<BI", self.filter_type, self.start_height) + ser_uint256(self.stop_hash)


class msg_getcfheaders(object):
    command = b"getcfheaders"

    def __init__(self, filter_type=0, start_height=0, stop_hash=0):
        self.filter_type = filter_type
        self.start_height = start_height
        self.stop_hash = stop_hash

    def serialize(self):
        return struct.pack("<BI", self.filter_type, self.start_height) + ser_uint256(self.stop_hash)


class msg_getcfcheckpt(object):
    command = b"getcfcheckpt"

    def __init__(self, filter_type=0, stop_hash=0):
        self.filter_type = filter_type
        self.stop_hash = stop_hash

    def serialize(self):
        return struct.pack("<B", self.filter_type) + ser_uint256(self.stop_hash)


class msg_cfilter(object):
    command = b"cfilter"

    def deserialize(self, f):
        self.filter_type = struct.unpack("<B", f.read(1))[0]
        self.block_hash = deser_uint256(f)
        self.filter_data = deser_string(f)


class msg_cfheaders(object):
    command = b"cfheaders"

    def deserialize(self, f):
        self.filter_type = struct.unpack("<B", f.read(1))[0]
        self.stop_hash = deser_uint256(f)
        self.prev_header = deser_uint256(f)
        self.filter_hashes = deser_uint256_vector(f)


class msg_cfcheckpt(object):
    command = b"cfcheckpt"

    def deserialize(self, f):
        self.filter_type = struct.unpack("<B", f.read(1))[0]
        self.stop_hash = deser_uint256(f)
        self.headers = deser_uint256_vector(f)


NodeConn.messagemap[b"cfilter"] = msg_cfilter
NodeConn.messagemap[b"cfheaders"] = msg_cfheaders
NodeConn.messagemap[b"cfcheckpt"] = msg_cfcheckpt


class CompactFilterP2PNode(SingleNodeConnCB):
    def __init__(self):
        super().__init__()
        self.version_services = 0
        self.cfilters = []
        self.cfheaders = None
        self.cfcheckpt = None

    def on_version(self, conn, message):
        self.version_services = message.nServices
        super().on_version(conn, message)

    def on_cfilter(self, conn, message):
        self.cfilters.append(message)

    def on_cfheaders(self, conn, message):
        self.cfheaders = message

    def on_cfcheckpt(self, conn, message):
        self.cfcheckpt = message

    def wait_for_disconnect(self, timeout=60):
        if self.connection is None:
            return True
        sleep_time = 0.05
        is_closed = self.connection.state == "closed"
        while not is_closed and timeout > 0:
            time.sleep(sleep_time)
            timeout -= sleep_time
            is_closed = self.connection.state == "closed"
        return is_closed

    def clear_blockfilters(self):
        self.cfilters = []
        self.cfheaders = None
        self.cfcheckpt = None


class P2PBlockFiltersTest(BitcoinTestFramework):
    def __init__(self):
        super().__init__()
        self.setup_clean_chain = True
        self.num_nodes = 1

    def setup_network(self):
        self.nodes = []
        self.nodes.append(start_node(0, self.options.tmpdir, ["-blockfilterindex", "-peerblockfilters=1"]))

    def run_test(self):
        node = self.nodes[0]

        node.generate(CFCHECKPT_INTERVAL + 1)

        wait_until(lambda: node.getindexinfo()["basic block filter index"]["synced"], timeout=120)

        peer = CompactFilterP2PNode()
        conn = NodeConn("127.0.0.1", p2p_port(0), node, peer)
        peer.add_connection(conn)
        NetworkThread().start()
        peer.wait_for_verack()

        assert peer.version_services & NODE_COMPACT_FILTERS

        chain_hashes = [node.getblockhash(height) for height in range(6)]
        rpc_filters = [node.getblockfilter(block_hash, "basic") for block_hash in chain_hashes]

        peer.clear_blockfilters()
        peer.send_and_ping(msg_getcfilters(0, 0, int(chain_hashes[5], 16)))
        assert len(peer.cfilters) == 6
        for i, message in enumerate(peer.cfilters):
            assert message.filter_type == 0
            assert message.block_hash == int(chain_hashes[i], 16)
            assert bytes_to_hex_str(message.filter_data) == rpc_filters[i]["filter"]

        peer.clear_blockfilters()
        peer.send_and_ping(msg_getcfheaders(0, 1, int(chain_hashes[5], 16)))
        assert peer.cfheaders is not None
        assert peer.cfheaders.filter_type == 0
        assert peer.cfheaders.stop_hash == int(chain_hashes[5], 16)
        assert peer.cfheaders.prev_header == int(rpc_filters[0]["header"], 16)
        expected_filter_hashes = [
            uint256_from_str(hash256(hex_str_to_bytes(result["filter"])))
            for result in rpc_filters[1:]
        ]
        assert peer.cfheaders.filter_hashes == expected_filter_hashes

        checkpoint_height = CFCHECKPT_INTERVAL
        checkpoint_hash = node.getblockhash(checkpoint_height)
        checkpoint_filter = node.getblockfilter(checkpoint_hash, "basic")
        stop_hash = node.getblockhash(CFCHECKPT_INTERVAL + 1)

        peer.clear_blockfilters()
        peer.send_and_ping(msg_getcfcheckpt(0, int(stop_hash, 16)))
        assert peer.cfcheckpt is not None
        assert peer.cfcheckpt.filter_type == 0
        assert peer.cfcheckpt.stop_hash == int(stop_hash, 16)
        assert peer.cfcheckpt.headers == [int(checkpoint_filter["header"], 16)]

        # Keep original connection alive so NetworkThread stays running
        # while we create / destroy peers for negative-path tests.

        # ----- Negative-path tests: malformed / oversized requests -----

        def connect_peer():
            """Create a fresh peer connection for disconnect tests."""
            p = CompactFilterP2PNode()
            c = NodeConn("127.0.0.1", p2p_port(0), node, p)
            p.add_connection(c)
            p.wait_for_verack()
            return p

        # 1. Unsupported filter type on getcfilters → disconnect
        peer = connect_peer()
        peer.send_message(msg_getcfilters(255, 0, int(chain_hashes[5], 16)))
        assert peer.wait_for_disconnect()

        # 2. Unsupported filter type on getcfheaders → disconnect
        peer = connect_peer()
        peer.send_message(msg_getcfheaders(255, 1, int(chain_hashes[5], 16)))
        assert peer.wait_for_disconnect()

        # 3. Unsupported filter type on getcfcheckpt → disconnect
        peer = connect_peer()
        peer.send_message(msg_getcfcheckpt(255, int(chain_hashes[5], 16)))
        assert peer.wait_for_disconnect()

        # 4. Unknown stop hash → disconnect
        peer = connect_peer()
        peer.send_message(msg_getcfilters(0, 0, 0xdeadbeef))
        assert peer.wait_for_disconnect()

        # 5. start_height > stop_height → disconnect
        peer = connect_peer()
        peer.send_message(msg_getcfilters(0, 999, int(chain_hashes[5], 16)))
        assert peer.wait_for_disconnect()

        # 6. Oversized getcfilters range (>= MAX_GETCFILTERS_SIZE=1000) → disconnect
        #    start=1, stop at height 1001 → range = 1000 >= 1000
        tip_hash = int(node.getblockhash(CFCHECKPT_INTERVAL + 1), 16)
        peer = connect_peer()
        peer.send_message(msg_getcfilters(0, 1, tip_hash))
        assert peer.wait_for_disconnect()

        # 7. Oversized getcfheaders range (>= MAX_GETCFHEADERS_SIZE=2000) → disconnect
        #    Generate more blocks so we have height >= 2001
        node.generate(1000)
        wait_until(lambda: node.getindexinfo()["basic block filter index"]["synced"],
                   timeout=120)
        far_hash = int(node.getblockhash(2001), 16)
        peer = connect_peer()
        peer.send_message(msg_getcfheaders(0, 1, far_hash))
        assert peer.wait_for_disconnect()

        # ----- Reorg coverage -----
        #
        # These exercise the cfcheckpt path across a chain reorganisation:
        # that the header walk is anchored to the requested stop block, that
        # a stop hash reorged off the active chain is rejected, and that the
        # response cache cannot serve pre-reorg headers.

        wait_until(lambda: node.getindexinfo()["basic block filter index"]["synced"],
                   timeout=120)

        def rpc_checkpoint_headers(stop_height):
            """Checkpoint headers the node should report for a given stop height."""
            return [
                int(node.getblockfilter(node.getblockhash(h), "basic")["header"], 16)
                for h in range(CFCHECKPT_INTERVAL, stop_height + 1, CFCHECKPT_INTERVAL)
            ]

        # 8. cfcheckpt spanning more than one checkpoint interval.  The
        #    single-checkpoint case above cannot distinguish a correct walk
        #    from one that only ever reads the first entry.
        stop_height = 2 * CFCHECKPT_INTERVAL + 1
        assert node.getblockcount() >= stop_height
        stop_hash = node.getblockhash(stop_height)
        expected_headers = rpc_checkpoint_headers(stop_height)
        assert len(expected_headers) == 2

        peer = connect_peer()
        peer.clear_blockfilters()
        peer.send_and_ping(msg_getcfcheckpt(0, int(stop_hash, 16)))
        assert peer.cfcheckpt is not None
        assert peer.cfcheckpt.headers == expected_headers

        # That request also warmed the response cache for this stop hash.
        pre_reorg_headers = list(peer.cfcheckpt.headers)
        pre_reorg_tip = node.getbestblockhash()

        # 9. Reorg out the blocks below the second checkpoint.  A transaction
        #    is mined onto the replacement chain so that the filter at height
        #    2000 -- and therefore the checkpoint header -- is guaranteed to
        #    differ from the pre-reorg one rather than differing only by an
        #    incidentally distinct coinbase.
        fork_height = 2 * CFCHECKPT_INTERVAL - 5
        node.invalidateblock(node.getblockhash(fork_height + 1))
        assert node.getblockcount() == fork_height

        node.sendtoaddress(node.getnewaddress(), 1)
        node.generate(stop_height - fork_height + 1)
        wait_until(lambda: node.getindexinfo()["basic block filter index"]["synced"],
                   timeout=120)
        assert node.getblockcount() > stop_height

        post_reorg_headers = rpc_checkpoint_headers(stop_height)
        # Guard against a vacuous pass: the reorg must actually have moved the
        # second checkpoint, or the cache assertion below proves nothing.  The
        # first checkpoint is below the fork and must be untouched.
        assert post_reorg_headers[0] == pre_reorg_headers[0]
        assert post_reorg_headers[1] != pre_reorg_headers[1]

        # 10. A stop hash that is no longer on the active chain must be
        #     rejected outright, not served from the cache or a stale index.
        peer = connect_peer()
        peer.send_message(msg_getcfcheckpt(0, int(pre_reorg_tip, 16)))
        assert peer.wait_for_disconnect()

        # 11. cfcheckpt on the reorganised chain must report the new
        #     checkpoint rather than the value cached before the reorg.
        new_stop_hash = node.getblockhash(stop_height)
        peer = connect_peer()
        peer.clear_blockfilters()
        peer.send_and_ping(msg_getcfcheckpt(0, int(new_stop_hash, 16)))
        assert peer.cfcheckpt is not None
        assert peer.cfcheckpt.headers == post_reorg_headers

        # 12. getcfilters must follow the reorg as well: the block at the
        #     first post-fork height is a different block than it was.
        new_block_hash = node.getblockhash(fork_height + 1)
        assert new_block_hash != pre_reorg_tip
        rpc_filter = node.getblockfilter(new_block_hash, "basic")

        peer = connect_peer()
        peer.clear_blockfilters()
        peer.send_and_ping(msg_getcfilters(0, fork_height + 1, int(new_block_hash, 16)))
        assert len(peer.cfilters) == 1
        assert peer.cfilters[0].block_hash == int(new_block_hash, 16)
        assert bytes_to_hex_str(peer.cfilters[0].filter_data) == rpc_filter["filter"]

        # Clean up the original sentinel connection
        conn.disconnect_node()


if __name__ == '__main__':
    P2PBlockFiltersTest().main()