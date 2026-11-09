#!/usr/bin/env python
# Copyright (c) 2021 The Dogecoin Core Developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.
"""CreateAuxBlock QA test.

# Tests createauxblock and submitauxblock RPC endpoints
"""

from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import *

from test_framework import scrypt_auxpow as auxpow

class CreateAuxBlockTest(BitcoinTestFramework):

  def __init__(self):
      super().__init__()
      self.setup_clean_chain = True
      self.num_nodes = 2
      self.is_network_split = False

  def setup_network(self):
      self.nodes = []
      self.nodes.append(start_node(0, self.options.tmpdir, ["-debug", "-txindex"]))
      self.nodes.append(start_node(1, self.options.tmpdir, ["-debug", "-rpcnamecoinapi"]))
      connect_nodes_bi(self.nodes, 0, 1)
      self.sync_all()

  def run_test(self):
    # Generate an initial chain
    self.nodes[0].generate(100)
    self.sync_all()
    # Generate a block so that we are not "downloading blocks".
    self.nodes[1].generate(1)
    self.sync_all()

    dummy_p2pkh_addr = "mmMP9oKFdADezYzduwJFcLNmmi8JHUKdx9"
    dummy_p2sh_addr = "2Mwvgpd2H7wDPXx8jWe3Vqiciix6JqSbsyz"

    # Compare basic data of createauxblock to getblocktemplate.
    auxblock = self.nodes[0].createauxblock(dummy_p2pkh_addr)
    blocktemplate = self.nodes[0].getblocktemplate()
    assert_equal(auxblock["coinbasevalue"], blocktemplate["coinbasevalue"])
    assert_equal(auxblock["bits"], blocktemplate["bits"])
    assert_equal(auxblock["height"], blocktemplate["height"])
    assert_equal(auxblock["previousblockhash"], blocktemplate["previousblockhash"])

    # Compare target and take byte order into account.
    target = auxblock["target"]
    reversedTarget = auxpow.reverseHex(target)
    assert_equal(reversedTarget, blocktemplate["target"])

    # Verify data that can be found in another way.
    assert_equal(auxblock["chainid"], 98)
    assert_equal(auxblock["height"], self.nodes[0].getblockcount() + 1)
    assert_equal(auxblock["previousblockhash"], self.nodes[0].getblockhash(auxblock["height"] - 1))

    # Calling again should give the same block.
    auxblock2 = self.nodes[0].createauxblock(dummy_p2pkh_addr)
    assert_equal(auxblock2, auxblock)

    # Calling with an invalid address must fail
    try:
      auxblock2 = self.nodes[0].createauxblock("x")
      raise AssertionError("invalid address accepted")
    except JSONRPCException as exc:
      assert_equal(exc.error["code"], -8)

    # Calling with a different address ...
    dummy_addr2 = self.nodes[0].getnewaddress()
    auxblock3 = self.nodes[0].createauxblock(dummy_addr2)

    # ... must give another block because the coinbase recipient differs  ...
    assert auxblock3["hash"] != auxblock["hash"]

    # ... but must have retained the same parameterization otherwise
    assert_equal(auxblock["coinbasevalue"], auxblock3["coinbasevalue"])
    assert_equal(auxblock["bits"], auxblock3["bits"])
    assert_equal(auxblock["height"], auxblock3["height"])
    assert_equal(auxblock["previousblockhash"], auxblock3["previousblockhash"])
    assert_equal(auxblock["chainid"], auxblock3["chainid"])
    assert_equal(auxblock["target"], auxblock3["target"])

    # Invalid format for hash - fails before checking auxpow
    try:
      self.nodes[0].submitauxblock("00", "x")
      raise AssertionError("malformed hash accepted")
    except JSONRPCException as exc:
      assert_equal(exc.error['code'], -8)
      assert("hash must be of length 64" in exc.error["message"])

    # Invalid format for auxpow.
    try:
      self.nodes[0].submitauxblock(auxblock2['hash'], "x")
      raise AssertionError("malformed auxpow accepted")
    except JSONRPCException as exc:
      assert_equal(exc.error['code'], -22)
      assert("decode failed" in exc.error["message"])

    # If we receive a new block, the old hash will be replaced.
    self.sync_all()
    self.nodes[1].generate(1)
    self.sync_all()
    auxblock2 = self.nodes[0].createauxblock(dummy_p2pkh_addr)
    assert auxblock['hash'] != auxblock2['hash']
    apow = auxpow.computeAuxpowWithChainId(auxblock['hash'], auxpow.reverseHex(auxblock['target']), "98", True)
    try:
        self.nodes[0].submitauxblock(auxblock['hash'], apow)
        raise AssertionError("invalid block hash accepted")
    except JSONRPCException as exc:
        assert_equal(exc.error['code'], -8)
        assert("block hash unknown" in exc.error["message"])

    # Auxpow doesn't match given hash
    res = self.nodes[0].submitauxblock(auxblock2['hash'], apow)
    assert not res

    # Invalidate the block again, send a transaction and query for the
    # auxblock to solve that contains the transaction.
    self.nodes[0].generate(1)
    addr = self.nodes[1].getnewaddress()
    txid = self.nodes[0].sendtoaddress(addr, 1)
    self.sync_all()
    assert_equal(self.nodes[1].getrawmempool(), [txid])
    auxblock = self.nodes[0].createauxblock(dummy_p2pkh_addr)
    reversedTarget = auxpow.reverseHex(auxblock["target"])

    # Compute invalid auxpow.
    apow = auxpow.computeAuxpowWithChainId(auxblock["hash"], reversedTarget, "98", False)
    res = self.nodes[0].submitauxblock(auxblock["hash"], apow)
    assert not res

    # Compute and submit valid auxpow.
    apow = auxpow.computeAuxpowWithChainId(auxblock["hash"], reversedTarget, "98", True)
    res = self.nodes[0].submitauxblock(auxblock["hash"], apow)
    assert res

    # Make sure that the block is accepted.
    self.sync_all()
    assert_equal(self.nodes[1].getrawmempool(), [])
    height = self.nodes[1].getblockcount()
    assert_equal(height, auxblock["height"])
    assert_equal(self.nodes[1].getblockhash(height), auxblock["hash"])

    # check the mined block and transaction
    self.check_mined_block(auxblock, apow, dummy_p2pkh_addr, Decimal("500000"), txid)

    # Mine to a p2sh address while having multiple cached aux block templates
    auxblock1 = self.nodes[0].createauxblock(dummy_p2pkh_addr)
    auxblock2 = self.nodes[0].createauxblock(dummy_p2sh_addr)
    auxblock3 = self.nodes[0].createauxblock(dummy_addr2)
    reversedTarget = auxpow.reverseHex(auxblock2["target"])
    apow = auxpow.computeAuxpowWithChainId(auxblock2["hash"], reversedTarget, "98", True)
    res = self.nodes[0].submitauxblock(auxblock2["hash"], apow)
    assert res

    self.sync_all()

    # check the mined block
    self.check_mined_block(auxblock2, apow, dummy_p2sh_addr, Decimal("500000"))

    # Solve the first p2pkh template before requesting a new auxblock
    # this succeeds but creates a chaintip fork
    reversedTarget = auxpow.reverseHex(auxblock1["target"])
    apow = auxpow.computeAuxpowWithChainId(auxblock1["hash"], reversedTarget, "98", True)
    res = self.nodes[0].submitauxblock(auxblock1["hash"], apow)
    assert res

    chaintips = self.nodes[0].getchaintips()
    tipsFound = 0;
    for ct in chaintips:
      if ct["hash"] in [ auxblock1["hash"], auxblock2["hash"] ]:
        tipsFound += 1
    assert_equal(tipsFound, 2)

    # Solve the last p2pkh template after requesting a new auxblock - this fails
    self.nodes[0].createauxblock(dummy_p2pkh_addr)
    reversedTarget = auxpow.reverseHex(auxblock3["target"])
    apow = auxpow.computeAuxpowWithChainId(auxblock3["hash"], reversedTarget, "98", True)
    try:
      self.nodes[0].submitauxblock(auxblock3["hash"], apow)
      raise AssertionError("Outdated blockhash accepted")
    except JSONRPCException as exc:
      assert_equal(exc.error["code"], -8)

    self.sync_all()

    # Call createauxblock while using the Namecoin API
    nmc_api_auxblock = self.nodes[1].createauxblock(dummy_p2pkh_addr)

    # must not contain a "target" field, but a "_target" field
    assert "target" not in nmc_api_auxblock
    assert "_target" in nmc_api_auxblock

    reversedTarget = auxpow.reverseHex(nmc_api_auxblock["_target"])
    apow = auxpow.computeAuxpowWithChainId(nmc_api_auxblock["hash"], reversedTarget, "98", True)
    res = self.nodes[1].submitauxblock(nmc_api_auxblock["hash"], apow)
    assert res

    # getting aux block light
    nmc_api_auxblock_light = self.nodes[1].createauxblocklight()
    assert nmc_api_auxblock_light is not None
    assert "job_id" in nmc_api_auxblock_light
    assert "version" in nmc_api_auxblock_light
    assert nmc_api_auxblock_light["version"] == 6422788

    # Build a coinbase with multiple payout outputs, mine the light
    # block ourselves, and submit it through submitauxblocklight.
    payout_addr1 = self.nodes[1].getnewaddress()
    payout_addr2 = self.nodes[1].getnewaddress()
    payout_addr3 = dummy_p2sh_addr
    script1 = self.nodes[1].validateaddress(payout_addr1)["scriptPubKey"]
    script2 = self.nodes[1].validateaddress(payout_addr2)["scriptPubKey"]
    script3 = self.nodes[1].validateaddress(payout_addr3)["scriptPubKey"]

    coinbase_value = nmc_api_auxblock_light["coinbasevalue"]
    value1 = coinbase_value // 3
    value2 = coinbase_value // 3
    value3 = coinbase_value - value1 - value2

    coinbase_tx = self.build_light_coinbase(
      nmc_api_auxblock_light["height"],
      [(value1, script1), (value2, script2), (value3, script3)])
    coinbase_txid = auxpow.doubleHashHex(coinbase_tx)

    # Fold the merkle branch supplied by createauxblocklight onto the
    # coinbase txid to get the final merkle root, exactly as a Stratum
    # client would (it never sees the other transactions themselves).
    merkle_root = coinbase_txid
    for step in nmc_api_auxblock_light["merkle"]:
      combined = auxpow.reverseHex(merkle_root) + auxpow.reverseHex(step)
      merkle_root = auxpow.doubleHashHex(combined)

    header = auxpow.reverseHex("%08x" % nmc_api_auxblock_light["version"])
    header += auxpow.reverseHex(nmc_api_auxblock_light["previousblockhash"])
    header += auxpow.reverseHex(merkle_root)
    header += auxpow.reverseHex("%08x" % nmc_api_auxblock_light["curtime"])
    header += auxpow.reverseHex(nmc_api_auxblock_light["bits"])
    header += "00000000"  # nonce; irrelevant, PoW is done on the parent chain
    light_block_hash = auxpow.doubleHashHex(header)

    reversedTarget = auxpow.reverseHex(nmc_api_auxblock_light["_target"])
    light_apow = auxpow.computeAuxpowWithChainId(light_block_hash, reversedTarget, "98", True)

    res = self.nodes[1].submitauxblocklight(nmc_api_auxblock_light["job_id"], coinbase_tx, light_apow)
    assert res

    self.sync_all()

    # Verify the block was accepted at the expected hash/height and that
    # the coinbase paid all three outputs correctly.
    height = self.nodes[1].getblockcount()
    assert_equal(height, nmc_api_auxblock_light["height"])
    tip_hash = self.nodes[1].getblockhash(height)
    assert_equal(tip_hash, light_block_hash)

    blk = self.nodes[1].getblock(tip_hash)
    assert "auxpow" in blk
    # node[1] has no -txindex; node[0] does, and both are synced.
    tx = self.nodes[0].getrawtransaction(blk["tx"][0], True)
    assert_equal(len(tx["vout"]), 3)
    assert_equal(tx["vout"][0]["value"], Decimal(value1) / Decimal(100000000))
    assert_equal(tx["vout"][1]["value"], Decimal(value2) / Decimal(100000000))
    assert_equal(tx["vout"][2]["value"], Decimal(value3) / Decimal(100000000))
    assert_equal(tx["vout"][0]["scriptPubKey"]["addresses"][0], payout_addr1)
    assert_equal(tx["vout"][1]["scriptPubKey"]["addresses"][0], payout_addr2)
    assert_equal(tx["vout"][2]["scriptPubKey"]["addresses"][0], payout_addr3)

    self.sync_all()

    # check the mined block
    self.check_mined_block(nmc_api_auxblock, apow, dummy_p2pkh_addr, Decimal("500000"))

  def build_light_coinbase(self, height, outputs):
    """
    Build a raw coinbase transaction (hex string) paying the given list
    of (value, scriptPubKey_hex) outputs, for use with
    submitauxblocklight.  Coinbase height-in-scriptSig isn't
    consensus-enforced here, so the scriptSig content just needs to be
    2-100 bytes.
    """
    def le_hex(value, num_bytes):
      return "".join("%02x" % ((value >> (8 * i)) & 0xff) for i in range(num_bytes))

    script_sig = "02" + le_hex(height, 2)
    vin = "01"
    vin += ("00" * 32) + ("ff" * 4)
    vin += ("%02x" % (len(script_sig) // 2)) + script_sig
    vin += ("ff" * 4)

    vout = "%02x" % len(outputs)
    for value, script in outputs:
      vout += le_hex(value, 8)
      vout += ("%02x" % (len(script) // 2)) + script

    return "01000000" + vin + vout + ("00" * 4)

  def check_mined_block(self, auxblock, apow, addr, min_value, txid=None):
    # Call getblock and verify the auxpow field.
    data = self.nodes[1].getblock(auxblock["hash"])
    assert "auxpow" in data
    auxJson = data["auxpow"]
    assert_equal(auxJson["index"], 0)
    assert_equal(auxJson["parentblock"], apow[-160:])

    # Call getrawtransaction and verify the coinbase tx
    coinbasetx = self.nodes[0].getrawtransaction(data["tx"][0], True)

    assert coinbasetx["vout"][0]["value"] >= min_value
    assert_equal(coinbasetx["vout"][0]["scriptPubKey"]["addresses"][0], addr)

    # Make sure the coinbase contains the block height
    coinbase = coinbasetx["vin"][0]["coinbase"]
    assert_equal("01%02x01" % auxblock["height"], coinbase[0:6])

    # Make sure our transaction got mined, if any
    if not txid is None:
      assert txid in data["tx"]

if __name__ == "__main__":
  CreateAuxBlockTest().main()
