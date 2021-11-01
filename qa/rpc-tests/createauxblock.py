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

    # If we receive a new block, the template cache must be emptied.
    self.sync_all()
    self.nodes[1].generate(1)
    self.sync_all()

    auxblock4 = self.nodes[0].createauxblock(dummy_p2pkh_addr)
    assert auxblock["hash"] != auxblock4["hash"]
    try:
      self.nodes[0].submitauxblock(auxblock["hash"], "x")
      raise AssertionError("invalid block hash accepted")
    except JSONRPCException as exc:
      assert_equal(exc.error["code"], -8)

    # Invalid format for auxpow.
    try:
      self.nodes[0].submitauxblock(auxblock4["hash"], "x")
      raise AssertionError("malformed auxpow accepted")
    except JSONRPCException as exc:
      assert_equal(exc.error["code"], -1)

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

    self.sync_all()

    # check the mined block
    self.check_mined_block(nmc_api_auxblock, apow, dummy_p2pkh_addr, Decimal("500000"))

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
