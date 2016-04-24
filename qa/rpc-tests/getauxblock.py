#!/usr/bin/env python
# Copyright (c) 2014-2015 Daniel Kraft
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

# Test the "getauxblock" merge-mining RPC interface.

from test_framework.test_framework import BitcoinTestFramework
from test_framework.util import *
from test_framework.script import CScriptNum

from test_framework import auxpow
from test_framework import scrypt_auxpow

import binascii

class GetAuxBlockTest (BitcoinTestFramework):

  def run_test (self):
    BitcoinTestFramework.run_test (self)

    # Generate a block so that we are not "downloading blocks".
    self.nodes[0].generate (1)

    # We used to compare to getblocktemplate, but this call is gone
    # now completely for merge-mining.

    # Verify data that can be found in another way.
    auxblock = self.nodes[0].getauxblock ()
    assert_equal (auxblock['chainid'], 98)

    assert_equal (auxblock['height'], self.nodes[0].getblockcount () + 1)
    assert_equal (auxblock['previousblockhash'], self.nodes[0].getblockhash (auxblock['height'] - 1))

    # Calling again should give the same block.
    auxblock2 = self.nodes[0].getauxblock ()
    assert_equal (auxblock2, auxblock)

    # If we receive a new block, the old hash will be replaced.
    self.sync_all ()
    self.nodes[1].generate (1)
    self.sync_all ()
    auxblock2 = self.nodes[0].getauxblock ()
    assert auxblock['hash'] != auxblock2['hash']
    try:
      self.nodes[0].getauxblock (auxblock['hash'], "x")
      raise AssertionError ("invalid block hash accepted")
    except JSONRPCException as exc:
      assert_equal (exc.error['code'], -8)

    # Invalid format for auxpow.
    try:
      self.nodes[0].getauxblock (auxblock2['hash'], "x")
      raise AssertionError ("malformed auxpow accepted")
    except JSONRPCException as exc:
      assert_equal (exc.error['code'], -1)

    # Invalidate the block again, send a transaction and query for the
    # auxblock to solve that contains the transaction.
    self.nodes[0].generate (1)
    addr = self.nodes[1].getnewaddress ()
    txid = self.nodes[0].sendtoaddress (addr, 1)
    self.sync_all ()
    assert_equal (self.nodes[1].getrawmempool (), [txid])
    auxblock = self.nodes[0].getauxblock ()
    target = auxpow.reverseHex (auxblock['target'])

    # Compute invalid auxpow.
    apow = scrypt_auxpow.computeAuxpowWithChainId (auxblock['hash'], target, "01", False)
    res = self.nodes[0].getauxblock (auxblock['hash'], apow)
    assert res is False

    # Compute and submit valid auxpow.
    apow = scrypt_auxpow.computeAuxpowWithChainId (auxblock['hash'], target, "01", True)
    res = self.nodes[0].getauxblock (auxblock['hash'], apow)
    assert res is True

    # Make sure that the block is indeed accepted.
    self.sync_all ()
    assert_equal (self.nodes[1].getrawmempool (), [])
    height = self.nodes[1].getblockcount ()
    assert_equal (height, auxblock['height'])
    assert_equal (self.nodes[1].getblockhash (height), auxblock['hash'])

    # Call getblock and verify the auxpow field.
    data = self.nodes[1].getblock (auxblock['hash'])
    assert 'auxpow' in data
    auxJson = data['auxpow']
    assert_equal (auxJson['index'], 0)
    assert_equal (auxJson['parentblock'], apow[-160:])

    # Check that previous blocks don't have 'auxpow' in their getblock JSON.
    oldHash = self.nodes[1].getblockhash (19)
    data = self.nodes[1].getblock (oldHash)
    assert 'auxpow' not in data

    # Check that it paid correctly to the first node.
    t = self.nodes[0].listtransactions ("", 1)
    assert_equal (len (t), 1)
    t = t[0]
    assert_equal (t['category'], "immature")
    assert_equal (t['blockhash'], auxblock['hash'])
    assert t['generated']
    assert t['amount'] >= Decimal ("25")
    assert_equal (t['confirmations'], 1)

    # Verify the coinbase script.  Ensure that it includes the block height
    # to make the coinbase tx unique.
    blk = self.nodes[1].getblock (auxblock['hash'])
    tx = self.nodes[1].getrawtransaction (blk['tx'][0], 1)
    coinbase = tx['vin'][0]['coinbase']
    blockHeightHex = binascii.hexlify(CScriptNum.encode(CScriptNum(auxblock['height'])))
    assert_equal (blockHeightHex, coinbase[0 : len(blockHeightHex)])

if __name__ == '__main__':
  GetAuxBlockTest ().main ()
