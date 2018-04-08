#!/usr/bin/env python3
# Copyright (c) 2014-2016 Daniel Kraft
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

# General code for auxpow testing.  This includes routines to
# solve an auxpow and to generate auxpow blocks.

import binascii
import hashlib

def computeAuxpow (block, target, ok):
  """
  Build an auxpow object (serialised as hex string) that solves
  (ok = True) or doesn't solve (ok = False) the block.
  """

  block = bytes (block, "ascii")

  # Start by building the merge-mining coinbase.  The merkle tree
  # consists only of the block hash as root.
  coinbase = b"fabe" + binascii.hexlify (b"m" * 2)
  coinbase += block
  coinbase += b"01000000" + (b"00" * 4)

  # Construct "vector" of transaction inputs.
  vin = b"01"
  vin += (b"00" * 32) + (b"ff" * 4)
  vin += bytes ("%02x" % (len (coinbase) / 2), "ascii") + coinbase
  vin += (b"ff" * 4)

  # Build up the full coinbase transaction.  It consists only
  # of the input and has no outputs.
  tx = b"01000000" + vin + b"00" + (b"00" * 4)
  txHash = doubleHashHex (tx)

  # Construct the parent block header.  It need not be valid, just good
  # enough for auxpow purposes.
  header = b"01000000"
  header += b"00" * 32
  header += reverseHex (txHash)
  header += b"00" * 4
  header += b"00" * 4
  header += b"00" * 4

  # Mine the block.
  (header, blockhash) = mineBlock (header, target, ok)

  # Build the MerkleTx part of the auxpow.
  auxpow = tx
  auxpow += blockhash
  auxpow += b"00"
  auxpow += b"00" * 4

  # Extend to full auxpow.
  auxpow += b"00"
  auxpow += b"00" * 4
  auxpow += header

  return auxpow.decode ("ascii")

def mineAuxpowBlock (node):
  """
  Mine an auxpow block on the given RPC connection.  This uses the
  createauxblock and submitauxblock command pair.
  """

  def create ():
    addr = node.getnewaddress ()
    return node.createauxblock (addr)

  return mineAuxpowBlockWithMethods (create, node.submitauxblock)

def mineAuxpowBlockWithMethods (create, submit):
  """
  Mine an auxpow block, using the given methods for creation and submission.
  """

  auxblock = create ()
  target = reverseHex (auxblock['_target'])
  apow = computeAuxpow (auxblock['hash'], target, True)
  res = submit (auxblock['hash'], apow)
  assert res

  return auxblock['hash']

def getCoinbaseAddr (node, blockHash):
    """
    Extract the coinbase tx' payout address for the given block.
    """

    blockData = node.getblock (blockHash)
    txn = blockData['tx']
    assert len (txn) >= 1

    txData = node.getrawtransaction (txn[0], 1)
    assert len (txData['vout']) >= 1 and len (txData['vin']) == 1
    assert 'coinbase' in txData['vin'][0]

    addr = txData['vout'][0]['scriptPubKey']['addresses']
    assert len (addr) == 1
    return addr[0]

def mineBlock (header, target, ok):
  """
  Given a block header, update the nonce until it is ok (or not)
  for the given target.
  """

  data = bytearray (binascii.unhexlify (header))
  while True:
    assert data[79] < 255
    data[79] += 1
    hexData = binascii.hexlify (data)

    blockhash = doubleHashHex (hexData)
    if (ok and blockhash < target) or ((not ok) and blockhash > target):
      break

  return (hexData, blockhash)

def doubleHashHex (data):
  """
  Perform Bitcoin's Double-SHA256 hash on the given hex string.
  """

  hasher = hashlib.sha256 ()
  hasher.update (binascii.unhexlify (data))
  data = hasher.digest ()

  hasher = hashlib.sha256 ()
  hasher.update (data)

  return reverseHex (hasher.hexdigest ())

def reverseHex (data):
  """
  Flip byte order in the given data (hex string).
  """

  b = bytearray (binascii.unhexlify (data))
  b.reverse ()

  return binascii.hexlify (b)
