#!/usr/bin/env python
# Copyright (c) 2014 Daniel Kraft
# Copyright (c) 2015-2022 The Dogecoin Core developers
# Distributed under the MIT software license, see the accompanying
# file COPYING or http://www.opensource.org/licenses/mit-license.php.

# General code for scrypt auxpow testing.  This includes routines to
# solve an auxpow header and to generate auxpow blocks with scrypt.
# extends and modifies auxpow module by Daniel Kraft.

# This module requires a built and installed version of the ltc_scrypt
# package, which can be downloaded from:
# https://pypi.python.org/packages/source/l/ltc_scrypt/ltc_scrypt-1.0.tar.gz

from .auxpow import *
import ltc_scrypt
import binascii

def computeAuxpowWithChainId (block, target, chainid, ok):
  """
  Build an auxpow object (serialised as hex string) that solves the
  block, for a given chain id.
  """

  # Start by building the merge-mining coinbase.  The merkle tree
  # consists only of the block hash as root.
  coinbase = "fabe" + binascii.hexlify((b"m" * 2)).decode("ascii")
  coinbase += block
  coinbase += "01000000" + ("00" * 4)

  # Construct "vector" of transaction inputs.
  vin = "01"
  vin += ("00" * 32) + ("ff" * 4)
  vin += ("%02x" % int(len (coinbase) / 2)) + coinbase
  vin += ("ff" * 4)

  # Build up the full coinbase transaction.  It consists only
  # of the input and has no outputs.
  tx = "01000000" + vin + "00" + ("00" * 4)
  txHash = doubleHashHex (tx)

  # Construct the parent block header.  It need not be valid, just good
  # enough for auxpow purposes.
  header = "0100" + chainid + "00"
  header += "00" * 32
  header += reverseHex (txHash)
  header += "00" * 4
  header += "00" * 4
  header += "00" * 4

  # Mine the block.
  (header, blockhash) = mineScryptBlock (header, target, ok)

  # Build the MerkleTx part of the auxpow.
  output = tx
  output += blockhash
  output += "00"
  output += "00" * 4

  # Extend to full auxpow.
  output += "00"
  output += "00" * 4
  output += header

  return output

# for now, just offer hashes to rpc until it matches the work we need
def mineScryptAux (node, chainid, ok):
  """
  Mine an auxpow block on the given RPC connection.
  """

  auxblock = node.getauxblock ()
  target = reverseHex (auxblock['target'])

  apow = computeAuxpowWithChainId (auxblock['hash'], target, chainid, ok)
  res = node.getauxblock (auxblock['hash'], apow)
  return res

def mineScryptBlock (header, target, ok):
  """
  Given a block header, update the nonce until it is ok (or not)
  for the given target.
  """

  data = bytearray (binascii.unhexlify(header))
  while True:
    assert data[79] < 255
    data[79] += 1
    hexData = binascii.hexlify(data).decode("ascii")

    scrypt = getScryptPoW(hexData)
    if (ok and scrypt < target) or ((not ok) and scrypt > target):
      break

  blockhash = doubleHashHex (hexData)
  return (hexData, blockhash)

def getScryptPoW(hexData):
  """
  Actual scrypt pow calculation
  """

  data = binascii.unhexlify(hexData)

  return reverseHex(binascii.hexlify(ltc_scrypt.getPoWHash(data)).decode("ascii"))
