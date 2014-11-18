// Copyright (c) 2014 Daniel Kraft
// Distributed under the MIT/X11 software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "auxpow.h"
#include "chainparams.h"
#include "coins.h"
#include "main.h"
#include "uint256.h"

#include "core/block.h"

#include "script/script.h"

#include <boost/test/unit_test.hpp>

#include <algorithm>
#include <vector>

BOOST_AUTO_TEST_SUITE (auxpow_tests)

/* ************************************************************************** */

/**
 * Utility class to construct auxpow's and manipulate them.  This is used
 * to simulate various scenarios.
 */
class CAuxpowBuilder
{
public:

  /** The parent block (with coinbase, not just header).  */
  CBlock parentBlock;

  /** The auxpow's merkle branch (connecting it to the coinbase).  */
  std::vector<uint256> auxpowChainMerkleBranch;
  /** The auxpow's merkle tree index.  */
  int auxpowChainIndex;

  /**
   * Initialise everything.
   * @param baseVersion The parent block's base version to use.
   * @param chainId The parent block's chain ID to use.
   */
  CAuxpowBuilder (int baseVersion, int chainId);

  /**
   * Set the coinbase's script.
   * @param scr Set it to this script.
   */
  void setCoinbase (const CScript& scr);

  /**
   * Build the auxpow merkle branch.  The member variables will be
   * set accordingly.  This has to be done before constructing the coinbase
   * itself (which must contain the root merkle hash).  When we have the
   * coinbase afterwards, the member variables can be used to initialise
   * the CAuxPow object from it.
   * @param hashAux The merge-mined chain's block hash.
   * @param h Height of the merkle tree to build.
   * @param index Index to use in the merkle tree.
   * @return The root hash, with reversed endian.
   */
  valtype buildAuxpowChain (const uint256& hashAux, unsigned h, int index);

  /**
   * Build the finished CAuxPow object.  We assume that the auxpowChain
   * member variables are already set.  We use the passed in transaction
   * as the base.  It should (probably) be the parent block's coinbase.
   * @param tx The base tx to use.
   * @return The constructed CAuxPow object.
   */
  CAuxPow get (const CTransaction& tx) const;

  /**
   * Build the finished CAuxPow object from the parent block's coinbase.
   * @return The constructed CAuxPow object.
   */
  inline CAuxPow
  get () const
  {
    assert (!parentBlock.vtx.empty ());
    return get (parentBlock.vtx[0]);
  }

  /**
   * Build a data vector to be included in the coinbase.  It consists
   * of the aux hash, the merkle tree size and the nonce.  Optionally,
   * the header can be added as well.
   * @param header Add the header?
   * @param hashAux The aux merkle root hash.
   * @param h Height of the merkle tree.
   * @param nonce The nonce value to use.
   * @return The constructed data.
   */
  static valtype buildCoinbaseData (bool header, const valtype& auxRoot,
                                    unsigned h, int nonce);

};

CAuxpowBuilder::CAuxpowBuilder (int baseVersion, int chainId)
  : auxpowChainIndex(-1)
{
  parentBlock.nVersion.SetBaseVersion(baseVersion);
  parentBlock.nVersion.SetChainId(chainId);
}

void
CAuxpowBuilder::setCoinbase (const CScript& scr)
{
  CMutableTransaction mtx;
  mtx.vin.resize (1);
  mtx.vin[0].prevout.SetNull ();
  mtx.vin[0].scriptSig = scr;

  parentBlock.vtx.clear ();
  parentBlock.vtx.push_back (mtx);
  parentBlock.hashMerkleRoot = parentBlock.BuildMerkleTree ();
}

valtype
CAuxpowBuilder::buildAuxpowChain (const uint256& hashAux, unsigned h, int index)
{
  auxpowChainIndex = index;

  /* Just use "something" for the branch.  Doesn't really matter.  */
  auxpowChainMerkleBranch.clear ();
  for (unsigned i = 0; i < h; ++i)
    auxpowChainMerkleBranch.push_back (uint256 (i));

  const uint256 hash
    = CBlock::CheckMerkleBranch (hashAux, auxpowChainMerkleBranch, index);

  valtype res = ToByteVector (hash);
  std::reverse (res.begin (), res.end ());

  return res;
}

CAuxPow
CAuxpowBuilder::get (const CTransaction& tx) const
{
  CAuxPow res(tx);
  res.SetMerkleBranch (parentBlock);

  res.vChainMerkleBranch = auxpowChainMerkleBranch;
  res.nChainIndex = auxpowChainIndex;
  res.parentBlock = parentBlock;

  return res;
}

valtype
CAuxpowBuilder::buildCoinbaseData (bool header, const valtype& auxRoot,
                                   unsigned h, int nonce)
{
  valtype res;

  if (header)
    res.insert (res.end (), UBEGIN (pchMergedMiningHeader),
                UEND (pchMergedMiningHeader));
  res.insert (res.end (), auxRoot.begin (), auxRoot.end ());

  const int size = (1 << h);
  res.insert (res.end (), UBEGIN (size), UEND (size));
  res.insert (res.end (), UBEGIN (nonce), UEND (nonce));

  return res;
}

/* ************************************************************************** */

BOOST_AUTO_TEST_CASE (cblockversion)
{
  /* Simple check to make sure that CBlockVersion behaves like the int32_t
     that backs it.  This is necessary because of the way CBlockHeader::GetHash
     calculates the hash.  */

  CBlockVersion nVersion;
  const int32_t nVersionRaw = 0x12345678;
  nVersion.SetGenesisVersion (nVersionRaw);

  BOOST_CHECK (sizeof (nVersion) == sizeof (nVersionRaw));
  BOOST_CHECK (std::equal (BEGIN (nVersion), END (nVersion),
                           BEGIN (nVersionRaw)));
}

/* ************************************************************************** */

BOOST_AUTO_TEST_CASE (check_auxpow)
{
  CAuxpowBuilder builder(5, 42);
  CAuxPow auxpow;

  const uint256 hashAux(12345);
  const int ourChainId = Params ().AuxpowChainId ();
  const unsigned height = 30;
  const int nonce = 7;
  int index;

  valtype auxRoot, data;
  CScript scr;

  /* Build a correct auxpow.  The height is the maximally allowed one.  */
  index = CAuxPow::getExpectedIndex (nonce, ourChainId, height);
  auxRoot = builder.buildAuxpowChain (hashAux, height, index);
  data = CAuxpowBuilder::buildCoinbaseData (true, auxRoot, height, nonce);
  scr = (CScript () << 2809 << 2013) + COINBASE_FLAGS;
  scr = (scr << OP_2 << data);
  builder.setCoinbase (scr);
  BOOST_CHECK (builder.get ().check (hashAux, ourChainId));

  /* Check that the auxpow is invalid if we change either the aux block's
     hash or the chain ID.  */
  BOOST_CHECK (!builder.get ().check (hashAux + 1, ourChainId));
  BOOST_CHECK (!builder.get ().check (hashAux, ourChainId + 1));

  /* Non-coinbase parent tx should fail.  Note that we can't just copy
     the coinbase literally, as we have to get a tx with different hash.  */
  const CTransaction oldCoinbase = builder.parentBlock.vtx[0];
  builder.setCoinbase (scr << 5);
  builder.parentBlock.vtx.push_back (oldCoinbase);
  builder.parentBlock.hashMerkleRoot = builder.parentBlock.BuildMerkleTree ();
  auxpow = builder.get (builder.parentBlock.vtx[0]);
  BOOST_CHECK (auxpow.check (hashAux, ourChainId));
  auxpow = builder.get (builder.parentBlock.vtx[1]);
  BOOST_CHECK (!auxpow.check (hashAux, ourChainId));

  /* The parent chain can't have the same chain ID.  */
  CAuxpowBuilder builder2(builder);
  builder2.parentBlock.nVersion.SetChainId (100);
  BOOST_CHECK (builder2.get ().check (hashAux, ourChainId));
  builder2.parentBlock.nVersion.SetChainId (ourChainId);
  BOOST_CHECK (!builder2.get ().check (hashAux, ourChainId));

  /* Disallow too long merkle branches.  */
  builder2 = builder;
  index = CAuxPow::getExpectedIndex (nonce, ourChainId, height + 1);
  auxRoot = builder2.buildAuxpowChain (hashAux, height + 1, index);
  data = CAuxpowBuilder::buildCoinbaseData (true, auxRoot, height + 1, nonce);
  scr = (CScript () << 2809 << 2013) + COINBASE_FLAGS;
  scr = (scr << OP_2 << data);
  builder2.setCoinbase (scr);
  BOOST_CHECK (!builder2.get ().check (hashAux, ourChainId));

  /* Verify that we compare correctly to the parent block's merkle root.  */
  builder2 = builder;
  BOOST_CHECK (builder2.get ().check (hashAux, ourChainId));
  builder2.parentBlock.hashMerkleRoot = 1234;
  BOOST_CHECK (!builder2.get ().check (hashAux, ourChainId));

  /* Build a non-header legacy version and check that it is also accepted.  */
  builder2 = builder;
  index = CAuxPow::getExpectedIndex (nonce, ourChainId, height);
  auxRoot = builder2.buildAuxpowChain (hashAux, height, index);
  data = CAuxpowBuilder::buildCoinbaseData (false, auxRoot, height, nonce);
  scr = (CScript () << 2809 << 2013) + COINBASE_FLAGS;
  scr = (scr << OP_2 << data);
  builder2.setCoinbase (scr);
  BOOST_CHECK (builder2.get ().check (hashAux, ourChainId));

  /* However, various attempts at smuggling two roots in should be detected.  */

  const valtype wrongAuxRoot
    = builder2.buildAuxpowChain (hashAux + 1, height, index);
  valtype data2
    = CAuxpowBuilder::buildCoinbaseData (false, wrongAuxRoot, height, nonce);
  builder2.setCoinbase (CScript () << data << data2);
  BOOST_CHECK (builder2.get ().check (hashAux, ourChainId));
  builder2.setCoinbase (CScript () << data2 << data);
  BOOST_CHECK (!builder2.get ().check (hashAux, ourChainId));

  data2 = CAuxpowBuilder::buildCoinbaseData (true, wrongAuxRoot, height, nonce);
  builder2.setCoinbase (CScript () << data << data2);
  BOOST_CHECK (!builder2.get ().check (hashAux, ourChainId));
  builder2.setCoinbase (CScript () << data2 << data);
  BOOST_CHECK (!builder2.get ().check (hashAux, ourChainId));

  data = CAuxpowBuilder::buildCoinbaseData (true, auxRoot, height, nonce);
  builder2.setCoinbase (CScript () << data << data2);
  BOOST_CHECK (!builder2.get ().check (hashAux, ourChainId));
  builder2.setCoinbase (CScript () << data2 << data);
  BOOST_CHECK (!builder2.get ().check (hashAux, ourChainId));

  data2 = CAuxpowBuilder::buildCoinbaseData (false, wrongAuxRoot,
                                             height, nonce);
  builder2.setCoinbase (CScript () << data << data2);
  BOOST_CHECK (builder2.get ().check (hashAux, ourChainId));
  builder2.setCoinbase (CScript () << data2 << data);
  BOOST_CHECK (builder2.get ().check (hashAux, ourChainId));

  /* Verify that the appended nonce/size values are checked correctly.  */

  data = CAuxpowBuilder::buildCoinbaseData (true, auxRoot, height, nonce);
  builder2.setCoinbase (CScript () << data);
  BOOST_CHECK (builder2.get ().check (hashAux, ourChainId));

  data.pop_back ();
  builder2.setCoinbase (CScript () << data);
  BOOST_CHECK (!builder2.get ().check (hashAux, ourChainId));

  data = CAuxpowBuilder::buildCoinbaseData (true, auxRoot, height - 1, nonce);
  builder2.setCoinbase (CScript () << data);
  BOOST_CHECK (!builder2.get ().check (hashAux, ourChainId));

  data = CAuxpowBuilder::buildCoinbaseData (true, auxRoot, height, nonce + 3);
  builder2.setCoinbase (CScript () << data);
  BOOST_CHECK (!builder2.get ().check (hashAux, ourChainId));

  /* Put the aux hash in an invalid merkle tree position.  */

  auxRoot = builder.buildAuxpowChain (hashAux, height, index + 1);
  data = CAuxpowBuilder::buildCoinbaseData (true, auxRoot, height, nonce);
  builder2.setCoinbase (CScript () << data);
  BOOST_CHECK (!builder2.get ().check (hashAux, ourChainId));

  auxRoot = builder.buildAuxpowChain (hashAux, height, index);
  data = CAuxpowBuilder::buildCoinbaseData (true, auxRoot, height, nonce);
  builder2.setCoinbase (CScript () << data);
  BOOST_CHECK (builder2.get ().check (hashAux, ourChainId));
}

/* ************************************************************************** */

/**
 * Mine a block (assuming minimal difficulty) that either matches
 * or doesn't match the difficulty target specified in the block header.
 * @param block The block to mine (by updating nonce).
 * @param ok Whether the block should be ok for PoW.
 * @param nBits Use this as difficulty if specified.
 */
static void
mineBlock (CBlockHeader& block, bool ok, int nBits = -1)
{
  if (nBits == -1)
    nBits = block.nBits;

  uint256 target;
  target.SetCompact (nBits);

  block.nNonce = 0;
  while (true)
    {
      const bool nowOk = (block.GetHash () <= target);
      if ((ok && nowOk) || (!ok && !nowOk))
        break;

      ++block.nNonce;
    }

  if (ok)
    BOOST_CHECK (CheckProofOfWork (block.GetHash (), nBits));
  else
    BOOST_CHECK (!CheckProofOfWork (block.GetHash (), nBits));
}

BOOST_AUTO_TEST_CASE (auxpow_pow)
{
  const uint256 target(~uint256(0) >> 1);
  ModifiableParams ()->setProofOfWorkLimit (target);
  CBlockHeader block;
  block.nBits = target.GetCompact ();

  /* Verify the block version checks.  */

  block.nVersion.SetGenesisVersion (1);
  mineBlock (block, true);
  BOOST_CHECK (CheckProofOfWork (block));

  block.nVersion.SetGenesisVersion (2);
  mineBlock (block, true);
  BOOST_CHECK (!CheckProofOfWork (block));

  block.nVersion.SetBaseVersion (2);
  block.nVersion.SetChainId (Params ().AuxpowChainId ());
  mineBlock (block, true);
  BOOST_CHECK (CheckProofOfWork (block));

  block.nVersion.SetChainId (Params ().AuxpowChainId () + 1);
  mineBlock (block, true);
  BOOST_CHECK (!CheckProofOfWork (block));

  /* Check the case when the block does not have auxpow (this is true
     right now).  */

  block.nVersion.SetChainId (Params ().AuxpowChainId ());
  block.nVersion.SetAuxpow (true);
  mineBlock (block, true);
  BOOST_CHECK (!CheckProofOfWork (block));

  block.nVersion.SetAuxpow (false);
  mineBlock (block, true);
  BOOST_CHECK (CheckProofOfWork (block));
  mineBlock (block, false);
  BOOST_CHECK (!CheckProofOfWork (block));

  /* ****************************************** */
  /* Check the case that the block has auxpow.  */

  CAuxpowBuilder builder(5, 42);
  CAuxPow auxpow;
  const int ourChainId = Params ().AuxpowChainId ();
  const unsigned height = 3;
  const int nonce = 7;
  const int index = CAuxPow::getExpectedIndex (nonce, ourChainId, height);
  valtype auxRoot, data;

  /* Valid auxpow, PoW check of parent block.  */
  block.nVersion.SetAuxpow (true);
  auxRoot = builder.buildAuxpowChain (block.GetHash (), height, index);
  data = CAuxpowBuilder::buildCoinbaseData (true, auxRoot, height, nonce);
  builder.setCoinbase (CScript () << data);
  mineBlock (builder.parentBlock, false, block.nBits);
  block.SetAuxpow (new CAuxPow (builder.get ()));
  BOOST_CHECK (!CheckProofOfWork (block));
  mineBlock (builder.parentBlock, true, block.nBits);
  block.SetAuxpow (new CAuxPow (builder.get ()));
  BOOST_CHECK (CheckProofOfWork (block));

  /* Mismatch between auxpow being present and block.nVersion.  Note that
     block.SetAuxpow sets also the version and that we want to ensure
     that the block hash itself doesn't change due to version changes.
     This requires some work arounds.  */
  block.nVersion.SetAuxpow (false);
  const uint256 hashAux = block.GetHash ();
  auxRoot = builder.buildAuxpowChain (hashAux, height, index);
  data = CAuxpowBuilder::buildCoinbaseData (true, auxRoot, height, nonce);
  builder.setCoinbase (CScript () << data);
  mineBlock (builder.parentBlock, true, block.nBits);
  block.SetAuxpow (new CAuxPow (builder.get ()));
  BOOST_CHECK (hashAux != block.GetHash ());
  block.nVersion.SetAuxpow (false);
  BOOST_CHECK (hashAux == block.GetHash ());
  BOOST_CHECK (!CheckProofOfWork (block));

  /* Modifying the block invalidates the PoW.  */
  block.nVersion.SetAuxpow (true);
  auxRoot = builder.buildAuxpowChain (block.GetHash (), height, index);
  data = CAuxpowBuilder::buildCoinbaseData (true, auxRoot, height, nonce);
  builder.setCoinbase (CScript () << data);
  mineBlock (builder.parentBlock, true, block.nBits);
  block.SetAuxpow (new CAuxPow (builder.get ()));
  BOOST_CHECK (CheckProofOfWork (block));
  block.hashMerkleRoot += 1;
  BOOST_CHECK (!CheckProofOfWork (block));
}

/* ************************************************************************** */

BOOST_AUTO_TEST_SUITE_END ()
