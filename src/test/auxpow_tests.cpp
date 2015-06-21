// Copyright (c) 2014-2015 Daniel Kraft
// Distributed under the MIT/X11 software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "auxpow.h"
#include "chainparams.h"
#include "coins.h"
#include "core.h"
#include "main.h"
#include "uint256.h"
#include "script.h"

#include <boost/test/unit_test.hpp>

#include <algorithm>
#include <vector>

typedef std::vector<unsigned char> valtype;

unsigned char pchTestMergedMiningHeader[] = { 0xfa, 0xbe, 'm', 'm' } ;

BOOST_AUTO_TEST_SUITE (auxpow_tests)

/* ************************************************************************** */

/**
 * Temporary method to call the current CheckProofOfWork method until later Bitcoin Core changes are merged in.
 */
static bool
CheckProofOfWork(const CBlockHeader& block, const CChainParams& params) {
  return block.CheckProofOfWork(params.GetAuxPowStartBlock() + 1);
}

/**
 * Temporary method until later Bitcoin Core changes are merged in.
 */
template <typename T>
std::vector<unsigned char> ToByteVector(const T& in)
{
    return std::vector<unsigned char>(in.begin(), in.end());
}

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
   * of the header, aux hash, the merkle tree size and the nonce.
   * @param hashAux The aux merkle root hash.
   * @param merkleHeight Height of the merkle tree.
   * @param nonce The nonce value to use.
   * @return The constructed data.
   */
  static valtype
  buildCoinbaseData (const valtype& auxRoot, const unsigned merkleHeight, const int nonce);

  /**
   * Build a data vector to be included in the coinbase, without the header.
   * It consists of the aux hash, the merkle tree size and the nonce.
   * @param hashAux The aux merkle root hash.
   * @param merkleHeight Height of the merkle tree.
   * @param nonce The nonce value to use.
   * @return The constructed data.
   */
  static valtype
  buildHeaderlessCoinbaseData(const valtype& auxRoot, const unsigned merkleHeight, const int nonce);
};

CAuxpowBuilder::CAuxpowBuilder (int baseVersion, int chainId)
  : auxpowChainIndex(-1)
{
  parentBlock.nVersion = baseVersion | (chainId * BLOCK_VERSION_CHAIN_START);
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
  LOCK(cs_main);
  CAuxPow res(tx);
  res.SetMerkleBranch (&parentBlock);

  res.vChainMerkleBranch = auxpowChainMerkleBranch;
  res.nChainIndex = auxpowChainIndex;
  res.parentBlockHeader = parentBlock;

  return res;
}

valtype
CAuxpowBuilder::buildCoinbaseData (const valtype& auxRoot, const unsigned merkleHeight, const int nonce)
{
  valtype res = buildHeaderlessCoinbaseData(auxRoot, merkleHeight, nonce);

  res.insert (res.begin(), UBEGIN (pchTestMergedMiningHeader),
              UEND (pchTestMergedMiningHeader));
  return res;
}

valtype
CAuxpowBuilder::buildHeaderlessCoinbaseData(const valtype& auxRoot, const unsigned merkleHeight, const int nonce)
{
  valtype res;

  res.insert (res.end (), auxRoot.begin (), auxRoot.end ());

  const int size = (1 << merkleHeight);
  res.insert (res.end (), UBEGIN (size), UEND (size));
  res.insert (res.end (), UBEGIN (nonce), UEND (nonce));

  return res;
}

/* ************************************************************************** */

BOOST_AUTO_TEST_CASE (check_auxpow)
{
  const CChainParams& params = Params ();
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
  data = CAuxpowBuilder::buildCoinbaseData (auxRoot, height, nonce);
  scr = (CScript () << 2809 << 2013) + COINBASE_FLAGS;
  scr = (scr << OP_2 << data);
  builder.setCoinbase (scr);
  BOOST_CHECK (builder.get ().check (hashAux, ourChainId, params));

  /* Check that the auxpow is invalid if we change either the aux block's
     hash or the chain ID.  */
  BOOST_CHECK (!builder.get ().check (hashAux + 1, ourChainId, params));
  BOOST_CHECK (!builder.get ().check (hashAux, ourChainId + 1, params));

  /* Non-coinbase parent tx should fail.  Note that we can't just copy
     the coinbase literally, as we have to get a tx with different hash.  */
  const CTransaction oldCoinbase = builder.parentBlock.vtx[0];
  builder.setCoinbase (scr << 5);
  builder.parentBlock.vtx.push_back (oldCoinbase);
  builder.parentBlock.hashMerkleRoot = builder.parentBlock.BuildMerkleTree ();
  auxpow = builder.get (builder.parentBlock.vtx[0]);
  BOOST_CHECK (auxpow.check (hashAux, ourChainId, params));
  auxpow = builder.get (builder.parentBlock.vtx[1]);
  BOOST_CHECK (!auxpow.check (hashAux, ourChainId, params));

  /* The parent chain can't have the same chain ID.  */
  CAuxpowBuilder builder2(builder);
  builder2.parentBlock.nVersion = 2 | (100 * BLOCK_VERSION_CHAIN_START);
  BOOST_CHECK (builder2.get ().check (hashAux, ourChainId, params));
  builder2.parentBlock.nVersion = 2 | (ourChainId * BLOCK_VERSION_CHAIN_START);
  BOOST_CHECK (!builder2.get ().check (hashAux, ourChainId, params));

  /* Disallow too long merkle branches.  */
  builder2 = builder;
  index = CAuxPow::getExpectedIndex (nonce, ourChainId, height + 1);
  auxRoot = builder2.buildAuxpowChain (hashAux, height + 1, index);
  data = CAuxpowBuilder::buildCoinbaseData (auxRoot, height + 1, nonce);
  scr = (CScript () << 2809 << 2013) + COINBASE_FLAGS;
  scr = (scr << OP_2 << data);
  builder2.setCoinbase (scr);
  BOOST_CHECK (!builder2.get ().check (hashAux, ourChainId, params));

  /* Verify that we compare correctly to the parent block's merkle root.  */
  builder2 = builder;
  BOOST_CHECK (builder2.get ().check (hashAux, ourChainId, params));
  builder2.parentBlock.hashMerkleRoot = 1234;
  BOOST_CHECK (!builder2.get ().check (hashAux, ourChainId, params));

  /* Build a non-header legacy version and check that it is rejected (these are not supported in Dogecoin).  */
  builder2 = builder;
  index = CAuxPow::getExpectedIndex (nonce, ourChainId, height);
  auxRoot = builder2.buildAuxpowChain (hashAux, height, index);
  data = CAuxpowBuilder::buildHeaderlessCoinbaseData (auxRoot, height, nonce);
  scr = (CScript () << 2809 << 2013) + COINBASE_FLAGS;
  scr = (scr << OP_2 << data);
  builder2.setCoinbase (scr);
  BOOST_CHECK (!builder2.get ().check (hashAux, ourChainId, params));

  /* Verify two merged-mine headers are rejected */

  valtype data2
    = CAuxpowBuilder::buildCoinbaseData (auxRoot, height, nonce);

  data2.insert (data2.begin(), UBEGIN (pchTestMergedMiningHeader),
              UEND (pchTestMergedMiningHeader));

  builder2.setCoinbase (CScript () << data2);
  BOOST_CHECK (!builder2.get ().check (hashAux, ourChainId, params));

  /* Verify that the appended nonce/size values are checked correctly.  */

  data = CAuxpowBuilder::buildCoinbaseData (auxRoot, height, nonce);
  builder2.setCoinbase (CScript () << data);
  BOOST_CHECK (builder2.get ().check (hashAux, ourChainId, params));

  data.pop_back ();
  builder2.setCoinbase (CScript () << data);
  BOOST_CHECK (!builder2.get ().check (hashAux, ourChainId, params));

  data = CAuxpowBuilder::buildCoinbaseData (auxRoot, height - 1, nonce);
  builder2.setCoinbase (CScript () << data);
  BOOST_CHECK (!builder2.get ().check (hashAux, ourChainId, params));

  data = CAuxpowBuilder::buildCoinbaseData (auxRoot, height, nonce + 3);
  builder2.setCoinbase (CScript () << data);
  BOOST_CHECK (!builder2.get ().check (hashAux, ourChainId, params));

  /* Put the aux hash in an invalid merkle tree position.  */

  auxRoot = builder.buildAuxpowChain (hashAux, height, index + 1);
  data = CAuxpowBuilder::buildCoinbaseData (auxRoot, height, nonce);
  builder2.setCoinbase (CScript () << data);
  BOOST_CHECK (!builder2.get ().check (hashAux, ourChainId, params));

  auxRoot = builder.buildAuxpowChain (hashAux, height, index);
  data = CAuxpowBuilder::buildCoinbaseData (auxRoot, height, nonce);
  builder2.setCoinbase (CScript () << data);
  BOOST_CHECK (builder2.get ().check (hashAux, ourChainId, params));
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
  /* Use regtest parameters to allow mining with easy difficulty.  */
  SelectParams (CBaseChainParams::REGTEST);
  const CChainParams& params = Params();

  const uint256 target = (~uint256(0) >> 1);
  // params.setProofOfWorkLimit (target);
  CBlockHeader block;
  block.nBits = target.GetCompact ();

  /* Verify the block version checks.  */

  block.nVersion = 1;
  mineBlock (block, true);
  BOOST_CHECK (CheckProofOfWork (block, params));

  /* Block versions without chain ID should be rejected */
  block.nVersion = 2;
  mineBlock (block, true);
  // FIXME: BOOST_CHECK (!CheckProofOfWork (block, params));

  block.nVersion = 2 | (params.AuxpowChainId() * BLOCK_VERSION_CHAIN_START);
  mineBlock (block, true);
  // FIXME: BOOST_CHECK (CheckProofOfWork (block, params));

  /* Block versions with the wrong chain ID should be rejected */
  block.nVersion = 2 | ((params.AuxpowChainId() + 1) * BLOCK_VERSION_CHAIN_START); 
  mineBlock (block, true);
  // FIXME: BOOST_CHECK (!CheckProofOfWork (block, params));

  /* Check the case when the block does not have auxpow (this is true
     right now).  */

  block.nVersion = block.nVersion | BLOCK_VERSION_AUXPOW;
  mineBlock (block, true);
  // FIXME: BOOST_CHECK (!CheckProofOfWork (block, params));

  block.nVersion = block.nVersion & (!BLOCK_VERSION_AUXPOW);
  mineBlock (block, true);
  // FIXME: BOOST_CHECK (CheckProofOfWork (block, params));
  mineBlock (block, false);
  // FIXME: BOOST_CHECK (!CheckProofOfWork (block, params));

  /* ****************************************** */
  /* Check the case that the block has auxpow.  */

  CAuxpowBuilder builder(5, 42);
  CAuxPow auxpow;
  const int32_t ourChainId = params.AuxpowChainId();
  const unsigned height = 3;
  const int nonce = 7;
  const int index = CAuxPow::getExpectedIndex (nonce, ourChainId, height);
  valtype auxRoot, data;

  /* Valid auxpow, PoW check of parent block.  */
 
  block.nVersion = block.nVersion | BLOCK_VERSION_AUXPOW; 
  auxRoot = builder.buildAuxpowChain (block.GetHash (), height, index);
  data = CAuxpowBuilder::buildCoinbaseData (auxRoot, height, nonce);
  builder.setCoinbase (CScript () << data);
  mineBlock (builder.parentBlock, false, block.nBits);
  block.SetAuxpow (new CAuxPow (builder.get ()));
  BOOST_CHECK (!CheckProofOfWork (block, params));
  mineBlock (builder.parentBlock, true, block.nBits);
  block.SetAuxpow (new CAuxPow (builder.get ()));
  // FIXME: BOOST_CHECK (CheckProofOfWork (block, params));

  /* Mismatch between auxpow being present and block.nVersion.  Note that
     block.SetAuxpow sets also the version and that we want to ensure
     that the block hash itself doesn't change due to version changes.
     This requires some work arounds.  */
  // TODO: Re-enable once CBlockVersion implemented
  /* block.nVersion = block.nVersion & (!BLOCK_VERSION_AUXPOW);
  const uint256 hashAux = block.GetHash ();
  auxRoot = builder.buildAuxpowChain (hashAux, height, index);
  data = CAuxpowBuilder::buildCoinbaseData (auxRoot, height, nonce);
  builder.setCoinbase (CScript () << data);
  mineBlock (builder.parentBlock, true, block.nBits);
  block.SetAuxpow (new CAuxPow (builder.get ()));
  BOOST_CHECK (hashAux != block.GetHash ());
  block.nVersion = block.nVersion & (!BLOCK_VERSION_AUXPOW);
  BOOST_CHECK (hashAux == block.GetHash ());
  BOOST_CHECK (!CheckProofOfWork (block, params)); */

  /* Modifying the block invalidates the PoW.  */
  block.nVersion = block.nVersion | BLOCK_VERSION_AUXPOW;

  auxRoot = builder.buildAuxpowChain (block.GetHash (), height, index);
  data = CAuxpowBuilder::buildCoinbaseData (auxRoot, height, nonce);
  builder.setCoinbase (CScript () << data);
  mineBlock (builder.parentBlock, true, block.nBits);
  block.SetAuxpow (new CAuxPow (builder.get ()));
  // FIXME: BOOST_CHECK (CheckProofOfWork (block, params));
  block.hashMerkleRoot += 1;
  BOOST_CHECK (!CheckProofOfWork (block, params));
}

/* ************************************************************************** */

BOOST_AUTO_TEST_CASE (auxpow_pow_block_version)
{
  SelectParams (CBaseChainParams::REGTEST);
  const CChainParams& params = Params();

  BOOST_CHECK(!IsAuxPowVersion(1, params));
  BOOST_CHECK(!IsAuxPowVersion(2, params));
  BOOST_CHECK(IsAuxPowVersion(2 + (BLOCK_VERSION_CHAIN_START * params.AuxpowChainId()), params));
}

BOOST_AUTO_TEST_SUITE_END ()
