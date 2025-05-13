// Copyright (c) 2025 The Dogecoin Core developers
// Distributed under the MIT/X11 software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "amount.h"
#include "consensus/merkle.cpp"
#include "primitives/block.h"
#include "primitives/transaction.h"
#include "rpc/auxcache.h"
#include "script/script.h"
#include "utiltime.h"

#include "test/test_bitcoin.h"

#include <boost/test/unit_test.hpp>
#include <memory>

BOOST_FIXTURE_TEST_SUITE(auxcache_tests, TestingSetup)

static const std::string cb_pk_1 = "03c758272b121a3e50a1a6a25aad800a45af51486227bb8f06257df80a15120135";
static const std::string cb_pk_2 = "020c163123e1e3b8bcf9114e2df152b5aa0bc5f69458991236746be48adce96bed";

// creates a bare dummy block with auxpow on
std::shared_ptr<CBlock> CreateDummyBlock(CScript scriptPubKey, CAmount amount) {
  CMutableTransaction coinbase{};
  coinbase.nVersion = 1;
  coinbase.vin.push_back(CTxIn(uint256::ZERO, 0));
  coinbase.vout.push_back(CTxOut(amount, scriptPubKey));

  CBlock block{};
  block.nVersion = 4;
  block.hashPrevBlock = uint256::ZERO;
  block.nTime = GetTime();
  block.nBits = 0x1e0ffff0;
  block.nNonce = 0;
  block.SetChainId(98);
  block.SetAuxpowFlag(true);

  block.vtx.resize(1);
  block.vtx[0] = MakeTransactionRef(std::move(coinbase));

  block.hashMerkleRoot = BlockMerkleRoot(block);

  return std::make_shared<CBlock>(block);
}


BOOST_AUTO_TEST_CASE(check_auxpow) {
  CAuxBlockCache cache;
  std::shared_ptr<CBlock> cached_block;
  bool res;

  CScript cb_script_1 = CScript() << ParseHex(cb_pk_1) << OP_CHECKSIG;
  CScriptID scriptId_1(cb_script_1);
  std::shared_ptr<CBlock> created_block_1 = CreateDummyBlock(cb_script_1, CAmount(69));
  uint256 blockhash_1 = created_block_1->GetHash();

  // add to cache
  res = cache.Add(scriptId_1, created_block_1);
  BOOST_CHECK(res);

  // get by scriptId
  res = cache.Get(scriptId_1, cached_block);
  BOOST_CHECK(res);
  BOOST_CHECK_EQUAL(blockhash_1.GetHex(), cached_block->GetHash().GetHex());

  // get by hash
  res = cache.Get(blockhash_1, cached_block);
  BOOST_CHECK(res);
  BOOST_CHECK_EQUAL(blockhash_1.GetHex(), cached_block->GetHash().GetHex());

  // Adding the same block again fails
  res = cache.Add(scriptId_1, created_block_1);
  BOOST_CHECK(!res);

  CScript cb_script_2 = CScript() << ParseHex(cb_pk_2) << OP_CHECKSIG;
  CScriptID scriptId_2(cb_script_2);
  std::shared_ptr<CBlock> created_block_2 = CreateDummyBlock(cb_script_2, CAmount(169));
  uint256 blockhash_2 = created_block_2->GetHash();

  // Make sure the block hashes are different
  BOOST_CHECK_NE(blockhash_1.GetHex(), blockhash_2.GetHex());

  // add second block to cache
  res = cache.Add(scriptId_2, created_block_2);
  BOOST_CHECK(res);

  // get second block by scriptId
  res = cache.Get(scriptId_2, cached_block);
  BOOST_CHECK(res);
  BOOST_CHECK_EQUAL(blockhash_2.GetHex(), cached_block->GetHash().GetHex());

  // get second block by hash
  res = cache.Get(blockhash_2, cached_block);
  BOOST_CHECK(res);
  BOOST_CHECK_EQUAL(blockhash_2.GetHex(), cached_block->GetHash().GetHex());

  // get first block by scriptId
  res = cache.Get(scriptId_1, cached_block);
  BOOST_CHECK(res);
  BOOST_CHECK_EQUAL(blockhash_1.GetHex(), cached_block->GetHash().GetHex());

  // NOTE: since not all OS' have high precision time, we mock +1 second here
  SetMockTime(GetTime() + 1LL);

  // create another block with the first scriptId
  std::shared_ptr<CBlock> created_block_3 = CreateDummyBlock(cb_script_1, CAmount(269));
  uint256 blockhash_3 = created_block_3->GetHash();

  // Make sure the block hashes are different
  BOOST_CHECK_NE(blockhash_1.GetHex(), blockhash_3.GetHex());

  // add third block to cache
  res = cache.Add(scriptId_1, created_block_3);
  BOOST_CHECK(res);

  // get third block by scriptId
  res = cache.Get(scriptId_1, cached_block);
  BOOST_CHECK(res);
  BOOST_CHECK_EQUAL(blockhash_3.GetHex(), cached_block->GetHash().GetHex());

  // the first block is still available by hash
  res = cache.Get(blockhash_1, cached_block);
  BOOST_CHECK(res);
  BOOST_CHECK_EQUAL(blockhash_1.GetHex(), cached_block->GetHash().GetHex());

  // clearing the cache removes all data
  cache.Reset();
  res = cache.Get(scriptId_1, cached_block);
  BOOST_CHECK(!res);
  BOOST_CHECK(!cached_block);
  res = cache.Get(scriptId_2, cached_block);
  BOOST_CHECK(!res);
  BOOST_CHECK(!cached_block);
  res = cache.Get(blockhash_1, cached_block);
  BOOST_CHECK(!res);
  BOOST_CHECK(!cached_block);
  res = cache.Get(blockhash_2, cached_block);
  BOOST_CHECK(!res);
  BOOST_CHECK(!cached_block);
  res = cache.Get(blockhash_3, cached_block);
  BOOST_CHECK(!res);
  BOOST_CHECK(!cached_block);

  // Unmock time
  SetMockTime(0LL);

}

BOOST_AUTO_TEST_SUITE_END()
