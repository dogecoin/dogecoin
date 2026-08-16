// Copyright (c) 2026 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "chainparams.h"
#include "pow.h"
#include "validation.h"

#include "test/test_bitcoin.h"

#include <boost/test/unit_test.hpp>

static CBlockHeader MineRegtestHeader(const CBlockIndex* pindexPrev)
{
    const CChainParams& chainparams = Params();
    const int nHeight = pindexPrev->nHeight + 1;
    const Consensus::Params& consensus = chainparams.GetConsensus(nHeight);

    CBlockHeader header;
    header.hashPrevBlock = pindexPrev->GetBlockHash();
    header.nVersion = 4;
    header.nTime = pindexPrev->GetBlockTime() + consensus.nPowTargetSpacing;
    header.nBits = GetNextWorkRequired(pindexPrev, &header, consensus);
    header.nNonce = 0;

    CBlock block;
    block.nVersion = header.nVersion;
    block.hashPrevBlock = header.hashPrevBlock;
    block.nTime = header.nTime;
    block.nBits = header.nBits;
    block.nNonce = header.nNonce;
    while (!CheckProofOfWork(block.GetPoWHash(), block.nBits, consensus)) {
        ++header.nNonce;
        block.nNonce = header.nNonce;
    }
    return header;
}

BOOST_FIXTURE_TEST_SUITE(headers_dos_tests, TestChain240Setup)

BOOST_AUTO_TEST_CASE(low_work_sidefork_header_limit)
{
    const CChainParams& chainparams = Params();
    const CBlockIndex* pindexForkParent = chainActive.Tip()->pprev;
    BOOST_REQUIRE(pindexForkParent != nullptr);

    for (unsigned int i = 0; i < MAX_LOW_WORK_SIDEFORK_HEADERS; ++i) {
        CBlockHeader header = MineRegtestHeader(pindexForkParent);
        CValidationState state;
        const CBlockIndex* pindex = nullptr;
        BOOST_CHECK(ProcessNewBlockHeaders({header}, state, chainparams, &pindex));
        BOOST_REQUIRE(pindex != nullptr);
        LOCK(cs_main);
        BOOST_CHECK(IsLowWorkSideForkIndex(pindex));
    }

    {
        CBlockHeader header = MineRegtestHeader(pindexForkParent);
        CValidationState state;
        const CBlockIndex* pindex = nullptr;
        BOOST_CHECK(!ProcessNewBlockHeaders({header}, state, chainparams, &pindex));
    }

    LOCK(cs_main);
    BOOST_CHECK_EQUAL(GetLowWorkSideForkHeaderCount(), MAX_LOW_WORK_SIDEFORK_HEADERS);
}

BOOST_AUTO_TEST_CASE(main_chain_header_still_accepted)
{
    const CChainParams& chainparams = Params();
    CBlockHeader header = MineRegtestHeader(chainActive.Tip());
    CValidationState state;
    const CBlockIndex* pindex = nullptr;
    BOOST_CHECK(ProcessNewBlockHeaders({header}, state, chainparams, &pindex));
    BOOST_REQUIRE(pindex != nullptr);
    LOCK(cs_main);
    BOOST_CHECK(!IsLowWorkSideForkIndex(pindex));
}

BOOST_AUTO_TEST_SUITE_END()
