// Copyright (c) 2026 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "chainparams.h"
#include "pow.h"
#include "validation.h"

#include "test/test_bitcoin.h"

#include <vector>

#include <boost/test/unit_test.hpp>

// Mine a valid regtest header building on pindexPrev.
//
// nSeq lets callers create many *distinct* headers off the same parent: each
// distinct nTime yields a distinct block hash. This is required to exercise the
// low-work side-fork accounting with a large number of competing headers.
static CBlockHeader MineRegtestHeader(const CBlockIndex* pindexPrev, unsigned int nSeq = 0)
{
    const CChainParams& chainparams = Params();
    const int nHeight = pindexPrev->nHeight + 1;
    const Consensus::Params& consensus = chainparams.GetConsensus(nHeight);

    CBlockHeader header;
    // Regtest enforces the auxpow chain ID for non-legacy blocks, so encode it.
    // A bare nVersion such as 4 has chain ID 0 and would be rejected by
    // CheckAuxPowProofOfWork().
    header.SetBaseVersion(4, consensus.nAuxpowChainId);
    header.hashPrevBlock = pindexPrev->GetBlockHash();
    header.nTime = pindexPrev->GetBlockTime() + consensus.nPowTargetSpacing + nSeq;
    header.nBits = GetNextWorkRequired(pindexPrev, &header, consensus);
    header.nNonce = 0;
    while (!CheckProofOfWork(header.GetPoWHash(), header.nBits, consensus))
        ++header.nNonce;
    return header;
}

BOOST_FIXTURE_TEST_SUITE(headers_validity_tests, TestChain240Setup)

// Competing headers rooted a few blocks below the active tip are low-work side
// forks: they are accepted (and counted) until the global cap is reached, and
// rejected with a DoS score afterwards.
BOOST_AUTO_TEST_CASE(low_work_sidefork_header_limit)
{
    const CChainParams& chainparams = Params();

    // Fork from an ancestor several blocks below the tip so every sibling we
    // mine has strictly less chain work than the tip and is therefore a
    // low-work side fork. A direct sibling of the tip would have *equal* work
    // and count as a useful extension, not a side fork.
    const CBlockIndex* pindexForkParent = chainActive.Tip()->GetAncestor(chainActive.Height() - 10);
    BOOST_REQUIRE(pindexForkParent != nullptr);

    for (unsigned int i = 0; i < MAX_LOW_WORK_SIDEFORK_HEADERS; ++i) {
        CBlockHeader header = MineRegtestHeader(pindexForkParent, i);
        CValidationState state;
        const CBlockIndex* pindex = nullptr;
        unsigned int nNewSideFork = 0;
        BOOST_CHECK(ProcessNewBlockHeaders({header}, state, chainparams, &pindex, &nNewSideFork));
        BOOST_REQUIRE(pindex != nullptr);
        BOOST_CHECK_EQUAL(nNewSideFork, 1u);
        LOCK(cs_main);
        BOOST_CHECK(IsLowWorkSideForkIndex(pindex));
    }

    {
        LOCK(cs_main);
        BOOST_CHECK_EQUAL(GetLowWorkSideForkHeaderCount(), MAX_LOW_WORK_SIDEFORK_HEADERS);
    }

    // One past the cap: the next distinct competing header is rejected with a
    // DoS score and does not count as an accepted side fork.
    CBlockHeader header = MineRegtestHeader(pindexForkParent, MAX_LOW_WORK_SIDEFORK_HEADERS);
    CValidationState state;
    const CBlockIndex* pindex = nullptr;
    unsigned int nNewSideFork = 0;
    BOOST_CHECK(!ProcessNewBlockHeaders({header}, state, chainparams, &pindex, &nNewSideFork));
    BOOST_CHECK_EQUAL(nNewSideFork, 0u);
    int nDoS = 0;
    BOOST_CHECK(state.IsInvalid(nDoS));
    BOOST_CHECK(nDoS > 0);

    {
        LOCK(cs_main);
        BOOST_CHECK_EQUAL(GetLowWorkSideForkHeaderCount(), MAX_LOW_WORK_SIDEFORK_HEADERS);
    }
}

// Every side-fork header delivered in a single headers message must be counted,
// not just the last one. This is the accounting a caller uses to score a peer
// per header rather than per message.
BOOST_AUTO_TEST_CASE(sidefork_batch_is_fully_counted)
{
    const CChainParams& chainparams = Params();
    const CBlockIndex* pindexForkParent = chainActive.Tip()->GetAncestor(chainActive.Height() - 10);
    BOOST_REQUIRE(pindexForkParent != nullptr);

    std::vector<CBlockHeader> headers;
    const unsigned int nBatch = 50;
    for (unsigned int i = 0; i < nBatch; ++i)
        headers.push_back(MineRegtestHeader(pindexForkParent, i));

    CValidationState state;
    const CBlockIndex* pindex = nullptr;
    unsigned int nNewSideFork = 0;
    BOOST_CHECK(ProcessNewBlockHeaders(headers, state, chainparams, &pindex, &nNewSideFork));
    BOOST_CHECK_EQUAL(nNewSideFork, nBatch);

    LOCK(cs_main);
    BOOST_CHECK_EQUAL(GetLowWorkSideForkHeaderCount(), nBatch);
}

// Headers that extend the active tip are never treated as low-work side forks.
BOOST_AUTO_TEST_CASE(main_chain_header_still_accepted)
{
    const CChainParams& chainparams = Params();
    CBlockHeader header = MineRegtestHeader(chainActive.Tip());
    CValidationState state;
    const CBlockIndex* pindex = nullptr;
    unsigned int nNewSideFork = 0;
    BOOST_CHECK(ProcessNewBlockHeaders({header}, state, chainparams, &pindex, &nNewSideFork));
    BOOST_REQUIRE(pindex != nullptr);
    BOOST_CHECK_EQUAL(nNewSideFork, 0u);
    LOCK(cs_main);
    BOOST_CHECK(!IsLowWorkSideForkIndex(pindex));
}

// The global side-fork count must shrink when the tip disconnects back past
// previously flagged entries. Otherwise a long-running node could fill the
// cap with stale headers and never recover without a restart.
BOOST_AUTO_TEST_CASE(sidefork_count_decreases_when_tip_retreats)
{
    const CChainParams& chainparams = Params();
    const int nForkHeight = chainActive.Height() - 10;
    const CBlockIndex* pindexForkParent = chainActive.Tip()->GetAncestor(nForkHeight);
    BOOST_REQUIRE(pindexForkParent != nullptr);

    const unsigned int nBatch = 8;
    for (unsigned int i = 0; i < nBatch; ++i) {
        CBlockHeader header = MineRegtestHeader(pindexForkParent, i);
        CValidationState state;
        const CBlockIndex* pindex = nullptr;
        unsigned int nNewSideFork = 0;
        BOOST_CHECK(ProcessNewBlockHeaders({header}, state, chainparams, &pindex, &nNewSideFork));
        BOOST_CHECK_EQUAL(nNewSideFork, 1u);
    }

    {
        LOCK(cs_main);
        BOOST_CHECK_EQUAL(GetLowWorkSideForkHeaderCount(), nBatch);
    }

    // Disconnect the last 10 active-chain blocks so the fork parent becomes
    // the tip. Those competing headers now extend the tip and must drop out
    // of the side-fork set.
    {
        LOCK(cs_main);
        CBlockIndex* pindexInvalidate = chainActive.Tip()->GetAncestor(nForkHeight + 1);
        BOOST_REQUIRE(pindexInvalidate != nullptr);
        CValidationState state;
        BOOST_CHECK(InvalidateBlock(state, chainparams, pindexInvalidate));
        BOOST_CHECK_EQUAL(chainActive.Height(), nForkHeight);
        BOOST_CHECK_EQUAL(GetLowWorkSideForkHeaderCount(), 0u);
    }
}

BOOST_AUTO_TEST_SUITE_END()
