// Copyright (c) 2015 The Bitcoin Core developers
// Copyright (c) 2018-2022 The Dogecoin Core developers
// Distributed under the MIT/X11 software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "chain.h"
#include "chainparams.h"
#include "dogecoin.h"
#include "pow.h"
#include "primitives/block.h"
#include "random.h"
#include "util.h"
#include "test/test_bitcoin.h"

#include <boost/test/unit_test.hpp>

BOOST_FIXTURE_TEST_SUITE(pow_tests, BasicTestingSetup)

/* Test calculation of next difficulty target with no constraints applying */
BOOST_AUTO_TEST_CASE(get_next_work)
{
    SelectParams(CBaseChainParams::MAIN);
    const Consensus::Params& params = Params().GetConsensus(0);

    int64_t nLastRetargetTime = 1388149872; // Block #30240
    CBlockIndex pindexLast;
    pindexLast.nHeight = 30479;
    pindexLast.nTime = 1388163922; // Block #30479
    pindexLast.nBits = 0x1c00974f;
    BOOST_CHECK_EQUAL(CalculateNextWorkRequired(&pindexLast, nLastRetargetTime, params), 0x1c0093a1);
}

/* Test the constraint on the upper bound for next work */
BOOST_AUTO_TEST_CASE(get_next_work_pow_limit)
{
    SelectParams(CBaseChainParams::MAIN);
    const Consensus::Params& params = Params().GetConsensus(0);

    int64_t nLastRetargetTime = 1231006505; // Block #0
    CBlockIndex pindexLast;
    pindexLast.nHeight = 2015;
    pindexLast.nTime = 1233061996;  // Block #2015
    pindexLast.nBits = 0x1d00ffff;
    BOOST_CHECK_EQUAL(CalculateNextWorkRequired(&pindexLast, nLastRetargetTime, params), 0x1d03fffc);
}

/* Test the constraint on the lower bound for actual time taken */
BOOST_AUTO_TEST_CASE(get_next_work_lower_limit_actual)
{
    SelectParams(CBaseChainParams::MAIN);
    const Consensus::Params& params = Params().GetConsensus(0);

    int64_t nLastRetargetTime = 1279008237; // Block #66528
    CBlockIndex pindexLast;
    pindexLast.nHeight = 66767;
    pindexLast.nTime = 1279008237 + (239 * 60 / 4 - 1); // Bitcoin Block #66528 + less than a quarter of the target timespan
    pindexLast.nBits = 0x1c05a3f4;
    BOOST_CHECK_EQUAL(CalculateNextWorkRequired(&pindexLast, nLastRetargetTime, params), 0x1c0168fd);
}

/* Test the constraint on the upper bound for actual time taken */
// Replaced by Dogecoin-specific PoW test
/* BOOST_AUTO_TEST_CASE(get_next_work_upper_limit_actual)
{
    SelectParams(CBaseChainParams::MAIN);
    const Consensus::Params& params = Params().GetConsensus(0);

    int64_t nLastRetargetTime = 1263163443; // NOTE: Not an actual block time
    CBlockIndex pindexLast;
    pindexLast.nHeight = 46367;
    pindexLast.nTime = 1269211443;  // Block #46367
    pindexLast.nBits = 0x1c387f6f;
    BOOST_CHECK_EQUAL(CalculateNextWorkRequired(&pindexLast, nLastRetargetTime, params), 0x1d00e1fd);
} */

BOOST_AUTO_TEST_CASE(GetBlockProofEquivalentTime_test)
{
    SelectParams(CBaseChainParams::MAIN);
    const Consensus::Params& params = Params().GetConsensus(0);

    std::vector<CBlockIndex> blocks(10000);
    for (int i = 0; i < 10000; i++) {
        blocks[i].pprev = i ? &blocks[i - 1] : NULL;
        blocks[i].nHeight = i;
        blocks[i].nTime = 1269211443 + i * params.nPowTargetSpacing;
        blocks[i].nBits = 0x207fffff; /* target 0x7fffff000... */
        blocks[i].nChainWork = i ? blocks[i - 1].nChainWork + GetBlockProof(blocks[i - 1]) : arith_uint256(0);
    }

    for (int j = 0; j < 1000; j++) {
        CBlockIndex *p1 = &blocks[InsecureRandRange(10000)];
        CBlockIndex *p2 = &blocks[InsecureRandRange(10000)];
        CBlockIndex *p3 = &blocks[InsecureRandRange(10000)];

        int64_t tdiff = GetBlockProofEquivalentTime(*p1, *p2, *p3, params);
        BOOST_CHECK_EQUAL(tdiff, p1->GetBlockTime() - p2->GetBlockTime());
    }
}

/** Build a linear chain of CBlockIndex entries for min-difficulty tests (constant nTime -> stable MTP). */
static CBlockIndex* MakeStubChain(std::vector<CBlockIndex>& blocks, int tipHeight, int64_t tipTime, unsigned int nBitsTip)
{
    const int depth = 20;
    blocks.resize(depth);
    for (int i = 0; i < depth; i++) {
        blocks[i].pprev = (i > 0) ? &blocks[i - 1] : nullptr;
        blocks[i].nHeight = tipHeight - (depth - 1 - i);
        blocks[i].nTime = static_cast<unsigned int>(tipTime);
        blocks[i].nBits = (i == depth - 1) ? nBitsTip : 0x1e0ffff0;
    }
    return &blocks.back();
}

BOOST_AUTO_TEST_CASE(strict_min_difficulty_rejects_consecutive_pow_limit)
{
    Consensus::Params params{};
    params.powLimit = uint256S("0x00000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffff");
    params.fPowAllowMinDifficultyBlocks = true;
    params.fPowAllowDigishieldMinDifficultyBlocks = true;
    params.nPowTargetSpacing = 60;
    params.fEnforceStrictMinDifficulty = true;

    std::vector<CBlockIndex> chain;
    const unsigned int nPowLimitBits = UintToArith256(params.powLimit).GetCompact();
    CBlockIndex* pindexLast = MakeStubChain(chain, 200000, 1000000, nPowLimitBits);

    CBlockHeader hdr;
    hdr.nTime = pindexLast->GetBlockTime() + 600 + 1;

    BOOST_CHECK(!AllowDigishieldMinDifficultyForBlock(pindexLast, &hdr, params));
}

BOOST_AUTO_TEST_CASE(strict_min_difficulty_accepts_after_gap)
{
    Consensus::Params params{};
    params.powLimit = uint256S("0x00000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffff");
    params.fPowAllowMinDifficultyBlocks = true;
    params.fPowAllowDigishieldMinDifficultyBlocks = true;
    params.nPowTargetSpacing = 60;
    params.fEnforceStrictMinDifficulty = true;

    std::vector<CBlockIndex> chain;
    CBlockIndex* pindexLast = MakeStubChain(chain, 200000, 1000000, 0x1c05a3f4);

    CBlockHeader hdr;
    hdr.nTime = pindexLast->GetBlockTime() + 600 + 1;

    BOOST_CHECK(AllowDigishieldMinDifficultyForBlock(pindexLast, &hdr, params));
}

BOOST_AUTO_TEST_CASE(strict_min_difficulty_rejects_if_not_past_mtp_threshold)
{
    Consensus::Params params{};
    params.powLimit = uint256S("0x00000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffff");
    params.fPowAllowMinDifficultyBlocks = true;
    params.fPowAllowDigishieldMinDifficultyBlocks = true;
    params.nPowTargetSpacing = 60;
    params.fEnforceStrictMinDifficulty = true;

    std::vector<CBlockIndex> chain;
    CBlockIndex* pindexLast = MakeStubChain(chain, 200000, 1000000, 0x1c05a3f4);

    CBlockHeader hdr;
    hdr.nTime = pindexLast->GetMedianTimePast() + 10 * params.nPowTargetSpacing;

    BOOST_CHECK(!AllowDigishieldMinDifficultyForBlock(pindexLast, &hdr, params));
}

BOOST_AUTO_TEST_CASE(legacy_min_difficulty_unchanged_when_strict_off)
{
    Consensus::Params params{};
    params.powLimit = uint256S("0x00000fffffffffffffffffffffffffffffffffffffffffffffffffffffffffff");
    params.fPowAllowMinDifficultyBlocks = true;
    params.fPowAllowDigishieldMinDifficultyBlocks = true;
    params.nPowTargetSpacing = 60;
    params.fEnforceStrictMinDifficulty = false;

    std::vector<CBlockIndex> chain;
    const unsigned int nPowLimitBits = UintToArith256(params.powLimit).GetCompact();
    CBlockIndex* pindexLast = MakeStubChain(chain, 200000, 1000000, nPowLimitBits);

    CBlockHeader hdr;
    hdr.nTime = pindexLast->GetBlockTime() + 2 * params.nPowTargetSpacing + 1;

    BOOST_CHECK(AllowDigishieldMinDifficultyForBlock(pindexLast, &hdr, params));
}

BOOST_AUTO_TEST_SUITE_END()
