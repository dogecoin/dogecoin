// Copyright (c) 2015 The Dogecoin Core developers
// Copyright (c) 2021 CoinGreen Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "arith_uint256.h"
#include "chainparams.h"
#include "coingreen.h"
#include "test/test_bitcoin.h"

#include <boost/test/unit_test.hpp>

BOOST_FIXTURE_TEST_SUITE(coingreen_tests, TestingSetup)

/**
 * the maximum block reward at a given height for a block without fees
 */
uint64_t expectedMaxSubsidy(int height) {
    if (height < 100000) {
        return 1000000 * COIN;
    } else if (height < 145000) {
        return 500000 * COIN;
    } else if (height < 200000) {
        return 250000 * COIN;
    } else if (height < 300000) {
        return 125000 * COIN;
    } else if (height < 400000) {
        return  62500 * COIN;
    } else if (height < 500000) {
        return  31250 * COIN;
    } else if (height < 600000) {
        return  15625 * COIN;
    } else {
        return  10000 * COIN;
    }
}

/**
 * the minimum possible value for the maximum block reward at a given height
 * for a block without fees
 */
uint64_t expectedMinSubsidy(int height) {
    if (height < 100000) {
        return 0;
    } else if (height < 145000) {
        return 0;
    } else if (height < 200000) {
        return 250000 * COIN;
    } else if (height < 300000) {
        return 125000 * COIN;
    } else if (height < 400000) {
        return  62500 * COIN;
    } else if (height < 500000) {
        return  31250 * COIN;
    } else if (height < 600000) {
        return  15625 * COIN;
    } else {
        return  10000 * COIN;
    }
}

BOOST_AUTO_TEST_CASE(subsidy_limit_test)
{
    int nHeight = 0;
    int nStepSize= 1;
    const CChainParams& mainParams = Params(CBaseChainParams::MAIN);
    CAmount nSum = 0;
    uint256 prevHash = uint256S("0");

    for (nHeight = 0; nHeight <= 100000; nHeight++) {
        const Consensus::Params& params = mainParams.GetConsensus(nHeight);
        CAmount nSubsidy = GetDogecoinBlockSubsidy(nHeight, params, prevHash);
        BOOST_CHECK(MoneyRange(nSubsidy));
        BOOST_CHECK(nSubsidy <= 1000000 * COIN);
        nSum += nSubsidy * nStepSize;
    }
    for (; nHeight <= 145000; nHeight++) {
        const Consensus::Params& params = mainParams.GetConsensus(nHeight);
        CAmount nSubsidy = GetDogecoinBlockSubsidy(nHeight, params, prevHash);
        BOOST_CHECK(MoneyRange(nSubsidy));
        BOOST_CHECK(nSubsidy <= 500000 * COIN);
        nSum += nSubsidy * nStepSize;
    }
    for (; nHeight < 600000; nHeight++) {
        const Consensus::Params& params = mainParams.GetConsensus(nHeight);
        CAmount nSubsidy = GetDogecoinBlockSubsidy(nHeight, params, prevHash);
        CAmount nExpectedSubsidy = (500000 >> (nHeight / 100000)) * COIN;
        BOOST_CHECK(MoneyRange(nSubsidy));
        BOOST_CHECK_EQUAL(nSubsidy, nExpectedSubsidy);
        nSum += nSubsidy * nStepSize;
    }

    //test sum +- ~10billion
    arith_uint256 upperlimit = arith_uint256("95e14ec776380000"); //108 billion doge
    BOOST_CHECK(nSum <= upperlimit);

    arith_uint256 lowerlimit = arith_uint256("7a1fe16027700000"); //88 billion doge
    BOOST_CHECK(nSum >= lowerlimit);

    // Test reward at 600k+ is constant
    const Consensus::Params& params = mainParams.GetConsensus(nHeight);
    CAmount nConstantSubsidy = GetDogecoinBlockSubsidy(nHeight, params, prevHash);
    BOOST_CHECK_EQUAL(nConstantSubsidy, 10000 * COIN);

    nConstantSubsidy = GetDogecoinBlockSubsidy(700000, params, prevHash);
    BOOST_CHECK_EQUAL(nConstantSubsidy, 10000 * COIN);
}

BOOST_AUTO_TEST_CASE(get_next_work_difficulty_limit)
{
    SelectParams(CBaseChainParams::MAIN);
    const Consensus::Params& params = Params().GetConsensus(0);

    CBlockIndex pindexLast;
    int64_t nLastRetargetTime = 1386474927; // Block # 1
    
    pindexLast.nHeight = 239;
    pindexLast.nTime = 1386475638; // Block #239
    pindexLast.nBits = 0x1e0ffff0;
    BOOST_CHECK_EQUAL(CalculateDogecoinNextWorkRequired(&pindexLast, nLastRetargetTime, params), 0x1e00ffff);
}

BOOST_AUTO_TEST_CASE(get_next_work_pre_digishield)
{
    SelectParams(CBaseChainParams::MAIN);
    const Consensus::Params& params = Params().GetConsensus(0);
    
    CBlockIndex pindexLast;
    int64_t nLastRetargetTime = 1386942008; // Block 9359

    pindexLast.nHeight = 9599;
    pindexLast.nTime = 1386954113;
    pindexLast.nBits = 0x1c1a1206;
    BOOST_CHECK_EQUAL(CalculateDogecoinNextWorkRequired(&pindexLast, nLastRetargetTime, params), 0x1c15ea59);
}

BOOST_AUTO_TEST_CASE(get_next_work_digishield)
{
    SelectParams(CBaseChainParams::MAIN);
    const Consensus::Params& params = Params().GetConsensus(145000);
    
    CBlockIndex pindexLast;
    int64_t nLastRetargetTime = 1395094427;

    // First hard-fork at 145,000, which applies to block 145,001 onwards
    pindexLast.nHeight = 145000;
    pindexLast.nTime = 1395094679;
    pindexLast.nBits = 0x1b499dfd;
    BOOST_CHECK_EQUAL(CalculateDogecoinNextWorkRequired(&pindexLast, nLastRetargetTime, params), 0x1b671062);
}

BOOST_AUTO_TEST_CASE(get_next_work_digishield_modulated_upper)
{
    SelectParams(CBaseChainParams::MAIN);
    const Consensus::Params& params = Params().GetConsensus(145000);
    
    CBlockIndex pindexLast;
    int64_t nLastRetargetTime = 1395100835;

    // Test the upper bound on modulated time using mainnet block #145,107
    pindexLast.nHeight = 145107;
    pindexLast.nTime = 1395101360;
    pindexLast.nBits = 0x1b3439cd;
    BOOST_CHECK_EQUAL(CalculateDogecoinNextWorkRequired(&pindexLast, nLastRetargetTime, params), 0x1b4e56b3);
}

BOOST_AUTO_TEST_CASE(get_next_work_digishield_modulated_lower)
{
    SelectParams(CBaseChainParams::MAIN);
    const Consensus::Params& params = Params().GetConsensus(145000);
    
    CBlockIndex pindexLast;
    int64_t nLastRetargetTime = 1395380517;

    // Test the lower bound on modulated time using mainnet block #149,423
    pindexLast.nHeight = 149423;
    pindexLast.nTime = 1395380447;
    pindexLast.nBits = 0x1b446f21;
    BOOST_CHECK_EQUAL(CalculateDogecoinNextWorkRequired(&pindexLast, nLastRetargetTime, params), 0x1b335358);
}

BOOST_AUTO_TEST_CASE(get_next_work_digishield_rounding)
{
    SelectParams(CBaseChainParams::MAIN);
    const Consensus::Params& params = Params().GetConsensus(145000);
    
    CBlockIndex pindexLast;
    int64_t nLastRetargetTime = 1395094679;

    // Test case for correct rounding of modulated time - this depends on
    // handling of integer division, and is not obvious from the code
    pindexLast.nHeight = 145001;
    pindexLast.nTime = 1395094727;
    pindexLast.nBits = 0x1b671062;
    BOOST_CHECK_EQUAL(CalculateDogecoinNextWorkRequired(&pindexLast, nLastRetargetTime, params), 0x1b6558a4);
}

BOOST_AUTO_TEST_CASE(hardfork_parameters)
{
    SelectParams(CBaseChainParams::MAIN);
    const Consensus::Params& initialParams = Params().GetConsensus(0);

    BOOST_CHECK_EQUAL(initialParams.nPowTargetTimespan, 14400);
    BOOST_CHECK_EQUAL(initialParams.fAllowLegacyBlocks, true);
    BOOST_CHECK_EQUAL(initialParams.fDigishieldDifficultyCalculation, false);

    const Consensus::Params& initialParamsEnd = Params().GetConsensus(144999);
    BOOST_CHECK_EQUAL(initialParamsEnd.nPowTargetTimespan, 14400);
    BOOST_CHECK_EQUAL(initialParamsEnd.fAllowLegacyBlocks, true);
    BOOST_CHECK_EQUAL(initialParamsEnd.fDigishieldDifficultyCalculation, false);

    const Consensus::Params& digishieldParams = Params().GetConsensus(145000);
    BOOST_CHECK_EQUAL(digishieldParams.nPowTargetTimespan, 60);
    BOOST_CHECK_EQUAL(digishieldParams.fAllowLegacyBlocks, true);
    BOOST_CHECK_EQUAL(digishieldParams.fDigishieldDifficultyCalculation, true);

    const Consensus::Params& digishieldParamsEnd = Params().GetConsensus(371336);
    BOOST_CHECK_EQUAL(digishieldParamsEnd.nPowTargetTimespan, 60);
    BOOST_CHECK_EQUAL(digishieldParamsEnd.fAllowLegacyBlocks, true);
    BOOST_CHECK_EQUAL(digishieldParamsEnd.fDigishieldDifficultyCalculation, true);

    const Consensus::Params& auxpowParams = Params().GetConsensus(371337);
    BOOST_CHECK_EQUAL(auxpowParams.nHeightEffective, 371337);
    BOOST_CHECK_EQUAL(auxpowParams.nPowTargetTimespan, 60);
    BOOST_CHECK_EQUAL(auxpowParams.fAllowLegacyBlocks, false);
    BOOST_CHECK_EQUAL(auxpowParams.fDigishieldDifficultyCalculation, true);

    const Consensus::Params& auxpowHighParams = Params().GetConsensus(700000); // Arbitrary point after last hard-fork
    BOOST_CHECK_EQUAL(auxpowHighParams.nPowTargetTimespan, 60);
    BOOST_CHECK_EQUAL(auxpowHighParams.fAllowLegacyBlocks, false);
    BOOST_CHECK_EQUAL(auxpowHighParams.fDigishieldDifficultyCalculation, true);
}

BOOST_AUTO_TEST_SUITE_END()
