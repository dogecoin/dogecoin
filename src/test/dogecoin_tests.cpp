// Copyright (c) 2015-2017 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "arith_uint256.h"
#include "chainparams.h"
#include "dogecoin.h"
#include "test/test_bitcoin.h"

#include <boost/test/unit_test.hpp>

BOOST_FIXTURE_TEST_SUITE(dogecoin_tests, TestingSetup)

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
    const auto chainParams = CreateChainParams(CBaseChainParams::MAIN);
    const auto params = chainParams->GetConsensus();
    CAmount nSum = 0;
    uint256 prevHash = uint256S("0");

    for (nHeight = 0; nHeight <= 100000; nHeight++) {
        CAmount nSubsidy = GetDogecoinBlockSubsidy(nHeight, params, prevHash);
        BOOST_CHECK(MoneyRange(nSubsidy));
        BOOST_CHECK(nSubsidy <= 1000000 * COIN);
        nSum += nSubsidy * nStepSize;
    }
    for (; nHeight <= 145000; nHeight++) {
        CAmount nSubsidy = GetDogecoinBlockSubsidy(nHeight, params, prevHash);
        BOOST_CHECK(MoneyRange(nSubsidy));
        BOOST_CHECK(nSubsidy <= 500000 * COIN);
        nSum += nSubsidy * nStepSize;
    }
    for (; nHeight < 600000; nHeight++) {
        CAmount nSubsidy = GetDogecoinBlockSubsidy(nHeight, params, prevHash);
        CAmount nExpectedSubsidy = (500000 >> (nHeight / 100000)) * COIN;
        BOOST_CHECK(MoneyRange(nSubsidy));
        BOOST_CHECK(nSubsidy == nExpectedSubsidy);
        nSum += nSubsidy * nStepSize;
    }

    //test sum +- ~10billion
    arith_uint256 upperlimit = arith_uint256("95e14ec776380000"); //108 billion doge
    BOOST_CHECK(nSum <= upperlimit);

    arith_uint256 lowerlimit = arith_uint256("7a1fe16027700000"); //88 billion doge
    BOOST_CHECK(nSum >= lowerlimit);

    // Test reward at 600k+ is constant
    CAmount nConstantSubsidy = GetDogecoinBlockSubsidy(600000, params, prevHash);
    BOOST_CHECK(nConstantSubsidy == 10000 * COIN);

    nConstantSubsidy = GetDogecoinBlockSubsidy(700000, params, prevHash);
    BOOST_CHECK(nConstantSubsidy == 10000 * COIN);
}

BOOST_AUTO_TEST_SUITE_END()
