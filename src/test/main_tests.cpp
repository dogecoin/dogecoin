#include "core.h"
#include "main.h"
#include "uint256.h"

#include <boost/test/unit_test.hpp>

BOOST_AUTO_TEST_SUITE(main_tests)

/**
 * the maximum block reward at a given height for a block without fees
 */
uint64_t expectedMaxSubsidy(int height) {
    if(height < 100000) {
        return 1000000 * COIN;
    } else if (height < 150000) {
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
    if(height < 100000) {
        return 0;
    } else if (height < 150000) {
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
    uint256 nSum = 0;

    for (nHeight = 0; nHeight <= 600000; nHeight += nStepSize) {
        uint64_t nSubsidy = GetBlockValue(nHeight, 0, 0);
        BOOST_CHECK(MoneyRange(nSubsidy));
        BOOST_CHECK(nSubsidy >= expectedMinSubsidy(nHeight));
        BOOST_CHECK(nSubsidy <= expectedMaxSubsidy(nHeight));
        
        nSum += nSubsidy * nStepSize;
    }

    //test sum +- ~10billion
    uint256 upperlimit = uint256("95e14ec776380000"); //108 billion doge
    BOOST_CHECK(nSum <= upperlimit);
    
    uint256 lowerlimit = uint256("7a1fe16027700000"); //88 billion doge
    BOOST_CHECK(nSum >= lowerlimit);
    
    //test infinitely increasing block rewards
    for (; nHeight < 700000; nHeight += nStepSize) {
        uint64_t nSubsidy = GetBlockValue(nHeight, 0, 0);
        BOOST_CHECK(MoneyRange(nSubsidy));
        BOOST_CHECK(nSubsidy >= expectedMinSubsidy(nHeight));
        BOOST_CHECK(nSubsidy <= expectedMaxSubsidy(nHeight));
        
        nSum += nSubsidy * nStepSize;
    }
    
    //make sure that MAX_MONEY is not what some people think it is
    BOOST_CHECK(nSum > MAX_MONEY);
    
    //test subsidy in 1000 years
    nHeight = 1000 * 365 * 24 * 60;
    uint64_t nSubsidy = GetBlockValue(nHeight, 0, 0);
    BOOST_CHECK(MoneyRange(nSubsidy));
    BOOST_CHECK(nSubsidy >= expectedMinSubsidy(nHeight));
    BOOST_CHECK(nSubsidy <= expectedMaxSubsidy(nHeight));
}

BOOST_AUTO_TEST_SUITE_END()
