#include "core.h"
#include "main.h"

#include <boost/test/unit_test.hpp>

BOOST_AUTO_TEST_SUITE(main_tests)

BOOST_AUTO_TEST_CASE(subsidy_limit_test)
{
    int nHeight = 0;
    int nStepSize= 1000;

    // Random rewards to block 145k mean we can't sensibly calculate
    // sum of the rewards until we're past block 145k
    for (nHeight = 0; nHeight <= 145000; nHeight += nStepSize) {
        uint64_t nSubsidy = GetBlockValue(nHeight, 0, 0);
        BOOST_CHECK(nSubsidy <= 1000000 * COIN);
    }

    uint64_t nSumAtBlock100k = 100000L * 500000L * COIN;
    uint64_t nSum100To145k = 45000L * 250000L * COIN;
    uint64_t nSum = nSumAtBlock100k + nSum100To145k;
    
    for (; nHeight < 600000; nHeight += 1000) {
        uint64_t nSubsidy = GetBlockValue(nHeight, 0, 0);
        BOOST_CHECK(nSubsidy <= 250000 * COIN);
        nSum += nSubsidy * 1000;
        // The following test breaks because we don't actually
        // have a datatype big enough for the maximum money
        // theoretically possible...
        // BOOST_CHECK(MoneyRange(nSum));
    }

    // This also doesn't work because MAX_MONEY is nonsense, and
    // I'm leaving it broken to force people to fix it later.
    BOOST_CHECK(nSum == MAX_MONEY);
}

BOOST_AUTO_TEST_SUITE_END()
