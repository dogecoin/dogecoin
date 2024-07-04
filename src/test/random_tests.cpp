// Copyright (c) 2017 The Bitcoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "random.h"

#include "test/test_bitcoin.h"

#include <boost/test/unit_test.hpp>

#include <random>
#include <algorithm>

BOOST_FIXTURE_TEST_SUITE(random_tests, BasicTestingSetup)

/** Does-it-compile test for compatibility with standard C++11 RNG interface. */
BOOST_AUTO_TEST_CASE(stdrandom_test)
{
    OpenSSLRandomContext ctx;
    std::uniform_int_distribution<int> distribution(3, 9);
    for (int i = 0; i < 100; ++i) {
        int x = distribution(ctx);
        BOOST_CHECK(x >= 3);
        BOOST_CHECK(x <= 9);

        std::vector<int> test{1,2,3,4,5,6,7,8,9,10};
        std::shuffle(test.begin(), test.end(), ctx);
        for (int j = 1; j <= 10; ++j) {
            BOOST_CHECK(std::find(test.begin(), test.end(), j) != test.end());
        }
    }

}

BOOST_AUTO_TEST_SUITE_END()
