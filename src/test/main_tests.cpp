// Copyright (c) 2014 The Bitcoin Core developers
// Distributed under the MIT/X11 software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "core.h"
#include "main.h"
#include "uint256.h"

#include <boost/test/unit_test.hpp>

using namespace std;

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


BOOST_AUTO_TEST_CASE(GetMinRelayFee_test)
{
    uint64_t value = 1000 * COIN; // 1,000 DOGE

    CMutableTransaction tx;
    CTxOut txout1(value, (CScript)vector<unsigned char>(24, 0));
    tx.vout.push_back(txout1);
    
    int64_t nMinRelayTxFee = COIN;

    BOOST_CHECK(GetMinRelayFee(tx, 1, false) == nMinRelayTxFee);
    BOOST_CHECK(GetMinRelayFee(tx, 999, false) == nMinRelayTxFee);
    BOOST_CHECK(GetMinRelayFee(tx, 1000, false) == 2 * nMinRelayTxFee);
    BOOST_CHECK(GetMinRelayFee(tx, 2000, false) == 3 * nMinRelayTxFee);
    BOOST_CHECK(GetMinRelayFee(tx, MAX_STANDARD_TX_SIZE, false) == (1+(MAX_STANDARD_TX_SIZE/1000))*nMinRelayTxFee);
}

BOOST_AUTO_TEST_CASE(GetMinRelayFee_dust_test)
{
    uint64_t value = 1000; // 1,000 Koinu

    CMutableTransaction tx;
    CTxOut txout1(value, (CScript)vector<unsigned char>(24, 0));
    tx.vout.push_back(txout1);
    
    int64_t nMinRelayTxFee = COIN;

    BOOST_CHECK(GetMinRelayFee(tx, 999, false) == 2 * nMinRelayTxFee);
    BOOST_CHECK(GetMinRelayFee(tx, 1000, false) == 3 * nMinRelayTxFee);
    BOOST_CHECK(GetMinRelayFee(tx, 2000, false) == 4 * nMinRelayTxFee);

    CTxOut txout2(value, (CScript)vector<unsigned char>(24, 0));
    tx.vout.push_back(txout2);

    BOOST_CHECK(GetMinRelayFee(tx, 999, false) == 3 * nMinRelayTxFee);
    BOOST_CHECK(GetMinRelayFee(tx, 1000, false) == 4 * nMinRelayTxFee);
    BOOST_CHECK(GetMinRelayFee(tx, 2000, false) == 5 * nMinRelayTxFee);
}

BOOST_AUTO_TEST_CASE(GetMinRelayFee_free_test)
{
    uint64_t value = 1000 * COIN; // 1,000 DOGE

    CMutableTransaction tx;
    CTxOut txout1(value, (CScript)vector<unsigned char>(24, 0));
    tx.vout.push_back(txout1);
    
    int64_t nMinRelayTxFee = COIN;
    
    BOOST_CHECK(GetMinRelayFee(tx, 100, true) == 0);
    BOOST_CHECK(GetMinRelayFee(tx, 1000, true) == 0);
    BOOST_CHECK(GetMinRelayFee(tx, 25999, true) == 0);
    
    BOOST_CHECK(GetMinRelayFee(tx, 26000, true) > 0);
    BOOST_CHECK(GetMinRelayFee(tx, 26000, true) == GetMinRelayFee(tx, 26000, false));
    
    BOOST_CHECK(GetMinRelayFee(tx, MAX_STANDARD_TX_SIZE, true) == (1+(MAX_STANDARD_TX_SIZE/1000))*nMinRelayTxFee);
}

BOOST_AUTO_TEST_SUITE_END()
