// Copyright (c) 2014 The Bitcoin Core developers
// Distributed under the MIT/X11 software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

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


BOOST_AUTO_TEST_CASE(GetMinFee_test)
{
    uint64_t value = 1000 * COIN;

    CTransaction tx;
    CTxOut txout1(value, (CScript)vector<unsigned char>(24, 0));
    tx.vout.push_back(txout1);
    
    if(CTransaction::nMinTxFee == CTransaction::nMinRelayTxFee)
        CTransaction::nMinTxFee++;
    
    BOOST_CHECK(GetMinFee(tx, 100, false, GMF_RELAY) == CTransaction::nMinRelayTxFee);
    BOOST_CHECK(GetMinFee(tx, 100, false, GMF_SEND) == CTransaction::nMinTxFee);
    
    BOOST_CHECK(GetMinFee(tx, 1, false, GMF_RELAY) == CTransaction::nMinRelayTxFee);
    BOOST_CHECK(GetMinFee(tx, 1, false, GMF_SEND) == CTransaction::nMinTxFee);
    
    BOOST_CHECK(GetMinFee(tx, 999, false, GMF_RELAY) == CTransaction::nMinRelayTxFee);
    BOOST_CHECK(GetMinFee(tx, 999, false, GMF_SEND) == CTransaction::nMinTxFee);
    
    BOOST_CHECK(GetMinFee(tx, 1000, false, GMF_RELAY) == 2 * CTransaction::nMinRelayTxFee);
    BOOST_CHECK(GetMinFee(tx, 1000, false, GMF_SEND) == 2 * CTransaction::nMinTxFee);
    
    BOOST_CHECK(GetMinFee(tx, 2000, false, GMF_RELAY) == 3 * CTransaction::nMinRelayTxFee);
    BOOST_CHECK(GetMinFee(tx, 2000, false, GMF_SEND) == 3 * CTransaction::nMinTxFee);
    
    BOOST_CHECK(GetMinFee(tx, MAX_STANDARD_TX_SIZE, false, GMF_RELAY) == (1+(MAX_STANDARD_TX_SIZE/1000))*CTransaction::nMinRelayTxFee);
    BOOST_CHECK(GetMinFee(tx, MAX_STANDARD_TX_SIZE, false, GMF_SEND) == (1+(MAX_STANDARD_TX_SIZE/1000))*CTransaction::nMinTxFee);
}

BOOST_AUTO_TEST_CASE(GetMinFee_dust_test)
{
    uint64_t value = 1000;

    CTransaction tx;
    CTxOut txout1(value, (CScript)vector<unsigned char>(24, 0));
    tx.vout.push_back(txout1);
    
    if(CTransaction::nMinTxFee == CTransaction::nMinRelayTxFee)
        CTransaction::nMinTxFee++;
    
    BOOST_CHECK(GetMinFee(tx, 1, false, GMF_RELAY) == 2 * CTransaction::nMinRelayTxFee);
    BOOST_CHECK(GetMinFee(tx, 1, false, GMF_SEND) == 2 * CTransaction::nMinTxFee);
    
    BOOST_CHECK(GetMinFee(tx, 999, false, GMF_RELAY) == 2 * CTransaction::nMinRelayTxFee);
    BOOST_CHECK(GetMinFee(tx, 999, false, GMF_SEND) == 2 * CTransaction::nMinTxFee);
    
    BOOST_CHECK(GetMinFee(tx, 1000, false, GMF_RELAY) == 3 * CTransaction::nMinRelayTxFee);
    BOOST_CHECK(GetMinFee(tx, 1000, false, GMF_SEND) == 3 * CTransaction::nMinTxFee);
    
    BOOST_CHECK(GetMinFee(tx, 2000, false, GMF_RELAY) == 4 * CTransaction::nMinRelayTxFee);
    BOOST_CHECK(GetMinFee(tx, 2000, false, GMF_SEND) == 4 * CTransaction::nMinTxFee);
    
    BOOST_CHECK(GetMinFee(tx, MAX_STANDARD_TX_SIZE, false, GMF_RELAY) == (2+(MAX_STANDARD_TX_SIZE/1000))*CTransaction::nMinRelayTxFee);
    BOOST_CHECK(GetMinFee(tx, MAX_STANDARD_TX_SIZE, false, GMF_SEND) == (2+(MAX_STANDARD_TX_SIZE/1000))*CTransaction::nMinTxFee);
}

BOOST_AUTO_TEST_CASE(GetMinFee_manydust_test)
{
    uint64_t value = 1000;

    CTransaction tx;
    CTxOut txout1(1000 * COIN, (CScript)vector<unsigned char>(24, 0));
    tx.vout.push_back(txout1);
    
    for(int i=0; i<100; i++) {
        CTxOut txoutn(value, (CScript)vector<unsigned char>(24, 0));
        tx.vout.push_back(txoutn);
    }
    
    CTxOut txout101(1000 * COIN, (CScript)vector<unsigned char>(24, 0));
    tx.vout.push_back(txout101);
    
    if(CTransaction::nMinTxFee == CTransaction::nMinRelayTxFee)
        CTransaction::nMinTxFee++;
    
    BOOST_CHECK(GetMinFee(tx, 1, false, GMF_RELAY) == 101 * CTransaction::nMinRelayTxFee);
    BOOST_CHECK(GetMinFee(tx, 1, false, GMF_SEND) == 101 * CTransaction::nMinTxFee);
    
    BOOST_CHECK(GetMinFee(tx, 999, false, GMF_RELAY) == 101 * CTransaction::nMinRelayTxFee);
    BOOST_CHECK(GetMinFee(tx, 999, false, GMF_SEND) == 101 * CTransaction::nMinTxFee);
    
    BOOST_CHECK(GetMinFee(tx, 1000, false, GMF_RELAY) == 102 * CTransaction::nMinRelayTxFee);
    BOOST_CHECK(GetMinFee(tx, 1000, false, GMF_SEND) == 102 * CTransaction::nMinTxFee);
    
    BOOST_CHECK(GetMinFee(tx, 2000, false, GMF_RELAY) == 103 * CTransaction::nMinRelayTxFee);
    BOOST_CHECK(GetMinFee(tx, 2000, false, GMF_SEND) == 103 * CTransaction::nMinTxFee);
    
    BOOST_CHECK(GetMinFee(tx, MAX_STANDARD_TX_SIZE, false, GMF_RELAY) == (101+(MAX_STANDARD_TX_SIZE/1000))*CTransaction::nMinRelayTxFee);
    BOOST_CHECK(GetMinFee(tx, MAX_STANDARD_TX_SIZE, false, GMF_SEND) == (101+(MAX_STANDARD_TX_SIZE/1000))*CTransaction::nMinTxFee);
}

BOOST_AUTO_TEST_CASE(GetMinFee_relayfree_test)
{
    uint64_t value = 1000 * COIN;

    CTransaction tx;
    CTxOut txout1(value, (CScript)vector<unsigned char>(24, 0));
    tx.vout.push_back(txout1);
    
    if(CTransaction::nMinTxFee == CTransaction::nMinRelayTxFee)
        CTransaction::nMinTxFee++;
    
    BOOST_CHECK(GetMinFee(tx, 100, true, GMF_RELAY) == 0);
    BOOST_CHECK(GetMinFee(tx, 1000, true, GMF_RELAY) == 0);
    BOOST_CHECK(GetMinFee(tx, 25999, true, GMF_RELAY) == 0);
    
    BOOST_CHECK(GetMinFee(tx, 26000, true, GMF_RELAY) > 0);
    BOOST_CHECK(GetMinFee(tx, 26000, true, GMF_RELAY) == GetMinFee(tx, 26000, false, GMF_RELAY));
    
    BOOST_CHECK(GetMinFee(tx, MAX_STANDARD_TX_SIZE, true, GMF_RELAY) == (1+(MAX_STANDARD_TX_SIZE/1000))*CTransaction::nMinRelayTxFee);
}

BOOST_AUTO_TEST_CASE(GetMinFee_createNoFree_test)
{
    uint64_t value = 1000 * COIN;

    CTransaction tx;
    CTxOut txout1(value, (CScript)vector<unsigned char>(24, 0));
    tx.vout.push_back(txout1);
    
    if(CTransaction::nMinTxFee == CTransaction::nMinRelayTxFee)
        CTransaction::nMinTxFee++;
    
    BOOST_CHECK(GetMinFee(tx, 100, true, GMF_SEND) > 0);
    BOOST_CHECK(GetMinFee(tx, 100, true, GMF_SEND) == GetMinFee(tx, 100, false, GMF_SEND));
    BOOST_CHECK(GetMinFee(tx, 1000, true, GMF_SEND) > 0);
    BOOST_CHECK(GetMinFee(tx, 1000, true, GMF_SEND) == GetMinFee(tx, 1000, false, GMF_SEND));
    BOOST_CHECK(GetMinFee(tx, 25999, true, GMF_SEND) > 0);
    BOOST_CHECK(GetMinFee(tx, 25999, true, GMF_SEND) == GetMinFee(tx, 25999, false, GMF_SEND));
    
    BOOST_CHECK(GetMinFee(tx, 26000, true, GMF_SEND) > 0);
    BOOST_CHECK(GetMinFee(tx, 26000, true, GMF_SEND) == GetMinFee(tx, 26000, false, GMF_SEND));
    
    BOOST_CHECK(GetMinFee(tx, MAX_STANDARD_TX_SIZE, true, GMF_SEND) == (1+(MAX_STANDARD_TX_SIZE/1000))*CTransaction::nMinTxFee);
}

BOOST_AUTO_TEST_SUITE_END()
