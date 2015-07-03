// Copyright (c) 2011-2013 The Bitcoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

//
// Unit tests for block-chain checkpoints
//

#include "checkpoints.h"

#include "uint256.h"
#include "test/test_bitcoin.h"
#include "chainparams.h"

#include <boost/test/unit_test.hpp>

using namespace std;

BOOST_FIXTURE_TEST_SUITE(Checkpoints_tests, BasicTestingSetup)

BOOST_AUTO_TEST_CASE(sanity)
{
    const Checkpoints::CCheckpointData& checkpoints = Params(CBaseChainParams::MAIN).Checkpoints();
    uint256 p42279 = uint256S("0x8444c3ef39a46222e87584ef956ad2c9ef401578bd8b51e8e4b9a86ec3134d3a");
    uint256 p42400 = uint256S("0x557bb7c17ed9e6d4a6f9361cfddf7c1fc0bdc394af7019167442b41f507252b4");
    BOOST_CHECK(Checkpoints::CheckBlock(checkpoints, 42279, p42279));
    BOOST_CHECK(Checkpoints::CheckBlock(checkpoints, 42400, p42400));

    
    // Wrong hashes at checkpoints should fail:
    BOOST_CHECK(!Checkpoints::CheckBlock(checkpoints, 42279, p42400));
    BOOST_CHECK(!Checkpoints::CheckBlock(checkpoints, 42400, p42279));

    // ... but any hash not at a checkpoint should succeed:
    BOOST_CHECK(Checkpoints::CheckBlock(checkpoints, 42279+1, p42400));
    BOOST_CHECK(Checkpoints::CheckBlock(checkpoints, 42400+1, p42279));

    BOOST_CHECK(Checkpoints::GetTotalBlocksEstimate(checkpoints) >= 42400);
}    

BOOST_AUTO_TEST_SUITE_END()
