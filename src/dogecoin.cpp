// Copyright (c) 2015-2017 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include <boost/random/uniform_int.hpp>
#include <boost/random/mersenne_twister.hpp>

#include "dogecoin.h"

int static generateMTRandom(unsigned int s, int range)
{
    boost::mt19937 gen(s);
    boost::uniform_int<> dist(1, range);
    return dist(gen);
}

CAmount GetDogecoinBlockSubsidy(int nHeight, const Consensus::Params& consensusParams, uint256 prevHash)
{
    int halvings = nHeight / consensusParams.nSubsidyHalvingInterval;

    if (nHeight < 145000) // && !consensusParams.SimplifiedRewards())
    {
        // Old-style rewards derived from the previous block hash
        const std::string cseed_str = prevHash.ToString().substr(7, 7);
        const char* cseed = cseed_str.c_str();
        char* endp = NULL;
        long seed = strtol(cseed, &endp, 16);
        CAmount maxReward = (1000000 >> halvings) - 1;
        int rand = generateMTRandom(seed, maxReward);

        return (1 + rand) * COIN;
    } else if (nHeight < (6 * consensusParams.nSubsidyHalvingInterval)) {
        // New-style constant rewards for each halving interval
        return (500000 * COIN) >> halvings;
    } else {
        // Constant inflation
        return 10000 * COIN;
    }
}
