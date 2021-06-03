// Copyright (c) 2015-2021 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include <boost/random/mersenne_twister.hpp>
#include <boost/random/uniform_int.hpp>

#include <arith_uint256.h>
#include <chain.h>
#include <dogecoin.h>
#include <pow.h>
#include <util/system.h>

int static generateMTRandom(unsigned int s, int range)
{
    boost::mt19937 gen(s);
    boost::uniform_int<> dist(1, range);
    return dist(gen);
}

CAmount GetDogecoinBlockSubsidy(int nHeight, const Consensus::Params& consensusParams, uint256 prevHash)
{
    int halvings = nHeight / consensusParams.nSubsidyHalvingInterval;

    if (nHeight < 145000 && !consensusParams.fSimplifiedRewards)
    {
        // Old-style rewards derived from the previous block hash

        // This extracts 7*4 bits out of the middle of the previous block hash to use as a seed.
        // It's longer than the 1.14 code, but avoids the locale-sensitive strtol() function.
        const int bitsPerNibble = 4;
        const int totalNibbles = 256 / bitsPerNibble;
        const int nibblesToClear = 7;
        const int nibblesToKeep = 7;
        arith_uint256 upperBits = UintToArith256(prevHash) << (nibblesToClear * bitsPerNibble);
        arith_uint256 cleanedBits = upperBits >> ((totalNibbles - nibblesToKeep) * bitsPerNibble);
        uint64_t seed = ArithToUint256(cleanedBits).GetUint64(0);

        // Convert the seed into a subsidy value.
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


// Dogecoin: Normally minimum difficulty blocks can only occur in between
// retarget blocks. However, once we introduce Digishield every block is
// a retarget, so we need to handle minimum difficulty on all blocks.
bool AllowDigishieldMinDifficultyForBlock(const CBlockIndex* pindexLast, const CBlockHeader *pblock, const Consensus::Params& params)
{
    // check if the chain allows minimum difficulty blocks
    if (!params.fPowAllowMinDifficultyBlocks)
        return false;

    // check if the chain allows minimum difficulty blocks on recalc blocks
    if (pindexLast->nHeight < 157500)
    // if (!params.fPowAllowDigishieldMinDifficultyBlocks)
        return false;

    // Allow for a minimum block time if the elapsed time > 2*nTargetSpacing
    return (pblock->GetBlockTime() > pindexLast->GetBlockTime() + params.nPowTargetSpacing*2);
}

unsigned int CalculateDogecoinNextWorkRequired(const CBlockIndex* pindexLast, int64_t nFirstBlockTime, const Consensus::Params& params)
{
    if (params.fPowNoRetargeting)
        return pindexLast->nBits;

    int nHeight = pindexLast->nHeight + 1;
    bool fNewDifficultyProtocol = (nHeight >= 145000);
    // bool fNewDifficultyProtocol = (nHeight >= params.GetDigiShieldForkBlock());
    const int64_t nRetargetTimespan = fNewDifficultyProtocol
                             ? 60 // params.DigiShieldTargetTimespan()
                             : params.nPowTargetTimespan;

    int64_t nTimespan = pindexLast->GetBlockTime() - nFirstBlockTime;
    int64_t nMaxTimespan;
    int64_t nMinTimespan;

    if (fNewDifficultyProtocol) //DigiShield implementation - thanks to RealSolid & WDC for this code
    {
        // amplitude filter - thanks to daft27 for this code
        nTimespan = nRetargetTimespan + (nTimespan - nRetargetTimespan) / 8;

        nMinTimespan = nRetargetTimespan - (nRetargetTimespan / 4);
        nMaxTimespan = nRetargetTimespan + (nRetargetTimespan / 2);
    } else if (nHeight > 10000) {
        nMinTimespan = nRetargetTimespan / 4;
        nMaxTimespan = nRetargetTimespan * 4;
    } else if (nHeight > 5000) {
        nMinTimespan = nRetargetTimespan / 8;
        nMaxTimespan = nRetargetTimespan * 4;
    } else {
        nMinTimespan = nRetargetTimespan / 16;
        nMaxTimespan = nRetargetTimespan * 4;
    }

    // Limit adjustment step
    if (nTimespan < nMinTimespan)
        nTimespan = nMinTimespan;
    else if (nTimespan > nMaxTimespan)
        nTimespan = nMaxTimespan;

    // Retarget
    const arith_uint256 bnPowLimit = UintToArith256(params.powLimit);
    arith_uint256 bnNew;
    bnNew.SetCompact(pindexLast->nBits);
    bnNew *= nTimespan;
    bnNew /= nRetargetTimespan;

    if (bnNew > bnPowLimit)
        bnNew = bnPowLimit;

    return bnNew.GetCompact();
}
