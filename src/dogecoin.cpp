// Copyright (c) 2015 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include <boost/random/uniform_int.hpp>
#include <boost/random/mersenne_twister.hpp>

#include "arith_uint256.h"
#include "dogecoin.h"
#include "util.h"

int static generateMTRandom(unsigned int s, int range)
{
    boost::mt19937 gen(s);
    boost::uniform_int<> dist(1, range);
    return dist(gen);
}

unsigned int CalculateDogecoinNextWorkRequired(const CBlockIndex* pindexLast, int64_t nFirstBlockTime, const Consensus::Params& params)
{
    int nHeight = pindexLast->nHeight + 1;
    bool fNewDifficultyProtocol = (nHeight >= 145000);
    // bool fNewDifficultyProtocol = (nHeight >= params.GetDigiShieldForkBlock());
    const int64_t retargetTimespan = fNewDifficultyProtocol ? 60 // params.DigiShieldTargetTimespan()
                                                              :
                                                              params.nPowTargetTimespan;

    const int64_t nActualTimespan = pindexLast->GetBlockTime() - nFirstBlockTime;
    int64_t nModulatedTimespan = nActualTimespan;
    int64_t nMaxTimespan;
    int64_t nMinTimespan;

    if (fNewDifficultyProtocol) //DigiShield implementation - thanks to RealSolid & WDC for this code
    {
        // amplitude filter - thanks to daft27 for this code
        nModulatedTimespan = retargetTimespan + (nModulatedTimespan - retargetTimespan) / 8;

        nMinTimespan = retargetTimespan - (retargetTimespan / 4);
        nMaxTimespan = retargetTimespan + (retargetTimespan / 2);
    } else if (nHeight > 10000) {
        nMinTimespan = retargetTimespan / 4;
        nMaxTimespan = retargetTimespan * 4;
    } else if (nHeight > 5000) {
        nMinTimespan = retargetTimespan / 8;
        nMaxTimespan = retargetTimespan * 4;
    } else {
        nMinTimespan = retargetTimespan / 16;
        nMaxTimespan = retargetTimespan * 4;
    }

    // Limit adjustment step
    if (nModulatedTimespan < nMinTimespan)
        nModulatedTimespan = nMinTimespan;
    else if (nModulatedTimespan > nMaxTimespan)
        nModulatedTimespan = nMaxTimespan;

    // Retarget
    const arith_uint256 bnPowLimit = UintToArith256(params.powLimit);
    arith_uint256 bnNew;
    arith_uint256 bnOld;
    bnNew.SetCompact(pindexLast->nBits);
    bnOld = bnNew;
    bnNew *= nModulatedTimespan;
    bnNew /= retargetTimespan;

    if (bnNew > bnPowLimit)
        bnNew = bnPowLimit;

    /// debug print
    LogPrintf("GetNextWorkRequired RETARGET\n");
    LogPrintf("params.nPowTargetTimespan = %d    nActualTimespan = %d\n", params.nPowTargetTimespan, nActualTimespan);
    LogPrintf("Before: %08x  %s\n", pindexLast->nBits, bnOld.ToString());
    LogPrintf("After:  %08x  %s\n", bnNew.GetCompact(), bnNew.ToString());

    return bnNew.GetCompact();
}


bool CheckAuxPowProofOfWork(const CBlockHeader& block, const Consensus::Params& params)
{
    /* Except for legacy blocks with full version 1, ensure that
       the chain ID is correct.  Legacy blocks are not allowed since
       the merge-mining start, which is checked in AcceptBlockHeader
       where the height is known.  */
    if (!block.IsLegacy() && params.fStrictChainId && block.GetChainId() != params.nAuxpowChainId)
        return error("%s : block does not have our chain ID"
                     " (got %d, expected %d, full nVersion %d)",
                     __func__, block.GetChainId(),
                     params.nAuxpowChainId, block.nVersion);

    /* If there is no auxpow, just check the block hash.  */
    if (!block.auxpow)
    {
        if (block.IsAuxpow())
            return error("%s : no auxpow on block with auxpow version",
                         __func__);

        if (!CheckProofOfWork(block.GetPoWHash(), block.nBits, params))
            return error("%s : non-AUX proof of work failed", __func__);

        return true;
    }

    /* We have auxpow.  Check it.  */

    if (!block.IsAuxpow())
        return error("%s : auxpow on block with non-auxpow version", __func__);

    if (!block.auxpow->check(block.GetHash(), block.GetChainId(), params))
        return error("%s : AUX POW is not valid", __func__);
    if (!CheckProofOfWork(block.auxpow->getParentBlockPoWHash(), block.nBits, params))
        return error("%s : AUX proof of work failed", __func__);

    return true;
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
