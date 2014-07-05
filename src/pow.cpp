// Copyright (c) 2009-2010 Satoshi Nakamoto
// Copyright (c) 2009-2014 The Bitcoin developers
// Distributed under the MIT/X11 software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "pow.h"

#include "chainparams.h"
#include "core.h"
#include "main.h"
#include "timedata.h"
#include "uint256.h"

// Determine if the for the given block, a min difficulty setting applies
bool AllowMinDifficultyForBlock(const CBlockIndex* pindexLast, const CBlockHeader *pblock)
{
    // check if the chain allows minimum difficulty blocks
    if (!Params().AllowMinDifficultyBlocks())
        return false;

    // check if we allow minimum difficulty at this block-height
    if (pindexLast->nHeight < Params().GetMinDifficultyAllowedStartBlock())
        return false;

    // Allow for a minimum block time if the elapsed time > 2*nTargetSpacing
    return (pblock->nTime > pindexLast->nTime + Params().TargetSpacing()*2);
}

unsigned int GetNextWorkRequired(const CBlockIndex* pindexLast, const CBlockHeader *pblock)
{
    unsigned int nProofOfWorkLimit = Params().ProofOfWorkLimit().GetCompact();
    int nHeight = pindexLast->nHeight + 1;
    bool fNewDifficultyProtocol = (nHeight >= Params().GetDigiShieldForkBlock());
    int64_t retargetInterval
      = fNewDifficultyProtocol
        ? Params().DigiShieldInterval()
        : Params().Interval();

    // Genesis block
    if (pindexLast == NULL)
        return nProofOfWorkLimit;

    if (AllowMinDifficultyForBlock(pindexLast, pblock))
    {
        // Special difficulty rule for testnet:
        // If the new block's timestamp is more than 2* nTargetSpacing minutes
        // then allow mining of a min-difficulty block.
        return nProofOfWorkLimit;
    }

    // Only change once per interval
    if ((pindexLast->nHeight+1) % retargetInterval != 0)
    {
        // kept for testnet legacy pre-block 160000
        if (Params().AllowMinDifficultyBlocks())
        {
            if (pblock->nTime > pindexLast->nTime + Params().TargetSpacing()*2)
            {
                // Special difficulty rule for testnet:
                // If the new block's timestamp is more than 2* nTargetSpacing minutes
                // then allow mining of a min-difficulty block.
                return nProofOfWorkLimit;
            } else {
                // Return the last non-special-min-difficulty-rules-block
                const CBlockIndex* pindex = pindexLast;
                while (pindex->pprev && pindex->nHeight % retargetInterval != 0 && pindex->nBits == nProofOfWorkLimit)
                    pindex = pindex->pprev;
                return pindex->nBits;
            }
        }
        return pindexLast->nBits;
    }

    // Dogecoin: This fixes an issue where a 51% attack can change difficulty at will.
    // Go back the full period unless it's the first retarget after genesis. Code courtesy of Art Forz
    int blockstogoback = retargetInterval-1;
    if ((pindexLast->nHeight+1) != retargetInterval)
        blockstogoback = retargetInterval;

    // Go back by what we want to be 14 days worth of blocks
    const CBlockIndex* pindexFirst = pindexLast;
    for (int i = 0; pindexFirst && i < blockstogoback; i++)
        pindexFirst = pindexFirst->pprev;
    assert(pindexFirst);

    return CalculateNextWorkRequired(pindexLast, pindexFirst->nTime);
}

unsigned int CalculateNextWorkRequired(const CBlockIndex* pindexLast, int64_t nLastRetargetTime) {
    int nHeight = pindexLast->nHeight + 1;
    bool fNewDifficultyProtocol = (nHeight >= Params().GetDigiShieldForkBlock());
    int64_t retargetTimespan =
        fNewDifficultyProtocol
        ? Params().DigiShieldTargetTimespan()
        : Params().TargetTimespan();

    // Limit adjustment step
    int64_t nActualTimespan = pindexLast->GetBlockTime() - nLastRetargetTime;
    int64_t nModulatedTimespan = nActualTimespan;

    if (fNewDifficultyProtocol) //DigiShield implementation - thanks to RealSolid & WDC for this code
    {
        // amplitude filter - thanks to daft27 for this code
        nModulatedTimespan = retargetTimespan + (nModulatedTimespan - retargetTimespan)/8;

        if (nModulatedTimespan < (retargetTimespan - (retargetTimespan/4)) ) nModulatedTimespan = (retargetTimespan - (retargetTimespan/4));
        if (nModulatedTimespan > (retargetTimespan + (retargetTimespan/2)) ) nModulatedTimespan = (retargetTimespan + (retargetTimespan/2));
    }
    else if (nHeight+1 > 10000) {
        if (nModulatedTimespan < retargetTimespan/4)
            nModulatedTimespan = retargetTimespan/4;
        if (nModulatedTimespan > retargetTimespan*4)
            nModulatedTimespan = retargetTimespan*4;
    }
    else if (nHeight+1 > 5000)
    {
        if (nModulatedTimespan < retargetTimespan/8)
            nModulatedTimespan = retargetTimespan/8;
        if (nModulatedTimespan > retargetTimespan*4)
            nModulatedTimespan = retargetTimespan*4;
    }
    else
    {
        if (nModulatedTimespan < retargetTimespan/16)
            nModulatedTimespan = retargetTimespan/16;
        if (nModulatedTimespan > retargetTimespan*4)
            nModulatedTimespan = retargetTimespan*4;
    }

    // Retarget
    uint256 bnNew;
    uint256 bnOld;
    bnNew.SetCompact(pindexLast->nBits);
    bnOld = bnNew;
    bnNew *= nModulatedTimespan;
    bnNew /= retargetTimespan;

    if (bnNew > Params().ProofOfWorkLimit())
        bnNew = Params().ProofOfWorkLimit();

    unsigned int nNewBits = bnNew.GetCompact();

    /// debug print
    LogPrintf("GetNextWorkRequired RETARGET\n");
    LogPrintf("nTargetTimespan = %d    nActualTimespan = %d    nModulated = %d\n", Params().TargetTimespan(), nActualTimespan, nModulatedTimespan);
    LogPrintf("Before: %08x  %s\n", pindexLast->nBits, bnOld.ToString());
    LogPrintf("After:  %08x  %s\n", bnNew.GetCompact(), bnNew.ToString());

    return nNewBits;
}

bool CheckProofOfWork(uint256 hash, unsigned int nBits)
{
    bool fNegative;
    bool fOverflow;
    uint256 bnTarget;
    bnTarget.SetCompact(nBits, &fNegative, &fOverflow);

    // Check range
    if (fNegative || bnTarget == 0 || fOverflow || bnTarget > Params().ProofOfWorkLimit())
        return error("CheckProofOfWork() : nBits below minimum work");

    // Check proof of work matches claimed amount
    if (hash > bnTarget)
        return error("CheckProofOfWork() : hash doesn't match nBits");

    return true;
}

//
// true if nBits is greater than the minimum amount of work that could
// possibly be required deltaTime after minimum work required was nBase
//
bool CheckMinWork(unsigned int nBits, unsigned int nBase, int64_t deltaTime)
{
    bool fOverflow = false;
    uint256 bnNewBlock;
    bnNewBlock.SetCompact(nBits, NULL, &fOverflow);
    if (fOverflow)
        return false;

    const uint256 &bnLimit = Params().ProofOfWorkLimit();
    // Testnet has min-difficulty blocks
    // after Params().TargetSpacing()*2 time between blocks:
    if (Params().AllowMinDifficultyBlocks() && deltaTime > Params().TargetSpacing()*2)
        return bnNewBlock <= bnLimit;

    uint256 bnResult;
    bnResult.SetCompact(nBase);
    while (deltaTime > 0 && bnResult < bnLimit)
    {
        if(chainActive.Height()+1<Params().GetDigiShieldForkBlock()){
            // Maximum 400% adjustment...
            bnResult *= 4;
            // ... in best-case exactly 4-times-normal target time
            deltaTime -= Params().TargetTimespan()*4;
        } else {
            // Maximum 10% adjustment...
            bnResult = (bnResult * 110) / 100;
            // ... in best-case exactly 4-times-normal target time
            deltaTime -= Params().DigiShieldTargetTimespan()*4;
        }
    }
    if (bnResult > bnLimit)
        bnResult = bnLimit;

    return bnNewBlock <= bnResult;
}

void UpdateTime(CBlockHeader* pblock, const CBlockIndex* pindexPrev)
{
    pblock->nTime = std::max(pindexPrev->GetMedianTimePast()+1, GetAdjustedTime());

    // Updating time can change work required on testnet:
    if (Params().AllowMinDifficultyBlocks())
        pblock->nBits = GetNextWorkRequired(pindexPrev, pblock);
}

uint256 GetProofIncrement(unsigned int nBits)
{
    uint256 bnTarget;
    bool fNegative;
    bool fOverflow;
    bnTarget.SetCompact(nBits, &fNegative, &fOverflow);
    if (fNegative || fOverflow || bnTarget == 0)
        return 0;
    // We need to compute 2**256 / (bnTarget+1), but we can't represent 2**256
    // as it's too large for a uint256. However, as 2**256 is at least as large
    // as bnTarget+1, it is equal to ((2**256 - bnTarget - 1) / (bnTarget+1)) + 1,
    // or ~bnTarget / (nTarget+1) + 1.
    return (~bnTarget / (bnTarget + 1)) + 1;
}
