// Copyright (c) 2009-2010 Satoshi Nakamoto
// Copyright (c) 2009-2014 The Bitcoin Core developers
// Copyright (c) 2015 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#ifndef BITCOIN_CONSENSUS_PARAMS_H
#define BITCOIN_CONSENSUS_PARAMS_H

#include "uint256.h"

namespace Consensus {
/**
 * Parameters that influence chain consensus.
 */
struct Params {
    uint256 hashGenesisBlock;
    int nSubsidyHalvingInterval;
    /** Used to check majorities for block version upgrade */
    int nMajorityEnforceBlockUpgrade;
    int nMajorityRejectBlockOutdated;
    int nMajorityWindow;
    int nCoinbaseMaturity;
    /** Proof of work parameters */
    uint256 powLimit;
    bool fPowAllowMinDifficultyBlocks;
    int64_t nPowTargetSpacing;
    int64_t nPowTargetTimespan;
    int64_t DifficultyAdjustmentInterval() const { return nPowTargetTimespan / nPowTargetSpacing; }

    /** Dogecoin-specific parameters */
    bool fDigishieldDifficultyCalculation;
    bool fPowAllowDigishieldMinDifficultyBlocks; // Allow minimum difficulty blocks where a retarget would normally occur
    bool fSimplifiedRewards;

    /** Auxpow parameters */
    int16_t nAuxpowChainId;
    bool fAllowAuxPow;
    bool fStrictChainId;
    bool fAllowLegacyBlocks;

    /** Height-aware consensus parameters */
    uint32_t nHeightEffective; // When these parameters come into use
    struct Params *pLeft;      // Left hand branch
    struct Params *pRight;     // Right hand branch
};
} // namespace Consensus

#endif // BITCOIN_CONSENSUS_PARAMS_H
