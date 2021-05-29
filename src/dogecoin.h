// Copyright (c) 2015-2017 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#ifndef BITCOIN_DOGECOIN_H
#define BITCOIN_DOGECOIN_H

#include <amount.h>
#include <chain.h>
#include <chainparams.h>
#include <uint256.h>

bool AllowDigishieldMinDifficultyForBlock(const CBlockIndex* pindexLast, const CBlockHeader *pblock, const Consensus::Params& params);
unsigned int CalculateDogecoinNextWorkRequired(const CBlockIndex* pindexLast, int64_t nLastRetargetTime, const Consensus::Params& params);
CAmount GetDogecoinBlockSubsidy(int nHeight, const Consensus::Params& consensusParams, uint256 prevHash);

#endif // BITCOIN_DOGECOIN_H
