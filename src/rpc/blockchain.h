// Copyright (c) 2017-2018 The Bitcoin Core developers
// Copyright (c) 2023 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#ifndef BITCOIN_RPC_BLOCKCHAIN_H
#define BITCOIN_RPC_BLOCKCHAIN_H

#include <vector>
#include <stdint.h>
#include <amount.h>

class CBlock;
class CBlockIndex;
class UniValue;

static constexpr int NUM_GETBLOCKSTATS_PERCENTILES = 5;

/** Used by getblockstats to get feerates at different percentiles by weight  */
void CalculatePercentilesBySize(CAmount result[NUM_GETBLOCKSTATS_PERCENTILES], std::vector<std::pair<CAmount, int64_t>>& scores, int64_t total_size);

#endif
