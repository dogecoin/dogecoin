// Copyright (c) 2015 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#ifndef DOGECOIN_DOGECOIN_H
#define DOGECOIN_DOGECOIN_H

#include "amount.h"
#include "chain.h"
#include "chainparams.h"
#include "primitives/block.h"

bool AllowDigishieldMinDifficultyForBlock(const CBlockIndex* pindexLast, const CBlockHeader *pblock, const Consensus::Params& params);
CAmount GetDogecoinBlockSubsidy(int nHeight, const Consensus::Params& consensusParams, uint256 prevHash);
unsigned int CalculateDogecoinNextWorkRequired(const CBlockIndex* pindexLast, int64_t nLastRetargetTime, const Consensus::Params& params);

/**
 * Check proof-of-work of a block header, taking auxpow into account.
 * @param block The block header.
 * @param params Consensus parameters.
 * @return True iff the PoW is correct.
 */
bool CheckAuxPowProofOfWork(const CBlockHeader& block, const Consensus::Params& params);

int64_t GetDogecoinDustFee(const std::vector<CTxOut> &vout, CFeeRate &baseFeeRate);

/** Wrapper to make decisions based on AuxPoW version */
class CVersionedAuxBlock {
private:
    int nAuxPowVersion;
    CBlock* pblock;
public:
    CVersionedAuxBlock(int nAuxPowVersion, CBlock *pblock) {
        this->nAuxPowVersion = nAuxPowVersion;
        this->pblock = pblock;
    }

    bool AllowAuxPow() const { return nAuxPowVersion > 0; }
    CBlock* GetBlock() { return pblock; }

    /** Return the correct header to mine against: block header or aux header */
    CPureBlockHeader* GetHeaderToMine() {
        return this->AllowAuxPow() ? &pblock->auxpow->parentBlock : pblock;
    }

    /** Return the correct Scrypt PoW based on the auxPow version. */
    uint256 GetMinedPoW() {
        return this->AllowAuxPow() ?
            pblock->auxpow->getParentBlockPoWHash() : pblock->GetPoWHash();
    }

    void SetNonce(uint32_t nNonce) { GetHeaderToMine()->nNonce = nNonce; }

    void IncrementNonce() { ++GetHeaderToMine()->nNonce; }

    /** Only initialize AuxPow if it is allowed */
    void InitAuxPow() {
        if (this->AllowAuxPow())
            CAuxPow::initAuxPow(*pblock);
    };
};

#endif // DOGECOIN_DOGECOIN_H
