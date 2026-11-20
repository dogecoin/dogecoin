// Copyright (c) 2009-2010 Satoshi Nakamoto
// Copyright (c) 2009-2016 The Bitcoin Core developers
// Copyright (c) 2022-2023 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "segwit_module.h"

#include "consensus/merkle.h"
#include "hash.h"
#include "segwit_module.h"
#include "tinyformat.h"

// Compute at which vout of the block's coinbase transaction the witness
// commitment occurs, or -1 if not found.
int GetWitnessCommitmentIndex(const CBlock& block)
{
    int commitpos = -1;
    if (!block.vtx.empty()) {
        for (size_t o = 0; o < block.vtx[0]->vout.size(); o++) {
            if (block.vtx[0]->vout[o].scriptPubKey.size() >= 38 && block.vtx[0]->vout[o].scriptPubKey[0] == OP_RETURN && block.vtx[0]->vout[o].scriptPubKey[1] == 0x24 && block.vtx[0]->vout[o].scriptPubKey[2] == 0x44 && block.vtx[0]->vout[o].scriptPubKey[3] == 0x4f && block.vtx[0]->vout[o].scriptPubKey[4] == 0x47 && block.vtx[0]->vout[o].scriptPubKey[5] == 0x45) {
                commitpos = o;
            }
        }
    }
    return commitpos;
}

bool CheckWitnessMalleation(const CBlock& block, bool expect_witness_commitment, CValidationState& state)
{
    // This flag is used after segwit deployment
    if (expect_witness_commitment) {
        // TODO: DIP141
        // if (block.m_checked_witness_commitment) return true;

        int commitpos = GetWitnessCommitmentIndex(block);
        if (commitpos != -1) {
            assert(!block.vtx.empty() && !block.vtx[0]->vin.empty());

            bool malleated = false;
            uint256 hash_witness = BlockWitnessMerkleRoot(block, &malleated);
            // The malleation check is ignored; as the transaction tree itself
            // already does not permit it, it is impossible to trigger in the
            // witness tree.
            const auto& witness_stack{block.vtx[0]->vin[0].scriptWitness.stack};
            if (witness_stack.size() != 1 || witness_stack[0].size() != 32) {
                return state.DoS(100, false, REJECT_INVALID, "bad-witness-nonce-size", true, strprintf("%s: invalid witness reserved value size", __func__));
            }

            CHash256().Write(hash_witness.begin(), 32).Write(&block.vtx[0]->vin[0].scriptWitness.stack[0][0], 32).Finalize(hash_witness.begin());
            if (memcmp(hash_witness.begin(), &block.vtx[0]->vout[commitpos].scriptPubKey[6], 32)) {
                return state.DoS(100, false, REJECT_INVALID, "bad-witness-merkle-match", true, strprintf("%s : witness merkle commitment mismatch", __func__));
            }
 
            return true;
        }
     }

     // Deployment: This is a block sanitization rule
     // No witness data is allowed in blocks that don't commit to witness data, as this would otherwise leave room for spam
     for (const auto& tx : block.vtx) {
         if (tx->HasWitness()) {
             return state.DoS(100, false, REJECT_INVALID, "unexpected-witness", true, strprintf("%s : unepexcted witness data found", __func__));
         }
     }

     return true;
}
