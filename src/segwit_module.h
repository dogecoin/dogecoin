// Copyright (c) 2009-2010 Satoshi Nakamoto
// Copyright (c) 2009-2016 The Bitcoin Core developers
// Copyright (c) 2022-2023 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#ifndef SEGWIT_MODULE_H
#define SEGWIT_MODULE_H

#include "consensus/validation.h"
#include "primitives/block.h"

// Compute at which vout of the block's coinbase transaction the witness
// commitment occurs, or -1 if not found.
int GetWitnessCommitmentIndex(const CBlock& block);

bool CheckWitnessMalleation(const CBlock& block, bool expect_witness_commitment, CValidationState& state);

#endif // SEGWIT_MODULE_H
