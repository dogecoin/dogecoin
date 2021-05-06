// Copyright (c) 2021 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "amount.h"
#include "chain.h"
#include "chainparams.h"

#ifdef ENABLE_WALLET
CFeeRate GetDogecoinWalletFeeRate();
CAmount GetDogecoinMinWalletFee(unsigned int nBytes_);
#endif
CAmount GetDogecoinMinRelayFee(const CTransaction& tx, unsigned int nBytes, bool fAllowFree);
CAmount GetDogecoinDustFee(const std::vector<CTxOut> &vout, CFeeRate &baseFeeRate);
