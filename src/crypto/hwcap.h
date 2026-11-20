// Copyright (c) 2024 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#ifndef BITCOIN_CRYPTO_HWCAP_H
#define BITCOIN_CRYPTO_HWCAP_H

#include "compat/cpuid.h"

struct HardwareCapabilities {
  bool has_sse2;
};

HardwareCapabilities DetectHWCapabilities();

#endif // BITCOIN_CRYPTO_HWCAP_H
