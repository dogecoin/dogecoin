// Copyright (c) 2022 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#ifndef DOGECOIN_WALLET_H
#define DOGECOIN_WALLET_H

#include <cstdint>

/**
 * BIP-32 bit for hardened keys
 *
 * see https://github.com/bitcoin/bips/blob/master/bip-0032.mediawiki
 * refactored out of wallet/scriptpubkeyman.cpp so that we can reuse it
 */
const uint32_t BIP32_HARDENED_KEY_LIMIT = 0x80000000;

namespace dogecoin {

/* LEGACY WALLET DERIVATION PATH
 * ------------------------------
 *
 * Sets constants for the static parts of the legacy derivation path
 * that is used for bdb-based HD-wallets.
 *
 * The path is m/<account>/<chain>/<child>, where:
 *
 * account is always 0'
 * chain is either internal (1') or external (3', or 0' for legacy)
 * child is a sequence from 0' to 2147483647'
 *
 * all parts of the path are hardened in this scheme.
 */

/**
 * The derivation value for the account part of the legacy wallet path.
 *
 * This is always 0', similar to Bitcoin
 */
const uint32_t DERIVE_ACCOUNT_KEY = 0 | BIP32_HARDENED_KEY_LIMIT;

/** Chain (internal/external) values for legacy wallets */
enum ChainDerivationValue: uint32_t {
  DERIVE_EXTERNAL_LEGACY = 0 | BIP32_HARDENED_KEY_LIMIT, // Bitcoin value, used in 1.14.0-1.14.4
  DERIVE_INTERNAL        = 1 | BIP32_HARDENED_KEY_LIMIT, // default internal value
  DERIVE_EXTERNAL        = 3 | BIP32_HARDENED_KEY_LIMIT, // external value, used in 1.14.5+
};

/**
 * Indicates whether the given chain derivation value is an external key.
 *
 * Both the legacy value and the 1.14.5+ value need to be detected here.
 * If the key is not external, it can be considered internal.
 *
 * also see src/wallet/scriptpubkeyman.cpp
 */
bool IsExternalKey (const uint32_t nChainDerivationValue) {
  return (nChainDerivationValue == ChainDerivationValue::DERIVE_EXTERNAL) ||
         (nChainDerivationValue == ChainDerivationValue::DERIVE_EXTERNAL_LEGACY);
}

} // namespace dogecoin

#endif // DOGECOIN_WALLET_H
