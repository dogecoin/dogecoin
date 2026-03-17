// Copyright (c) 2024 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#ifndef BITCOIN_BIP39_H
#define BITCOIN_BIP39_H

#include <string>
#include <vector>
#include "support/allocators/secure.h"

namespace bip39 {

static const size_t BIP39_SEED_LEN = 64;

typedef std::vector<unsigned char, secure_allocator<unsigned char>> SecureVector;

/**
 * Validate that a mnemonic has an acceptable word count (12, 15, 18, 21, or 24).
 * Returns true if word count is valid.
 */
bool ValidateMnemonicWordCount(const std::string& mnemonic);

/**
 * Derive a 512-bit seed from a BIP39 mnemonic and passphrase.
 * Uses PBKDF2-HMAC-SHA512 with 2048 iterations, per BIP39 spec.
 * Salt = "mnemonic" + passphrase
 */
SecureVector MnemonicToSeed(const std::string& mnemonic, const std::string& passphrase = "");

/**
 * PBKDF2-HMAC-SHA512 key derivation.
 */
SecureVector PBKDF2_HMAC_SHA512(const std::string& password, const std::string& salt,
                                 uint32_t iterations, size_t keylen);

} // namespace bip39

#endif // BITCOIN_BIP39_H
