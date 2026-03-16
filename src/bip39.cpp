// Copyright (c) 2024 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "bip39.h"
#include "crypto/hmac_sha512.h"
#include "support/cleanse.h"

#include <sstream>
#include <cstring>

namespace bip39 {

bool ValidateMnemonicWordCount(const std::string& mnemonic)
{
    std::istringstream iss(mnemonic);
    std::string word;
    int count = 0;
    while (iss >> word)
        count++;

    return (count == 12 || count == 15 || count == 18 || count == 21 || count == 24);
}

SecureVector PBKDF2_HMAC_SHA512(const std::string& password, const std::string& salt,
                                 uint32_t iterations, size_t keylen)
{
    SecureVector result(keylen);
    size_t blocks = (keylen + CHMAC_SHA512::OUTPUT_SIZE - 1) / CHMAC_SHA512::OUTPUT_SIZE;

    for (size_t block = 1; block <= blocks; block++)
    {
        // U1 = HMAC(password, salt || INT_32_BE(block))
        unsigned char blockBE[4];
        blockBE[0] = (block >> 24) & 0xff;
        blockBE[1] = (block >> 16) & 0xff;
        blockBE[2] = (block >> 8)  & 0xff;
        blockBE[3] = (block)       & 0xff;

        unsigned char U[CHMAC_SHA512::OUTPUT_SIZE];
        unsigned char T[CHMAC_SHA512::OUTPUT_SIZE];

        {
            CHMAC_SHA512 hmac((const unsigned char*)password.data(), password.size());
            hmac.Write((const unsigned char*)salt.data(), salt.size());
            hmac.Write(blockBE, 4);
            hmac.Finalize(U);
        }

        memcpy(T, U, CHMAC_SHA512::OUTPUT_SIZE);

        // U2..Uc
        for (uint32_t i = 1; i < iterations; i++)
        {
            CHMAC_SHA512 hmac((const unsigned char*)password.data(), password.size());
            hmac.Write(U, CHMAC_SHA512::OUTPUT_SIZE);
            hmac.Finalize(U);

            for (size_t j = 0; j < CHMAC_SHA512::OUTPUT_SIZE; j++)
                T[j] ^= U[j];
        }

        size_t offset = (block - 1) * CHMAC_SHA512::OUTPUT_SIZE;
        size_t copyLen = std::min((size_t)CHMAC_SHA512::OUTPUT_SIZE, keylen - offset);
        memcpy(result.data() + offset, T, copyLen);

        memory_cleanse(U, sizeof(U));
        memory_cleanse(T, sizeof(T));
    }

    return result;
}

SecureVector MnemonicToSeed(const std::string& mnemonic, const std::string& passphrase)
{
    // BIP39: PBKDF2(HMAC-SHA512, mnemonic, "mnemonic" + passphrase, 2048, 64)
    const std::string salt = "mnemonic" + passphrase;
    return PBKDF2_HMAC_SHA512(mnemonic, salt, 2048, BIP39_SEED_LEN);
}

} // namespace bip39
