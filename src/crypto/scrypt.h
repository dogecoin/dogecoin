// Copyright (c) 2011-2012 The Litecoin Core developers
// Copyright (c) 2013-2021 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#ifndef BITCOIN_CRYPTO_SCRYPT_H
#define BITCOIN_CRYPTO_SCRYPT_H
#include "crypto/hwcap.h"
#include <stdlib.h>
#include <stdint.h>

#if defined(HAVE_CONFIG_H)
#include "bitcoin-config.h" // for USE_SSE2
#endif

static const int SCRYPT_SCRATCHPAD_SIZE = 131072 + 63;

void
PBKDF2_SHA256(const uint8_t *passwd, size_t passwdlen, const uint8_t *salt,
    size_t saltlen, uint64_t c, uint8_t *buf, size_t dkLen);

namespace scrypt_generic {
    void scrypt_1024_1_1_256_sp(const char *input, char *output, char *scratchpad);
}

namespace scrypt_sse2 {
    void scrypt_1024_1_1_256_sp(const char *input, char *output, char *scratchpad);
}

void scrypt_1024_1_1_256(const char *input, char *output);
bool scrypt_select_implementation(const HardwareCapabilities capabilities);

#ifndef __FreeBSD__
static inline uint32_t le32dec(const void *pp)
{
        const uint8_t *p = (uint8_t const *)pp;
        return ((uint32_t)(p[0]) + ((uint32_t)(p[1]) << 8) +
            ((uint32_t)(p[2]) << 16) + ((uint32_t)(p[3]) << 24));
}

static inline void le32enc(void *pp, uint32_t x)
{
        uint8_t *p = (uint8_t *)pp;
        p[0] = x & 0xff;
        p[1] = (x >> 8) & 0xff;
        p[2] = (x >> 16) & 0xff;
        p[3] = (x >> 24) & 0xff;
}
#endif
#endif // BITCOIN_CRYPTO_SCRYPT_H
