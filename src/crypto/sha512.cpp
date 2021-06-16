// Copyright (c) 2014 The Bitcoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "crypto/sha512.h"

#include "crypto/common.h"

#include <string.h>

#include <intel-ipsec-mb.h>

// Internal implementation code.
namespace
{
/// Internal SHA-512 implementation.
namespace sha512
{

/** Initialize SHA-256 state. */
void inline Initialize(uint64_t* s)
{
    s[0] = 0x6a09e667f3bcc908ull;
    s[1] = 0xbb67ae8584caa73bull;
    s[2] = 0x3c6ef372fe94f82bull;
    s[3] = 0xa54ff53a5f1d36f1ull;
    s[4] = 0x510e527fade682d1ull;
    s[5] = 0x9b05688c2b3e6c1full;
    s[6] = 0x1f83d9abfb41bd6bull;
    s[7] = 0x5be0cd19137e2179ull;
}

/** Perform one SHA-512 transformation, processing a 128-byte chunk. */
void Transform(uint64_t* s, const unsigned char* chunk)
{
    // Perform SHA512 one block (Intel AVX2)
    sha512_one_block_avx2(chunk, s);
}

} // namespace sha512

} // namespace


////// SHA-512

CSHA512::CSHA512() : bytes(0)
{
    sha512::Initialize(s);
}

CSHA512& CSHA512::Write(const unsigned char* data, size_t len)
{
    const unsigned char* end = data + len;
    size_t bufsize = bytes % 128;
    if (bufsize && bufsize + len >= 128) {
        // Fill the buffer, and process it.
        memcpy(buf + bufsize, data, 128 - bufsize);
        bytes += 128 - bufsize;
        data += 128 - bufsize;
        sha512::Transform(s, buf);
        bufsize = 0;
    }
    while (end >= data + 128) {
        // Process full chunks directly from the source.
        sha512::Transform(s, data);
        data += 128;
        bytes += 128;
    }
    if (end > data) {
        // Fill the buffer with what remains.
        memcpy(buf + bufsize, data, end - data);
        bytes += end - data;
    }
    return *this;
}

void CSHA512::Finalize(unsigned char hash[OUTPUT_SIZE])
{
    static const unsigned char pad[128] = {0x80};
    unsigned char sizedesc[16] = {0x00};
    WriteBE64(sizedesc + 8, bytes << 3);
    Write(pad, 1 + ((239 - (bytes % 128)) % 128));
    Write(sizedesc, 16);
    WriteBE64(hash, s[0]);
    WriteBE64(hash + 8, s[1]);
    WriteBE64(hash + 16, s[2]);
    WriteBE64(hash + 24, s[3]);
    WriteBE64(hash + 32, s[4]);
    WriteBE64(hash + 40, s[5]);
    WriteBE64(hash + 48, s[6]);
    WriteBE64(hash + 56, s[7]);
}

CSHA512& CSHA512::Reset()
{
    bytes = 0;
    sha512::Initialize(s);
    return *this;
}

