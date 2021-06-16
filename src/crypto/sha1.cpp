// Copyright (c) 2014 The Bitcoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "crypto/sha1.h"

#include "crypto/common.h"

#include <string.h>

#include <intel-ipsec-mb.h>

// Internal implementation code.
namespace
{
/// Internal SHA-1 implementation.
namespace sha1
{

/** Initialize SHA-1 state. */
void inline Initialize(uint32_t* s)
{
    s[0] = 0x67452301ul;
    s[1] = 0xEFCDAB89ul;
    s[2] = 0x98BADCFEul;
    s[3] = 0x10325476ul;
    s[4] = 0xC3D2E1F0ul;
}

/** Perform a SHA-1 transformation, processing a 64-byte chunk. */
void Transform(uint32_t* s, const unsigned char* chunk)
{
    // Perform SHA1 one block (Intel AVX2)
    sha1_one_block_avx2(chunk, s);
}

} // namespace sha1

} // namespace

////// SHA1

CSHA1::CSHA1() : bytes(0)
{
    sha1::Initialize(s);
}

CSHA1& CSHA1::Write(const unsigned char* data, size_t len)
{
    const unsigned char* end = data + len;
    size_t bufsize = bytes % 64;
    if (bufsize && bufsize + len >= 64) {
        // Fill the buffer, and process it.
        memcpy(buf + bufsize, data, 64 - bufsize);
        bytes += 64 - bufsize;
        data += 64 - bufsize;
        sha1::Transform(s, buf);
        bufsize = 0;
    }
    while (end >= data + 64) {
        // Process full chunks directly from the source.
        sha1::Transform(s, data);
        bytes += 64;
        data += 64;
    }
    if (end > data) {
        // Fill the buffer with what remains.
        memcpy(buf + bufsize, data, end - data);
        bytes += end - data;
    }
    return *this;
}

void CSHA1::Finalize(unsigned char hash[OUTPUT_SIZE])
{
    static const unsigned char pad[64] = {0x80};
    unsigned char sizedesc[8];
    WriteBE64(sizedesc, bytes << 3);
    Write(pad, 1 + ((119 - (bytes % 64)) % 64));
    Write(sizedesc, 8);
    WriteBE32(hash, s[0]);
    WriteBE32(hash + 4, s[1]);
    WriteBE32(hash + 8, s[2]);
    WriteBE32(hash + 12, s[3]);
    WriteBE32(hash + 16, s[4]);
}

CSHA1& CSHA1::Reset()
{
    bytes = 0;
    sha1::Initialize(s);
    return *this;
}
