// Copyright (c) 2014 The Bitcoin Core developers
// Copyright (c) 2022 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "crypto/sha512.h"

#include "crypto/common.h"
#include "support/experimental.h"

#include <string.h>

#if (defined(__ia64__) || defined(__x86_64__)) && \
    !defined(__APPLE__) && \
    (defined(USE_AVX2))
#include <intel-ipsec-mb.h>
#endif

#if defined(__arm__) || defined(__aarch32__) || defined(__arm64__) || defined(__aarch64__) || defined(_M_ARM)
# if defined(__GNUC__)
#  include <stdint.h>
# endif
# if defined(__ARM_NEON) || defined(_MSC_VER) || defined(__GNUC__)
#  include <arm_neon.h>
# endif
/** Apple Clang **/
# if defined(__APPLE__) && defined(__apple_build_version__)
#  include <sys/sysctl.h>
# endif
/** GCC and LLVM Clang, but not Apple Clang */
# if defined(__GNUC__) && !defined(__apple_build_version__)
#  if defined(__ARM_ACLE) || defined(__ARM_FEATURE_CRYPTO)
#   include "compat/arm_acle_selector.h"
#  endif
# endif

static const uint64_t sha512_round_constants[] =
{
    0x428a2f98d728ae22, 0x7137449123ef65cd,
    0xb5c0fbcfec4d3b2f, 0xe9b5dba58189dbbc,
    0x3956c25bf348b538, 0x59f111f1b605d019,
    0x923f82a4af194f9b, 0xab1c5ed5da6d8118,
    0xd807aa98a3030242, 0x12835b0145706fbe,
    0x243185be4ee4b28c, 0x550c7dc3d5ffb4e2,
    0x72be5d74f27b896f, 0x80deb1fe3b1696b1,
    0x9bdc06a725c71235, 0xc19bf174cf692694,
    0xe49b69c19ef14ad2, 0xefbe4786384f25e3,
    0x0fc19dc68b8cd5b5, 0x240ca1cc77ac9c65,
    0x2de92c6f592b0275, 0x4a7484aa6ea6e483,
    0x5cb0a9dcbd41fbd4, 0x76f988da831153b5,
    0x983e5152ee66dfab, 0xa831c66d2db43210,
    0xb00327c898fb213f, 0xbf597fc7beef0ee4,
    0xc6e00bf33da88fc2, 0xd5a79147930aa725,
    0x06ca6351e003826f, 0x142929670a0e6e70,
    0x27b70a8546d22ffc, 0x2e1b21385c26c926,
    0x4d2c6dfc5ac42aed, 0x53380d139d95b3df,
    0x650a73548baf63de, 0x766a0abb3c77b2a8,
    0x81c2c92e47edaee6, 0x92722c851482353b,
    0xa2bfe8a14cf10364, 0xa81a664bbc423001,
    0xc24b8b70d0f89791, 0xc76c51a30654be30,
    0xd192e819d6ef5218, 0xd69906245565a910,
    0xf40e35855771202a, 0x106aa07032bbd1b8,
    0x19a4c116b8d2d0c8, 0x1e376c085141ab53,
    0x2748774cdf8eeb99, 0x34b0bcb5e19b48a8,
    0x391c0cb3c5c95a63, 0x4ed8aa4ae3418acb,
    0x5b9cca4f7763e373, 0x682e6ff3d6b2b8a3,
    0x748f82ee5defb2fc, 0x78a5636f43172f60,
    0x84c87814a1f0ab72, 0x8cc702081a6439ec,
    0x90befffa23631e28, 0xa4506cebde82bde9,
    0xbef9a3f7b2c67915, 0xc67178f2e372532b,
    0xca273eceea26619c, 0xd186b8c721c0c207,
    0xeada7dd6cde0eb1e, 0xf57d4f7fee6ed178,
    0x06f067aa72176fba, 0x0a637dc5a2c898a6,
    0x113f9804bef90dae, 0x1b710b35131c471b,
    0x28db77f523047d84, 0x32caab7b40c72493,
    0x3c9ebe0a15c9bebc, 0x431d67c49c100d4c,
    0x4cc5d4becb3e42b6, 0x597f299cfc657e2a,
    0x5fcb6fab3ad6faec, 0x6c44198c4a475817
};
#endif  /** ARM Headers */

namespace sha512_armv82
{
void Transform_ARMV82(uint64_t* s, const unsigned char* chunk);
}

// Internal implementation code.
namespace
{
/// Internal SHA-512 implementation.
namespace sha512
{
uint64_t inline Ch(uint64_t x, uint64_t y, uint64_t z) { return z ^ (x & (y ^ z)); }
uint64_t inline Maj(uint64_t x, uint64_t y, uint64_t z) { return (x & y) | (z & (x | y)); }
uint64_t inline Sigma0(uint64_t x) { return (x >> 28 | x << 36) ^ (x >> 34 | x << 30) ^ (x >> 39 | x << 25); }
uint64_t inline Sigma1(uint64_t x) { return (x >> 14 | x << 50) ^ (x >> 18 | x << 46) ^ (x >> 41 | x << 23); }
uint64_t inline sigma0(uint64_t x) { return (x >> 1 | x << 63) ^ (x >> 8 | x << 56) ^ (x >> 7); }
uint64_t inline sigma1(uint64_t x) { return (x >> 19 | x << 45) ^ (x >> 61 | x << 3) ^ (x >> 6); }

/** One round of SHA-512. */
void inline Round(uint64_t a, uint64_t b, uint64_t c, uint64_t& d, uint64_t e, uint64_t f, uint64_t g, uint64_t& h, uint64_t k, uint64_t w)
{
    uint64_t t1 = h + Sigma1(e) + Ch(e, f, g) + k + w;
    uint64_t t2 = Sigma0(a) + Maj(a, b, c);
    d += t1;
    h = t1 + t2;
}

/** Initialize SHA-512 state. */
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

/** Perform one SHA-512 transformation, processing a 128-byte chunk. (AVX2) */
void Transform_AVX2(uint64_t* s, const unsigned char* chunk)
{
#ifdef USE_AVX2
    // Perform SHA512 one block (Intel AVX2)
    EXPERIMENTAL_FEATURE
    sha512_one_block_avx2(chunk, s);
#endif
}

/** Perform one SHA-512 transformation, processing a 128-byte chunk. */
void Transform(uint64_t* s, const unsigned char* chunk)
{
    // Perform SHA512 one block (legacy)
    uint64_t a = s[0], b = s[1], c = s[2], d = s[3], e = s[4], f = s[5], g = s[6], h = s[7];
    uint64_t w0, w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15;

    Round(a, b, c, d, e, f, g, h, 0x428a2f98d728ae22ull, w0 = ReadBE64(chunk + 0));
    Round(h, a, b, c, d, e, f, g, 0x7137449123ef65cdull, w1 = ReadBE64(chunk + 8));
    Round(g, h, a, b, c, d, e, f, 0xb5c0fbcfec4d3b2full, w2 = ReadBE64(chunk + 16));
    Round(f, g, h, a, b, c, d, e, 0xe9b5dba58189dbbcull, w3 = ReadBE64(chunk + 24));
    Round(e, f, g, h, a, b, c, d, 0x3956c25bf348b538ull, w4 = ReadBE64(chunk + 32));
    Round(d, e, f, g, h, a, b, c, 0x59f111f1b605d019ull, w5 = ReadBE64(chunk + 40));
    Round(c, d, e, f, g, h, a, b, 0x923f82a4af194f9bull, w6 = ReadBE64(chunk + 48));
    Round(b, c, d, e, f, g, h, a, 0xab1c5ed5da6d8118ull, w7 = ReadBE64(chunk + 56));
    Round(a, b, c, d, e, f, g, h, 0xd807aa98a3030242ull, w8 = ReadBE64(chunk + 64));
    Round(h, a, b, c, d, e, f, g, 0x12835b0145706fbeull, w9 = ReadBE64(chunk + 72));
    Round(g, h, a, b, c, d, e, f, 0x243185be4ee4b28cull, w10 = ReadBE64(chunk + 80));
    Round(f, g, h, a, b, c, d, e, 0x550c7dc3d5ffb4e2ull, w11 = ReadBE64(chunk + 88));
    Round(e, f, g, h, a, b, c, d, 0x72be5d74f27b896full, w12 = ReadBE64(chunk + 96));
    Round(d, e, f, g, h, a, b, c, 0x80deb1fe3b1696b1ull, w13 = ReadBE64(chunk + 104));
    Round(c, d, e, f, g, h, a, b, 0x9bdc06a725c71235ull, w14 = ReadBE64(chunk + 112));
    Round(b, c, d, e, f, g, h, a, 0xc19bf174cf692694ull, w15 = ReadBE64(chunk + 120));

    Round(a, b, c, d, e, f, g, h, 0xe49b69c19ef14ad2ull, w0 += sigma1(w14) + w9 + sigma0(w1));
    Round(h, a, b, c, d, e, f, g, 0xefbe4786384f25e3ull, w1 += sigma1(w15) + w10 + sigma0(w2));
    Round(g, h, a, b, c, d, e, f, 0x0fc19dc68b8cd5b5ull, w2 += sigma1(w0) + w11 + sigma0(w3));
    Round(f, g, h, a, b, c, d, e, 0x240ca1cc77ac9c65ull, w3 += sigma1(w1) + w12 + sigma0(w4));
    Round(e, f, g, h, a, b, c, d, 0x2de92c6f592b0275ull, w4 += sigma1(w2) + w13 + sigma0(w5));
    Round(d, e, f, g, h, a, b, c, 0x4a7484aa6ea6e483ull, w5 += sigma1(w3) + w14 + sigma0(w6));
    Round(c, d, e, f, g, h, a, b, 0x5cb0a9dcbd41fbd4ull, w6 += sigma1(w4) + w15 + sigma0(w7));
    Round(b, c, d, e, f, g, h, a, 0x76f988da831153b5ull, w7 += sigma1(w5) + w0 + sigma0(w8));
    Round(a, b, c, d, e, f, g, h, 0x983e5152ee66dfabull, w8 += sigma1(w6) + w1 + sigma0(w9));
    Round(h, a, b, c, d, e, f, g, 0xa831c66d2db43210ull, w9 += sigma1(w7) + w2 + sigma0(w10));
    Round(g, h, a, b, c, d, e, f, 0xb00327c898fb213full, w10 += sigma1(w8) + w3 + sigma0(w11));
    Round(f, g, h, a, b, c, d, e, 0xbf597fc7beef0ee4ull, w11 += sigma1(w9) + w4 + sigma0(w12));
    Round(e, f, g, h, a, b, c, d, 0xc6e00bf33da88fc2ull, w12 += sigma1(w10) + w5 + sigma0(w13));
    Round(d, e, f, g, h, a, b, c, 0xd5a79147930aa725ull, w13 += sigma1(w11) + w6 + sigma0(w14));
    Round(c, d, e, f, g, h, a, b, 0x06ca6351e003826full, w14 += sigma1(w12) + w7 + sigma0(w15));
    Round(b, c, d, e, f, g, h, a, 0x142929670a0e6e70ull, w15 += sigma1(w13) + w8 + sigma0(w0));

    Round(a, b, c, d, e, f, g, h, 0x27b70a8546d22ffcull, w0 += sigma1(w14) + w9 + sigma0(w1));
    Round(h, a, b, c, d, e, f, g, 0x2e1b21385c26c926ull, w1 += sigma1(w15) + w10 + sigma0(w2));
    Round(g, h, a, b, c, d, e, f, 0x4d2c6dfc5ac42aedull, w2 += sigma1(w0) + w11 + sigma0(w3));
    Round(f, g, h, a, b, c, d, e, 0x53380d139d95b3dfull, w3 += sigma1(w1) + w12 + sigma0(w4));
    Round(e, f, g, h, a, b, c, d, 0x650a73548baf63deull, w4 += sigma1(w2) + w13 + sigma0(w5));
    Round(d, e, f, g, h, a, b, c, 0x766a0abb3c77b2a8ull, w5 += sigma1(w3) + w14 + sigma0(w6));
    Round(c, d, e, f, g, h, a, b, 0x81c2c92e47edaee6ull, w6 += sigma1(w4) + w15 + sigma0(w7));
    Round(b, c, d, e, f, g, h, a, 0x92722c851482353bull, w7 += sigma1(w5) + w0 + sigma0(w8));
    Round(a, b, c, d, e, f, g, h, 0xa2bfe8a14cf10364ull, w8 += sigma1(w6) + w1 + sigma0(w9));
    Round(h, a, b, c, d, e, f, g, 0xa81a664bbc423001ull, w9 += sigma1(w7) + w2 + sigma0(w10));
    Round(g, h, a, b, c, d, e, f, 0xc24b8b70d0f89791ull, w10 += sigma1(w8) + w3 + sigma0(w11));
    Round(f, g, h, a, b, c, d, e, 0xc76c51a30654be30ull, w11 += sigma1(w9) + w4 + sigma0(w12));
    Round(e, f, g, h, a, b, c, d, 0xd192e819d6ef5218ull, w12 += sigma1(w10) + w5 + sigma0(w13));
    Round(d, e, f, g, h, a, b, c, 0xd69906245565a910ull, w13 += sigma1(w11) + w6 + sigma0(w14));
    Round(c, d, e, f, g, h, a, b, 0xf40e35855771202aull, w14 += sigma1(w12) + w7 + sigma0(w15));
    Round(b, c, d, e, f, g, h, a, 0x106aa07032bbd1b8ull, w15 += sigma1(w13) + w8 + sigma0(w0));

    Round(a, b, c, d, e, f, g, h, 0x19a4c116b8d2d0c8ull, w0 += sigma1(w14) + w9 + sigma0(w1));
    Round(h, a, b, c, d, e, f, g, 0x1e376c085141ab53ull, w1 += sigma1(w15) + w10 + sigma0(w2));
    Round(g, h, a, b, c, d, e, f, 0x2748774cdf8eeb99ull, w2 += sigma1(w0) + w11 + sigma0(w3));
    Round(f, g, h, a, b, c, d, e, 0x34b0bcb5e19b48a8ull, w3 += sigma1(w1) + w12 + sigma0(w4));
    Round(e, f, g, h, a, b, c, d, 0x391c0cb3c5c95a63ull, w4 += sigma1(w2) + w13 + sigma0(w5));
    Round(d, e, f, g, h, a, b, c, 0x4ed8aa4ae3418acbull, w5 += sigma1(w3) + w14 + sigma0(w6));
    Round(c, d, e, f, g, h, a, b, 0x5b9cca4f7763e373ull, w6 += sigma1(w4) + w15 + sigma0(w7));
    Round(b, c, d, e, f, g, h, a, 0x682e6ff3d6b2b8a3ull, w7 += sigma1(w5) + w0 + sigma0(w8));
    Round(a, b, c, d, e, f, g, h, 0x748f82ee5defb2fcull, w8 += sigma1(w6) + w1 + sigma0(w9));
    Round(h, a, b, c, d, e, f, g, 0x78a5636f43172f60ull, w9 += sigma1(w7) + w2 + sigma0(w10));
    Round(g, h, a, b, c, d, e, f, 0x84c87814a1f0ab72ull, w10 += sigma1(w8) + w3 + sigma0(w11));
    Round(f, g, h, a, b, c, d, e, 0x8cc702081a6439ecull, w11 += sigma1(w9) + w4 + sigma0(w12));
    Round(e, f, g, h, a, b, c, d, 0x90befffa23631e28ull, w12 += sigma1(w10) + w5 + sigma0(w13));
    Round(d, e, f, g, h, a, b, c, 0xa4506cebde82bde9ull, w13 += sigma1(w11) + w6 + sigma0(w14));
    Round(c, d, e, f, g, h, a, b, 0xbef9a3f7b2c67915ull, w14 += sigma1(w12) + w7 + sigma0(w15));
    Round(b, c, d, e, f, g, h, a, 0xc67178f2e372532bull, w15 += sigma1(w13) + w8 + sigma0(w0));

    Round(a, b, c, d, e, f, g, h, 0xca273eceea26619cull, w0 += sigma1(w14) + w9 + sigma0(w1));
    Round(h, a, b, c, d, e, f, g, 0xd186b8c721c0c207ull, w1 += sigma1(w15) + w10 + sigma0(w2));
    Round(g, h, a, b, c, d, e, f, 0xeada7dd6cde0eb1eull, w2 += sigma1(w0) + w11 + sigma0(w3));
    Round(f, g, h, a, b, c, d, e, 0xf57d4f7fee6ed178ull, w3 += sigma1(w1) + w12 + sigma0(w4));
    Round(e, f, g, h, a, b, c, d, 0x06f067aa72176fbaull, w4 += sigma1(w2) + w13 + sigma0(w5));
    Round(d, e, f, g, h, a, b, c, 0x0a637dc5a2c898a6ull, w5 += sigma1(w3) + w14 + sigma0(w6));
    Round(c, d, e, f, g, h, a, b, 0x113f9804bef90daeull, w6 += sigma1(w4) + w15 + sigma0(w7));
    Round(b, c, d, e, f, g, h, a, 0x1b710b35131c471bull, w7 += sigma1(w5) + w0 + sigma0(w8));
    Round(a, b, c, d, e, f, g, h, 0x28db77f523047d84ull, w8 += sigma1(w6) + w1 + sigma0(w9));
    Round(h, a, b, c, d, e, f, g, 0x32caab7b40c72493ull, w9 += sigma1(w7) + w2 + sigma0(w10));
    Round(g, h, a, b, c, d, e, f, 0x3c9ebe0a15c9bebcull, w10 += sigma1(w8) + w3 + sigma0(w11));
    Round(f, g, h, a, b, c, d, e, 0x431d67c49c100d4cull, w11 += sigma1(w9) + w4 + sigma0(w12));
    Round(e, f, g, h, a, b, c, d, 0x4cc5d4becb3e42b6ull, w12 += sigma1(w10) + w5 + sigma0(w13));
    Round(d, e, f, g, h, a, b, c, 0x597f299cfc657e2aull, w13 += sigma1(w11) + w6 + sigma0(w14));
    Round(c, d, e, f, g, h, a, b, 0x5fcb6fab3ad6faecull, w14 + sigma1(w12) + w7 + sigma0(w15));
    Round(b, c, d, e, f, g, h, a, 0x6c44198c4a475817ull, w15 + sigma1(w13) + w8 + sigma0(w0));

    s[0] += a;
    s[1] += b;
    s[2] += c;
    s[3] += d;
    s[4] += e;
    s[5] += f;
    s[6] += g;
    s[7] += h;
}

/** Define SHA512 hardware */
#if defined(__linux__)
#define HWCAP_SHA512  (1<<21)
#include <sys/auxv.h>
#elif defined(__WIN64__)
#include <intrin.h>
bool isAVX (void) {
  int cpuinfo[4];
  __cpuid(cpuinfo, 1);
  return ((cpuinfo[2] & (1 << 28)) != 0);
}
#endif

/** Define a function pointer for Transform */
void (*transform_ptr) (uint64_t*, const unsigned char*) = &Transform;

/** Initialize the function pointer */
void inline Initialize_transform_ptr(void)
{
// Override the function pointer for ARMV82/AVX2
#if (defined(USE_ARMV82) && defined(__APPLE__))
    if (sysctlbyname("hw.optional.arm.FEAT_SHA512", NULL, NULL, NULL, 0) == 0)
       transform_ptr = &sha512_armv82::Transform_ARMV82;
#elif defined(USE_ARMV82)
    if (getauxval(AT_HWCAP) & HWCAP_SHA512)
       transform_ptr = &sha512_armv82::Transform_ARMV82;
#elif USE_AVX2 && defined(__linux__)
    if (__builtin_cpu_supports("avx2"))
       transform_ptr = &Transform_AVX2;
#elif USE_AVX2 && defined(__WIN64__)
    if (isAVX)
       transform_ptr = &Transform_AVX2;
#endif
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
        sha512::transform_ptr(s, buf);
        bufsize = 0;
    }
    while (end >= data + 128) {
        // Process full chunks directly from the source.
        sha512::transform_ptr(s, data);
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

void detect_sha512_hardware()
{
    sha512::Initialize_transform_ptr();
}
