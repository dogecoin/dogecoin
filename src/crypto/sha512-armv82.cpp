// Copyright (c) 2014 The Bitcoin Core developers
// Copyright (c) 2022 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "crypto/sha512.h"

#include "crypto/common.h"
#include "support/experimental.h"

#include <string.h>

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

#ifdef USE_ARMV82

EXPERIMENTAL_FEATURE

/* ----------------------------------------------------------------------
 * Hardware-accelerated implementation of SHA-512 using Arm NEON.
 */

typedef struct sha512_neon_core sha512_neon_core;
struct sha512_neon_core {
    uint64x2_t ab, cd, ef, gh;
};

static inline uint64x2_t sha512_neon_load_input(const uint8_t *p)
{
    return vreinterpretq_u64_u8(vrev64q_u8(vld1q_u8(p)));
}

static inline uint64x2_t sha512_neon_schedule_update(
    uint64x2_t m8, uint64x2_t m7, uint64x2_t m4, uint64x2_t m3, uint64x2_t m1)
{
    /*
     * vsha512su0q_u64() takes words from a long way back in the
     * schedule and performs the sigma_0 half of the computation of
     * the next two 64-bit message-schedule words.
     *
     * vsha512su1q_u64() combines the result of that with the sigma_1
     * steps, to output the finished version of those two words. The
     * total amount of input data it requires fits nicely into three
     * 128-bit vector registers, but one of those registers is
     * misaligned compared to the 128-bit chunks that the message
     * schedule is stored in. So we use vextq_u64 to make one of its
     * input words out of the second half of m4 and the first half of
     * m3.
     */
    return vsha512su1q_u64(vsha512su0q_u64(m8, m7), m1, vextq_u64(m4, m3, 1));
}

static inline void sha512_neon_round2(
    unsigned round_index, uint64x2_t schedule_words,
    uint64x2_t *ab, uint64x2_t *cd, uint64x2_t *ef, uint64x2_t *gh)
{
    /*
     * vsha512hq_u64 performs the Sigma_1 and Ch half of the
     * computation of two rounds of SHA-512 (including feeding back
     * one of the outputs from the first of those half-rounds into the
     * second one).
     *
     * vsha512h2q_u64 combines the result of that with the Sigma_0 and
     * Maj steps, and outputs one 128-bit vector that replaces the gh
     * piece of the input hash state, and a second that updates cd by
     * addition.
     *
     * Similarly to vsha512su1q_u64 above, some of the input registers
     * expected by these instructions are misaligned by 64 bits
     * relative to the chunks we've divided the hash state into, so we
     * have to start by making 'de' and 'fg' words out of our input
     * cd,ef,gh, using vextq_u64.
     *
     * Also, one of the inputs to vsha512hq_u64 is expected to contain
     * the results of summing gh + two round constants + two words of
     * message schedule, but the two words of the message schedule
     * have to be the opposite way round in the vector register from
     * the way that vsha512su1q_u64 output them. Hence, there's
     * another vextq_u64 in here that swaps the two halves of the
     * initial_sum vector register.
     *
     * (This also means that I don't have to prepare a specially
     * reordered version of the sha512_round_constants[] array: as
     * long as I'm unavoidably doing a swap at run time _anyway_, I
     * can load from the normally ordered version of that array, and
     * just take care to fold in that data _before_ the swap rather
     * than after.)
     */

    /* Load two round constants, with the first one in the low half */
    uint64x2_t round_constants = vld1q_u64(
        sha512_round_constants + round_index);

    /* Add schedule words to round constants */
    uint64x2_t initial_sum = vaddq_u64(schedule_words, round_constants);

    /* Swap that sum around so the word used in the first of the two
     * rounds is in the _high_ half of the vector, matching where h
     * lives in the gh vector */
    uint64x2_t swapped_initial_sum = vextq_u64(initial_sum, initial_sum, 1);

    /* Add gh to that, now that they're matching ways round */
    uint64x2_t sum = vaddq_u64(swapped_initial_sum, *gh);

    /* Make the misaligned de and fg words */
    uint64x2_t de = vextq_u64(*cd, *ef, 1);
    uint64x2_t fg = vextq_u64(*ef, *gh, 1);

    /* Now we're ready to put all the pieces together. The output from
     * vsha512h2q_u64 can be used directly as the new gh, and the
     * output from vsha512hq_u64 is simultaneously the intermediate
     * value passed to h2 and the thing you have to add on to cd. */
    uint64x2_t intermed = vsha512hq_u64(sum, fg, de);
    *gh = vsha512h2q_u64(intermed, *cd, *ab);
    *cd = vaddq_u64(*cd, intermed);
}

static inline void sha512_neon_block(sha512_neon_core *core, const uint8_t *p)
{
    uint64x2_t s0, s1, s2, s3, s4, s5, s6, s7;

    uint64x2_t ab = core->ab, cd = core->cd, ef = core->ef, gh = core->gh;

    s0 = sha512_neon_load_input(p + 16*0);
    sha512_neon_round2(0, s0, &ab, &cd, &ef, &gh);
    s1 = sha512_neon_load_input(p + 16*1);
    sha512_neon_round2(2, s1, &gh, &ab, &cd, &ef);
    s2 = sha512_neon_load_input(p + 16*2);
    sha512_neon_round2(4, s2, &ef, &gh, &ab, &cd);
    s3 = sha512_neon_load_input(p + 16*3);
    sha512_neon_round2(6, s3, &cd, &ef, &gh, &ab);
    s4 = sha512_neon_load_input(p + 16*4);
    sha512_neon_round2(8, s4, &ab, &cd, &ef, &gh);
    s5 = sha512_neon_load_input(p + 16*5);
    sha512_neon_round2(10, s5, &gh, &ab, &cd, &ef);
    s6 = sha512_neon_load_input(p + 16*6);
    sha512_neon_round2(12, s6, &ef, &gh, &ab, &cd);
    s7 = sha512_neon_load_input(p + 16*7);
    sha512_neon_round2(14, s7, &cd, &ef, &gh, &ab);
    s0 = sha512_neon_schedule_update(s0, s1, s4, s5, s7);
    sha512_neon_round2(16, s0, &ab, &cd, &ef, &gh);
    s1 = sha512_neon_schedule_update(s1, s2, s5, s6, s0);
    sha512_neon_round2(18, s1, &gh, &ab, &cd, &ef);
    s2 = sha512_neon_schedule_update(s2, s3, s6, s7, s1);
    sha512_neon_round2(20, s2, &ef, &gh, &ab, &cd);
    s3 = sha512_neon_schedule_update(s3, s4, s7, s0, s2);
    sha512_neon_round2(22, s3, &cd, &ef, &gh, &ab);
    s4 = sha512_neon_schedule_update(s4, s5, s0, s1, s3);
    sha512_neon_round2(24, s4, &ab, &cd, &ef, &gh);
    s5 = sha512_neon_schedule_update(s5, s6, s1, s2, s4);
    sha512_neon_round2(26, s5, &gh, &ab, &cd, &ef);
    s6 = sha512_neon_schedule_update(s6, s7, s2, s3, s5);
    sha512_neon_round2(28, s6, &ef, &gh, &ab, &cd);
    s7 = sha512_neon_schedule_update(s7, s0, s3, s4, s6);
    sha512_neon_round2(30, s7, &cd, &ef, &gh, &ab);
    s0 = sha512_neon_schedule_update(s0, s1, s4, s5, s7);
    sha512_neon_round2(32, s0, &ab, &cd, &ef, &gh);
    s1 = sha512_neon_schedule_update(s1, s2, s5, s6, s0);
    sha512_neon_round2(34, s1, &gh, &ab, &cd, &ef);
    s2 = sha512_neon_schedule_update(s2, s3, s6, s7, s1);
    sha512_neon_round2(36, s2, &ef, &gh, &ab, &cd);
    s3 = sha512_neon_schedule_update(s3, s4, s7, s0, s2);
    sha512_neon_round2(38, s3, &cd, &ef, &gh, &ab);
    s4 = sha512_neon_schedule_update(s4, s5, s0, s1, s3);
    sha512_neon_round2(40, s4, &ab, &cd, &ef, &gh);
    s5 = sha512_neon_schedule_update(s5, s6, s1, s2, s4);
    sha512_neon_round2(42, s5, &gh, &ab, &cd, &ef);
    s6 = sha512_neon_schedule_update(s6, s7, s2, s3, s5);
    sha512_neon_round2(44, s6, &ef, &gh, &ab, &cd);
    s7 = sha512_neon_schedule_update(s7, s0, s3, s4, s6);
    sha512_neon_round2(46, s7, &cd, &ef, &gh, &ab);
    s0 = sha512_neon_schedule_update(s0, s1, s4, s5, s7);
    sha512_neon_round2(48, s0, &ab, &cd, &ef, &gh);
    s1 = sha512_neon_schedule_update(s1, s2, s5, s6, s0);
    sha512_neon_round2(50, s1, &gh, &ab, &cd, &ef);
    s2 = sha512_neon_schedule_update(s2, s3, s6, s7, s1);
    sha512_neon_round2(52, s2, &ef, &gh, &ab, &cd);
    s3 = sha512_neon_schedule_update(s3, s4, s7, s0, s2);
    sha512_neon_round2(54, s3, &cd, &ef, &gh, &ab);
    s4 = sha512_neon_schedule_update(s4, s5, s0, s1, s3);
    sha512_neon_round2(56, s4, &ab, &cd, &ef, &gh);
    s5 = sha512_neon_schedule_update(s5, s6, s1, s2, s4);
    sha512_neon_round2(58, s5, &gh, &ab, &cd, &ef);
    s6 = sha512_neon_schedule_update(s6, s7, s2, s3, s5);
    sha512_neon_round2(60, s6, &ef, &gh, &ab, &cd);
    s7 = sha512_neon_schedule_update(s7, s0, s3, s4, s6);
    sha512_neon_round2(62, s7, &cd, &ef, &gh, &ab);
    s0 = sha512_neon_schedule_update(s0, s1, s4, s5, s7);
    sha512_neon_round2(64, s0, &ab, &cd, &ef, &gh);
    s1 = sha512_neon_schedule_update(s1, s2, s5, s6, s0);
    sha512_neon_round2(66, s1, &gh, &ab, &cd, &ef);
    s2 = sha512_neon_schedule_update(s2, s3, s6, s7, s1);
    sha512_neon_round2(68, s2, &ef, &gh, &ab, &cd);
    s3 = sha512_neon_schedule_update(s3, s4, s7, s0, s2);
    sha512_neon_round2(70, s3, &cd, &ef, &gh, &ab);
    s4 = sha512_neon_schedule_update(s4, s5, s0, s1, s3);
    sha512_neon_round2(72, s4, &ab, &cd, &ef, &gh);
    s5 = sha512_neon_schedule_update(s5, s6, s1, s2, s4);
    sha512_neon_round2(74, s5, &gh, &ab, &cd, &ef);
    s6 = sha512_neon_schedule_update(s6, s7, s2, s3, s5);
    sha512_neon_round2(76, s6, &ef, &gh, &ab, &cd);
    s7 = sha512_neon_schedule_update(s7, s0, s3, s4, s6);
    sha512_neon_round2(78, s7, &cd, &ef, &gh, &ab);

    core->ab = vaddq_u64(core->ab, ab);
    core->cd = vaddq_u64(core->cd, cd);
    core->ef = vaddq_u64(core->ef, ef);
    core->gh = vaddq_u64(core->gh, gh);
}
#endif

namespace sha512_armv82
{

/** Perform one SHA-512 transformation, processing a 128-byte chunk. (ARMv8.2) */
void Transform_ARMV82(uint64_t* s, const unsigned char* chunk)
{
#ifdef USE_ARMV82
    sha512_neon_core core;

    core.ab = vld1q_u64(s);
    core.cd = vld1q_u64(s+2);
    core.ef = vld1q_u64(s+4);
    core.gh = vld1q_u64(s+6);

    // Perform SHA512 one block (ARMv8.2)
    sha512_neon_block(&core, chunk);

    s[0] = vgetq_lane_u64 (core.ab, 0);
    s[1] = vgetq_lane_u64 (core.ab, 1);
    s[2] = vgetq_lane_u64 (core.cd, 0);
    s[3] = vgetq_lane_u64 (core.cd, 1);
    s[4] = vgetq_lane_u64 (core.ef, 0);
    s[5] = vgetq_lane_u64 (core.ef, 1);
    s[6] = vgetq_lane_u64 (core.gh, 0);
    s[7] = vgetq_lane_u64 (core.gh, 1);
#endif
}

}

