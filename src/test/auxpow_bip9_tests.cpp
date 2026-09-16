// Copyright (c) 2026 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

//
// AuxPoW <-> BIP9 (versionbits) compatibility, see issue #1340.
//
// Dogecoin and BIP9 both claim the block header's nVersion field, and they
// disagree about what it means:
//
//   * AuxPoW reads the top 16 bits as a merge-mining chain ID, bit 8 as the
//     "this block carries an auxpow" flag, and the bottom 8 bits as the base
//     version. A mainnet block is 0x0062xxxx.
//   * BIP9 puts 001 in the top three bits and hands bits 0-28 to soft-fork
//     deployments to signal in. A signalling block is 0x2xxxxxxx.
//
// Those two layouts cannot both be true of the same block, which is why
// ComputeBlockVersion() is commented out in CreateNewBlock() with a FIXME
// pointing at the always-auxpow fork.
//
// None of this is a consensus change. These tests just pin the current
// behaviour down so that the constraints on any future fix are written in
// code that runs, rather than in a decade-old issue thread. Two questions
// raised in #1340 and never conclusively answered are settled below:
// whether another chain's BIP9 soft-fork can collide with our chain ID
// (it cannot), and what the chain ID test actually looks like (an equality
// on the top 16 bits, not a mask).
//

#include "arith_uint256.h"
#include "chainparams.h"
#include "consensus/params.h"
#include "dogecoin.h"
#include "primitives/block.h"
#include "primitives/pureheader.h"
#include "versionbits.h"

#include "test/test_bitcoin.h"

#include <boost/test/unit_test.hpp>

BOOST_FIXTURE_TEST_SUITE(auxpow_bip9_tests, BasicTestingSetup)

/** A height well after merge-mining started, so AuxPoW rules are in force. */
static const uint32_t AUXPOW_HEIGHT = 371337;

/**
 * A BIP9 block version is not a well-formed Dogecoin block version: the chain
 * ID Dogecoin reads out of it is wrong, and the base version is 0.
 */
BOOST_AUTO_TEST_CASE(bip9_version_is_malformed_to_auxpow)
{
    const Consensus::Params& params = Params(CBaseChainParams::MAIN).GetConsensus(AUXPOW_HEIGHT);

    // The bare BIP9 version, before any deployment sets a bit.
    CPureBlockHeader header;
    header.nVersion = VERSIONBITS_TOP_BITS;

    // Dogecoin reads the top 16 bits as the chain ID. For a BIP9 version that
    // is 0x2000, not the 0x0062 this chain uses.
    BOOST_CHECK_EQUAL(header.GetChainId(), 0x2000);
    BOOST_CHECK(header.GetChainId() != params.nAuxpowChainId);

    // The bottom 8 bits are the base version, and BIP9 leaves them clear, so
    // the block also looks like version 0 - below the BIP34/66/65 minimums.
    BOOST_CHECK_EQUAL(CPureBlockHeader::GetBaseVersion(VERSIONBITS_TOP_BITS), 0);
}

/**
 * The consequence: a block carrying a BIP9 version is rejected outright. This
 * is the test that guards the FIXME in CreateNewBlock() - uncommenting the
 * ComputeBlockVersion() call there would produce blocks this chain refuses.
 */
BOOST_AUTO_TEST_CASE(bip9_block_is_rejected_by_auxpow_rules)
{
    // Uses regtest, whose difficulty target is low enough that any header
    // satisfies the proof of work. That isolates the version: the two headers
    // below differ in nothing else, so the differing verdict is attributable
    // to nVersion alone rather than to hashing.
    const Consensus::Params& params = Params(CBaseChainParams::REGTEST).GetConsensus(0);

    // The chain ID rule below only bites while this holds.
    BOOST_CHECK(params.fStrictChainId);

    CBlockHeader block;
    block.SetNull();
    block.nBits = UintToArith256(params.powLimit).GetCompact();
    block.nTime = 1400000000;

    // A version this chain actually mines is accepted.
    block.SetBaseVersion(VERSIONBITS_LAST_OLD_BLOCK_VERSION, params.nAuxpowChainId);
    BOOST_CHECK(CheckAuxPowProofOfWork(block, params));

    // The same header carrying a BIP9 version is not, because the chain ID
    // read out of it is 0x2000 rather than 0x0062. This is the check that
    // would start failing if the ComputeBlockVersion() call in
    // CreateNewBlock() were simply uncommented.
    block.nVersion = VERSIONBITS_TOP_BITS
                     | (1 << params.vDeployments[Consensus::DEPLOYMENT_CSV].bit);
    BOOST_CHECK(!block.IsLegacy());
    BOOST_CHECK(!CheckAuxPowProofOfWork(block, params));
}

/**
 * Conversely, a well-formed Dogecoin block carries no BIP9 signal at all: the
 * top three bits are 0x0000, so versionbits never counts it as signalling.
 */
BOOST_AUTO_TEST_CASE(dogecoin_version_carries_no_bip9_signal)
{
    const Consensus::Params& params = Params(CBaseChainParams::MAIN).GetConsensus(AUXPOW_HEIGHT);

    CPureBlockHeader header;
    header.SetBaseVersion(VERSIONBITS_LAST_OLD_BLOCK_VERSION, params.nAuxpowChainId);

    // This is what CreateNewBlock() actually mines today.
    BOOST_CHECK_EQUAL(header.nVersion, 0x00620004);
    BOOST_CHECK_EQUAL(header.GetChainId(), params.nAuxpowChainId);
    BOOST_CHECK_EQUAL(header.GetBaseVersion(), VERSIONBITS_LAST_OLD_BLOCK_VERSION);

    // VersionBitsConditionChecker only counts a block as signalling when the
    // top three bits are 001. A Dogecoin block never satisfies that, so every
    // deployment sits at zero support no matter how many blocks are mined.
    BOOST_CHECK((header.nVersion & VERSIONBITS_TOP_MASK) != VERSIONBITS_TOP_BITS);
}

/**
 * #1340 asks whether another merge-mining parent chain running a BIP9
 * soft-fork could trip "Aux POW parent has our chain ID". It cannot: the 001
 * BIP9 prefix forces the readable chain ID into 0x2000-0x3FFF, and 0x0062 is
 * outside that window. Checked exhaustively over every chain ID a BIP9 version
 * can produce.
 */
BOOST_AUTO_TEST_CASE(bip9_parent_can_never_claim_our_chain_id)
{
    const Consensus::Params& params = Params(CBaseChainParams::MAIN).GetConsensus(AUXPOW_HEIGHT);

    // Only bits 16-28 of a BIP9 version reach the chain ID; the low 16 bits
    // cannot change it, so enumerating those 13 bits is exhaustive.
    for (int32_t high = 0; high <= 0x1FFF; ++high) {
        CPureBlockHeader parent;
        parent.nVersion = VERSIONBITS_TOP_BITS | (high << 16);

        BOOST_CHECK_EQUAL(parent.GetChainId(), 0x2000 | high);
        BOOST_CHECK(parent.GetChainId() != params.nAuxpowChainId);
    }
}

/**
 * The reverse direction is the one that constrains us. VERSION_AUXPOW is bit
 * 8, which sits inside the range BIP9 hands to deployments, so Dogecoin can
 * never deploy a soft-fork on bit 8: every signalling block would claim to
 * carry an auxpow it does not have and be rejected. This guards that no
 * deployment is ever configured there.
 */
BOOST_AUTO_TEST_CASE(no_deployment_may_use_the_auxpow_flag_bit)
{
    const int32_t auxpowFlag = CPureBlockHeader::VERSION_AUXPOW;
    const int auxpowBit = 8;

    BOOST_CHECK_EQUAL(auxpowFlag, 1 << auxpowBit);

    // Inside BIP9's range, so the collision is real rather than theoretical.
    BOOST_CHECK(auxpowBit <= 28);

    const std::string chains[] = {
        CBaseChainParams::MAIN,
        CBaseChainParams::TESTNET,
        CBaseChainParams::REGTEST,
    };

    for (const std::string& chain : chains) {
        const Consensus::Params& params = Params(chain).GetConsensus(AUXPOW_HEIGHT);
        for (int i = 0; i < (int)Consensus::MAX_VERSION_BITS_DEPLOYMENTS; ++i) {
            BOOST_CHECK_MESSAGE(params.vDeployments[i].bit != auxpowBit,
                                "deployment " << i << " on " << chain
                                << " uses the auxpow flag bit");
        }
    }
}

BOOST_AUTO_TEST_SUITE_END()
