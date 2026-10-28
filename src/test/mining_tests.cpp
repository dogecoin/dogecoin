// Copyright (c) 2026 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "base58.h"
#include "key.h"
#include "pubkey.h"
#include "rpc/server.h"
#include "script/script.h"
#include "validation.h"
#include "validationinterface.h"
#include "test/test_bitcoin.h"

#include <boost/test/unit_test.hpp>

#include <univalue.h>

// Defined in rpc_tests.cpp; looks up the RPC in tableRPC and invokes it,
// rethrowing a JSONRPCError's "message" field as a std::runtime_error
extern UniValue CallRPC(std::string args);

namespace {

//! A chain of just the genesis block is always stale relative to !
//DEFAULT_MAX_TIP_AGE, so IsInitialBlockDownload() returns true.
struct RegtestGenesisOnlySetup : public TestingSetup {
    RegtestGenesisOnlySetup() : TestingSetup(CBaseChainParams::REGTEST) {}
};

//! Stands in for the wallet's ScriptForMining handler so generate/
//! generatetoaddress have something to mine to.
class DummyMiningScript : public CValidationInterface
{
protected:
    void GetScriptForMining(std::shared_ptr<CReserveScript>& script) override
    {
        auto reserveScript = std::make_shared<CReserveScript>();
        reserveScript->reserveScript = CScript() << OP_TRUE;
        script = reserveScript;
    }
};

//! Restores fReindex/fImporting however the test exits.
struct ReindexFlagGuard {
    const bool fSavedReindex;
    const bool fSavedImporting;
    ReindexFlagGuard() : fSavedReindex(fReindex), fSavedImporting(fImporting) {}
    ~ReindexFlagGuard() { fReindex = fSavedReindex; fImporting = fSavedImporting; }
};

std::string NewRegtestAddress()
{
    CKey key;
    key.MakeNewKey(true);
    return CBitcoinAddress(key.GetPubKey().GetID()).ToString();
}

} // namespace

BOOST_AUTO_TEST_SUITE(mining_tests)

// Regression test for a fork-before-checkpoint incident: mining or accepting a
// submitted block while a reindex/import is still reconstructing the chain
// builds new history on a tip that is about to move once the reindex catches
// up, which can fork the chain before a hardcoded checkpoint.
BOOST_FIXTURE_TEST_CASE(reindex_guard_blocks_generate, RegtestGenesisOnlySetup)
{
    ReindexFlagGuard flagGuard;
    DummyMiningScript miningScript;
    RegisterValidationInterface(&miningScript);

    const std::string address = NewRegtestAddress();

    fReindex = true;
    fImporting = false;
    BOOST_CHECK_THROW(CallRPC("generate 1"), std::runtime_error);
    BOOST_CHECK_THROW(CallRPC("generatetoaddress 1 " + address), std::runtime_error);

    fReindex = false;
    fImporting = true;
    BOOST_CHECK_THROW(CallRPC("generate 1"), std::runtime_error);

    UnregisterValidationInterface(&miningScript);
}

BOOST_FIXTURE_TEST_CASE(reindex_guard_blocks_submitblock, RegtestGenesisOnlySetup)
{
    ReindexFlagGuard flagGuard;

    // The guard runs before hex-decoding, so a placeholder hexdata argument is
    // enough.
    fReindex = true;
    fImporting = false;
    BOOST_CHECK_THROW(CallRPC("submitblock deadbeef"), std::runtime_error);

    fReindex = false;
    fImporting = true;
    BOOST_CHECK_THROW(CallRPC("submitblock deadbeef"), std::runtime_error);
}

// Once the reindex/import finishes, generate must go back to working normally.
BOOST_FIXTURE_TEST_CASE(generate_works_once_reindex_flags_clear, TestChain240Setup)
{
    ReindexFlagGuard flagGuard;
    DummyMiningScript miningScript;
    RegisterValidationInterface(&miningScript);

    fReindex = false;
    fImporting = false;
    UniValue result;
    BOOST_CHECK_NO_THROW(result = CallRPC("generate 1"));
    BOOST_CHECK_EQUAL(result.size(), 1u);

    UnregisterValidationInterface(&miningScript);
}

BOOST_AUTO_TEST_SUITE_END()
