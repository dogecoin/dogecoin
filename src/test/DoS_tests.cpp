// Copyright (c) 2011-2016 The Bitcoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

// Unit tests for denial-of-service detection/prevention code

#include "chainparams.h"
#include "keystore.h"
#include "net.h"
#include "net_processing.h"
#include "pow.h"
#include "script/sign.h"
#include "serialize.h"
#include "util.h"
#include "validation.h"

#include "test/test_bitcoin.h"

#include <stdint.h>

#include <boost/assign/list_of.hpp> // for 'map_list_of()'
#include <boost/date_time/posix_time/posix_time_types.hpp>
#include <boost/foreach.hpp>
#include <boost/test/unit_test.hpp>

// Tests these internal-to-net_processing.cpp methods:
extern bool AddOrphanTx(const CTransactionRef& tx, NodeId peer);
extern void EraseOrphansFor(NodeId peer);
extern unsigned int LimitOrphanTxSize(unsigned int nMaxOrphans);
struct COrphanTx {
    CTransactionRef tx;
    NodeId fromPeer;
    int64_t nTimeExpire;
};
extern std::map<uint256, COrphanTx> mapOrphanTransactions;

CService ip(uint32_t i)
{
    struct in_addr s;
    s.s_addr = i;
    return CService(CNetAddr(s), Params().GetDefaultPort());
}

static NodeId id = 0;

BOOST_FIXTURE_TEST_SUITE(DoS_tests, TestingSetup)

static size_t BanListSize(CConnman* connman)
{
    banmap_t banmap;
    connman->GetBanned(banmap);
    return banmap.size();
}

BOOST_AUTO_TEST_CASE(DoS_banning)
{
    std::atomic<bool> interruptDummy(false);

    connman->ClearBanned();
    connman->ClearDiscouraged();
    CAddress addr1(ip(0xa0b0c001), NODE_NONE);
    CNode dummyNode1(id++, NODE_NETWORK, 0, INVALID_SOCKET, addr1, 0, 0, "", true);
    dummyNode1.SetSendVersion(PROTOCOL_VERSION);
    GetNodeSignals().InitializeNode(&dummyNode1, *connman);
    dummyNode1.nVersion = 1;
    dummyNode1.fSuccessfullyConnected = true;
    Misbehaving(dummyNode1.GetId(), 100); // Should get discouraged
    SendMessages(&dummyNode1, *connman, interruptDummy);
    BOOST_CHECK(dummyNode1.fDisconnect);
    BOOST_CHECK(connman->IsDiscouraged(addr1));
    BOOST_CHECK(!connman->IsDiscouraged(ip(0xa0b0c001|0x0000ff00))); // Different IP, not discouraged
    // Discouragement is not a ban
    BOOST_CHECK(!connman->IsBanned(addr1));
    BOOST_CHECK_EQUAL(BanListSize(connman), 0U);

    CAddress addr2(ip(0xa0b0c002), NODE_NONE);
    CNode dummyNode2(id++, NODE_NETWORK, 0, INVALID_SOCKET, addr2, 1, 1, "", true);
    dummyNode2.SetSendVersion(PROTOCOL_VERSION);
    GetNodeSignals().InitializeNode(&dummyNode2, *connman);
    dummyNode2.nVersion = 1;
    dummyNode2.fSuccessfullyConnected = true;
    Misbehaving(dummyNode2.GetId(), 50);
    SendMessages(&dummyNode2, *connman, interruptDummy);
    BOOST_CHECK(!dummyNode2.fDisconnect);
    BOOST_CHECK(!connman->IsDiscouraged(addr2)); // 2 not discouraged yet...
    BOOST_CHECK(connman->IsDiscouraged(addr1));  // ... but 1 still should be
    Misbehaving(dummyNode2.GetId(), 50);
    SendMessages(&dummyNode2, *connman, interruptDummy);
    BOOST_CHECK(connman->IsDiscouraged(addr2));
    BOOST_CHECK_EQUAL(BanListSize(connman), 0U);
}

BOOST_AUTO_TEST_CASE(DoS_banscore)
{
    std::atomic<bool> interruptDummy(false);

    connman->ClearBanned();
    connman->ClearDiscouraged();
    ForceSetArg("-banscore", "111"); // because 11 is my favorite number
    CAddress addr1(ip(0xa0b0c001), NODE_NONE);
    CNode dummyNode1(id++, NODE_NETWORK, 0, INVALID_SOCKET, addr1, 3, 1, "", true);
    dummyNode1.SetSendVersion(PROTOCOL_VERSION);
    GetNodeSignals().InitializeNode(&dummyNode1, *connman);
    dummyNode1.nVersion = 1;
    dummyNode1.fSuccessfullyConnected = true;
    Misbehaving(dummyNode1.GetId(), 100);
    SendMessages(&dummyNode1, *connman, interruptDummy);
    BOOST_CHECK(!connman->IsDiscouraged(addr1));
    Misbehaving(dummyNode1.GetId(), 10);
    SendMessages(&dummyNode1, *connman, interruptDummy);
    BOOST_CHECK(!connman->IsDiscouraged(addr1));
    Misbehaving(dummyNode1.GetId(), 1);
    SendMessages(&dummyNode1, *connman, interruptDummy);
    BOOST_CHECK(connman->IsDiscouraged(addr1));
    BOOST_CHECK(!connman->IsBanned(addr1));
    ForceSetArg("-banscore", std::to_string(DEFAULT_BANSCORE_THRESHOLD));
}

// Misbehaviour never times out of the discouragement filter by wall clock,
// while manual bans still expire after -bantime.
BOOST_AUTO_TEST_CASE(DoS_bantime)
{
    std::atomic<bool> interruptDummy(false);

    connman->ClearBanned();
    connman->ClearDiscouraged();
    int64_t nStartTime = GetTime();
    SetMockTime(nStartTime); // Overrides future calls to GetTime()

    CAddress addr(ip(0xa0b0c001), NODE_NONE);
    CNode dummyNode(id++, NODE_NETWORK, 0, INVALID_SOCKET, addr, 4, 4, "", true);
    dummyNode.SetSendVersion(PROTOCOL_VERSION);
    GetNodeSignals().InitializeNode(&dummyNode, *connman);
    dummyNode.nVersion = 1;
    dummyNode.fSuccessfullyConnected = true;

    Misbehaving(dummyNode.GetId(), 100);
    SendMessages(&dummyNode, *connman, interruptDummy);
    BOOST_CHECK(connman->IsDiscouraged(addr));
    BOOST_CHECK(!connman->IsBanned(addr));

    SetMockTime(nStartTime+60*60*24+1);
    BOOST_CHECK(connman->IsDiscouraged(addr));

    // Manual bans keep using -bantime (default 24h)
    CAddress addr_manual(ip(0xa0b0c003), NODE_NONE);
    SetMockTime(nStartTime);
    connman->Ban(addr_manual, BanReasonManuallyAdded);
    BOOST_CHECK(connman->IsBanned(addr_manual));
    BOOST_CHECK(!connman->IsDiscouraged(addr_manual));
    SetMockTime(nStartTime+60*60);
    BOOST_CHECK(connman->IsBanned(addr_manual));
    SetMockTime(nStartTime+60*60*24+1);
    BOOST_CHECK(!connman->IsBanned(addr_manual));
    SetMockTime(0);
    connman->ClearBanned();
}

// CVE-2020-14198: many misbehaving peers must not grow the ban list, and the
// discouragement filter must stay bounded.
BOOST_AUTO_TEST_CASE(DoS_discourage_does_not_grow_banlist)
{
    std::atomic<bool> interruptDummy(false);

    connman->ClearBanned();
    connman->ClearDiscouraged();
    const int nPeers = 200;
    for (int i = 0; i < nPeers; i++) {
        CAddress addr(ip(0x00000a0b | ((uint32_t)i << 24)), NODE_NONE); // 11.10.0.i
        CNode node(id++, NODE_NETWORK, 0, INVALID_SOCKET, addr, 5, 5, "", true);
        node.SetSendVersion(PROTOCOL_VERSION);
        GetNodeSignals().InitializeNode(&node, *connman);
        node.nVersion = 1;
        node.fSuccessfullyConnected = true;
        {
            LOCK(cs_main);
            Misbehaving(node.GetId(), 100);
        }
        SendMessages(&node, *connman, interruptDummy);
        BOOST_CHECK(node.fDisconnect);
        BOOST_CHECK(connman->IsDiscouraged(addr));
        bool fUpdateConnectionTime = false;
        GetNodeSignals().FinalizeNode(node.GetId(), fUpdateConnectionTime);
    }
    BOOST_CHECK_EQUAL(BanListSize(connman), 0U);
    connman->ClearDiscouraged();
}

// Whitelisted and addnode (manual) peers are not punished; local peers are
// disconnected but not discouraged (same exemptions as bitcoin#19219).
BOOST_AUTO_TEST_CASE(DoS_discourage_exemptions)
{
    std::atomic<bool> interruptDummy(false);

    connman->ClearBanned();
    connman->ClearDiscouraged();

    CAddress addr_wl(ip(0xa0b0c011), NODE_NONE);
    CNode node_wl(id++, NODE_NETWORK, 0, INVALID_SOCKET, addr_wl, 6, 6, "", true);
    node_wl.fWhitelisted = true;
    CAddress addr_an(ip(0xa0b0c012), NODE_NONE);
    CNode node_an(id++, NODE_NETWORK, 0, INVALID_SOCKET, addr_an, 7, 7, "", true);
    node_an.fAddnode = true;
    CAddress addr_local(ip(0x0100007f), NODE_NONE); // 127.0.0.1
    CNode node_local(id++, NODE_NETWORK, 0, INVALID_SOCKET, addr_local, 8, 8, "", true);

    for (CNode* node : {&node_wl, &node_an, &node_local}) {
        node->SetSendVersion(PROTOCOL_VERSION);
        GetNodeSignals().InitializeNode(node, *connman);
        node->nVersion = 1;
        node->fSuccessfullyConnected = true;
        {
            LOCK(cs_main);
            Misbehaving(node->GetId(), 100);
        }
        SendMessages(node, *connman, interruptDummy);
    }
    BOOST_CHECK(!node_wl.fDisconnect);
    BOOST_CHECK(!connman->IsDiscouraged(addr_wl));
    BOOST_CHECK(!node_an.fDisconnect);
    BOOST_CHECK(!connman->IsDiscouraged(addr_an));
    BOOST_CHECK(node_local.fDisconnect);
    BOOST_CHECK(!connman->IsDiscouraged(addr_local));
    BOOST_CHECK_EQUAL(BanListSize(connman), 0U);

    for (CNode* node : {&node_wl, &node_an, &node_local}) {
        bool fUpdateConnectionTime = false;
        GetNodeSignals().FinalizeNode(node->GetId(), fUpdateConnectionTime);
    }
}

// Discouraged inbound peers are refused only when the inbound slots are
// (almost) full; whitelisted peers are never refused for being discouraged.
BOOST_AUTO_TEST_CASE(DoS_discouraged_inbound_admission)
{
    const int nMaxInbound = 117;
    // Not discouraged: never refused by this rule.
    BOOST_CHECK(!ShouldRefuseDiscouragedInbound(false, false, 0, nMaxInbound));
    BOOST_CHECK(!ShouldRefuseDiscouragedInbound(false, false, nMaxInbound, nMaxInbound));
    // Discouraged: accepted while there is room...
    BOOST_CHECK(!ShouldRefuseDiscouragedInbound(true, false, 0, nMaxInbound));
    BOOST_CHECK(!ShouldRefuseDiscouragedInbound(true, false, nMaxInbound - 2, nMaxInbound));
    // ...refused when (almost) full.
    BOOST_CHECK(ShouldRefuseDiscouragedInbound(true, false, nMaxInbound - 1, nMaxInbound));
    BOOST_CHECK(ShouldRefuseDiscouragedInbound(true, false, nMaxInbound, nMaxInbound));
    // Whitelisted: never refused for being discouraged.
    BOOST_CHECK(!ShouldRefuseDiscouragedInbound(true, true, nMaxInbound, nMaxInbound));
}

// After the protections, AttemptToEvictConnection keeps only the candidates
// preferred for eviction (discouraged peers), if any remain (bitcoin#14929).
namespace {
struct TestEvictionCandidate {
    int id;
    bool prefer_evict;
};
std::vector<int> CandidateIds(const std::vector<TestEvictionCandidate>& candidates)
{
    std::vector<int> ids;
    for (const TestEvictionCandidate& c : candidates) ids.push_back(c.id);
    return ids;
}
} // namespace

BOOST_AUTO_TEST_CASE(DoS_prefer_evict_filter)
{
    // No preferred candidate: all candidates remain, in order.
    std::vector<TestEvictionCandidate> none = {{1, false}, {2, false}, {3, false}};
    KeepOnlyPreferredEvictionCandidates(none);
    BOOST_CHECK(CandidateIds(none) == std::vector<int>({1, 2, 3}));

    // Some preferred: only they remain, in order.
    std::vector<TestEvictionCandidate> some = {{1, false}, {2, true}, {3, false}, {4, true}};
    KeepOnlyPreferredEvictionCandidates(some);
    BOOST_CHECK(CandidateIds(some) == std::vector<int>({2, 4}));

    // All preferred: all remain.
    std::vector<TestEvictionCandidate> all = {{1, true}, {2, true}};
    KeepOnlyPreferredEvictionCandidates(all);
    BOOST_CHECK(CandidateIds(all) == std::vector<int>({1, 2}));

    // Empty: stays empty.
    std::vector<TestEvictionCandidate> empty;
    KeepOnlyPreferredEvictionCandidates(empty);
    BOOST_CHECK(empty.empty());
}

CTransactionRef RandomOrphan()
{
    std::map<uint256, COrphanTx>::iterator it;
    it = mapOrphanTransactions.lower_bound(InsecureRand256());
    if (it == mapOrphanTransactions.end())
        it = mapOrphanTransactions.begin();
    return it->second.tx;
}

BOOST_AUTO_TEST_CASE(DoS_mapOrphans)
{
    CKey key;
    key.MakeNewKey(true);
    CBasicKeyStore keystore;
    keystore.AddKey(key);

    // 50 orphan transactions:
    for (int i = 0; i < 50; i++)
    {
        CMutableTransaction tx;
        tx.vin.resize(1);
        tx.vin[0].prevout.n = 0;
        tx.vin[0].prevout.hash = InsecureRand256();
        tx.vin[0].scriptSig << OP_1;
        tx.vout.resize(1);
        tx.vout[0].nValue = 1*CENT;
        tx.vout[0].scriptPubKey = GetScriptForDestination(key.GetPubKey().GetID());

        AddOrphanTx(MakeTransactionRef(tx), i);
    }

    // ... and 50 that depend on other orphans:
    for (int i = 0; i < 50; i++)
    {
        CTransactionRef txPrev = RandomOrphan();

        CMutableTransaction tx;
        tx.vin.resize(1);
        tx.vin[0].prevout.n = 0;
        tx.vin[0].prevout.hash = txPrev->GetHash();
        tx.vout.resize(1);
        tx.vout[0].nValue = 1*CENT;
        tx.vout[0].scriptPubKey = GetScriptForDestination(key.GetPubKey().GetID());
        SignSignature(keystore, *txPrev, tx, 0, SIGHASH_ALL);

        AddOrphanTx(MakeTransactionRef(tx), i);
    }

    // This really-big orphan should be ignored:
    for (int i = 0; i < 10; i++)
    {
        CTransactionRef txPrev = RandomOrphan();

        CMutableTransaction tx;
        tx.vout.resize(1);
        tx.vout[0].nValue = 1*CENT;
        tx.vout[0].scriptPubKey = GetScriptForDestination(key.GetPubKey().GetID());
        tx.vin.resize(2777);
        for (unsigned int j = 0; j < tx.vin.size(); j++)
        {
            tx.vin[j].prevout.n = j;
            tx.vin[j].prevout.hash = txPrev->GetHash();
        }
        SignSignature(keystore, *txPrev, tx, 0, SIGHASH_ALL);
        // Re-use same signature for other inputs
        // (they don't have to be valid for this test)
        for (unsigned int j = 1; j < tx.vin.size(); j++)
            tx.vin[j].scriptSig = tx.vin[0].scriptSig;

        BOOST_CHECK(!AddOrphanTx(MakeTransactionRef(tx), i));
    }

    // Test EraseOrphansFor:
    for (NodeId i = 0; i < 3; i++)
    {
        size_t sizeBefore = mapOrphanTransactions.size();
        EraseOrphansFor(i);
        BOOST_CHECK(mapOrphanTransactions.size() < sizeBefore);
    }

    // Test LimitOrphanTxSize() function:
    LimitOrphanTxSize(40);
    BOOST_CHECK(mapOrphanTransactions.size() <= 40);
    LimitOrphanTxSize(10);
    BOOST_CHECK(mapOrphanTransactions.size() <= 10);
    LimitOrphanTxSize(0);
    BOOST_CHECK(mapOrphanTransactions.empty());
}

BOOST_AUTO_TEST_SUITE_END()
