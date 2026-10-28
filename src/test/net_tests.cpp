// Copyright (c) 2012-2016 The Bitcoin Core developers
// Copyright (c) 2018-2022 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.
#include "addrman.h"
#include "test/test_bitcoin.h"
#include <string>
#include <boost/test/unit_test.hpp>
#include "hash.h"
#include "serialize.h"
#include "streams.h"
#include "net.h"
#include "netbase.h"
#include "chainparams.h"

class CAddrManSerializationMock : public CAddrMan
{
public:
    virtual void Serialize(CDataStream& s) const = 0;

    //! Ensure that bucket placement is always the same for testing purposes.
    void MakeDeterministic()
    {
        nKey.SetNull();
        insecure_rand = FastRandomContext(true);
    }
};

class CAddrManUncorrupted : public CAddrManSerializationMock
{
public:
    void Serialize(CDataStream& s) const
    {
        CAddrMan::Serialize(s);
    }
};

class CAddrManCorrupted : public CAddrManSerializationMock
{
public:
    void Serialize(CDataStream& s) const
    {
        // Produces corrupt output that claims addrman has 20 addrs when it only has one addr.
        unsigned char nVersion = 1;
        s << nVersion;
        s << ((unsigned char)32);
        s << nKey;
        s << 10; // nNew
        s << 10; // nTried

        int nUBuckets = ADDRMAN_NEW_BUCKET_COUNT ^ (1 << 30);
        s << nUBuckets;

        CService serv;
        Lookup("252.1.1.1", serv, 7777, false);
        CAddress addr = CAddress(serv, NODE_NONE);
        CNetAddr resolved;
        LookupHost("252.2.2.2", resolved, false);
        CAddrInfo info = CAddrInfo(addr, resolved);
        s << info;
    }
};

CDataStream AddrmanToStream(CAddrManSerializationMock& _addrman)
{
    CDataStream ssPeersIn(SER_DISK, CLIENT_VERSION);
    ssPeersIn << FLATDATA(Params().MessageStart());
    ssPeersIn << _addrman;
    std::string str = ssPeersIn.str();
    std::vector<unsigned char> vchData(str.begin(), str.end());
    return CDataStream(vchData, SER_DISK, CLIENT_VERSION);
}

BOOST_FIXTURE_TEST_SUITE(net_tests, BasicTestingSetup)

BOOST_AUTO_TEST_CASE(caddrdb_read)
{
    CAddrManUncorrupted addrmanUncorrupted;
    addrmanUncorrupted.MakeDeterministic();

    CService addr1, addr2, addr3;
    Lookup("250.7.1.1", addr1, 8333, false);
    Lookup("250.7.2.2", addr2, 9999, false);
    Lookup("250.7.3.3", addr3, 9999, false);
    BOOST_CHECK(Lookup(std::string("250.7.3.3", 9), addr3, 9999, false));
    BOOST_CHECK(!Lookup(std::string("250.7.3.3\0example.com", 21), addr3, 9999, false));

    // Add three addresses to new table.
    CService source;
    Lookup("252.5.1.1", source, 8333, false);
    addrmanUncorrupted.Add(CAddress(addr1, NODE_NONE), source);
    addrmanUncorrupted.Add(CAddress(addr2, NODE_NONE), source);
    addrmanUncorrupted.Add(CAddress(addr3, NODE_NONE), source);

    // Test that the de-serialization does not throw an exception.
    CDataStream ssPeers1 = AddrmanToStream(addrmanUncorrupted);
    bool exceptionThrown = false;
    CAddrMan addrman1;

    BOOST_CHECK(addrman1.size() == 0);
    try {
        unsigned char pchMsgTmp[4];
        ssPeers1 >> FLATDATA(pchMsgTmp);
        ssPeers1 >> addrman1;
    } catch (const std::exception& e) {
        exceptionThrown = true;
    }

    BOOST_CHECK(addrman1.size() == 3);
    BOOST_CHECK(exceptionThrown == false);

    // Test that CAddrDB::Read creates an addrman with the correct number of addrs.
    CDataStream ssPeers2 = AddrmanToStream(addrmanUncorrupted);

    CAddrMan addrman2;
    CAddrDB adb;
    BOOST_CHECK(addrman2.size() == 0);
    adb.Read(addrman2, ssPeers2);
    BOOST_CHECK(addrman2.size() == 3);
}


BOOST_AUTO_TEST_CASE(caddrdb_read_corrupted)
{
    CAddrManCorrupted addrmanCorrupted;
    addrmanCorrupted.MakeDeterministic();

    // Test that the de-serialization of corrupted addrman throws an exception.
    CDataStream ssPeers1 = AddrmanToStream(addrmanCorrupted);
    bool exceptionThrown = false;
    CAddrMan addrman1;
    BOOST_CHECK(addrman1.size() == 0);
    try {
        unsigned char pchMsgTmp[4];
        ssPeers1 >> FLATDATA(pchMsgTmp);
        ssPeers1 >> addrman1;
    } catch (const std::exception& e) {
        exceptionThrown = true;
    }
    // Even through de-serialization failed addrman is not left in a clean state.
    BOOST_CHECK(addrman1.size() == 1);
    BOOST_CHECK(exceptionThrown);

    // Test that CAddrDB::Read leaves addrman in a clean state if de-serialization fails.
    CDataStream ssPeers2 = AddrmanToStream(addrmanCorrupted);

    CAddrMan addrman2;
    CAddrDB adb;
    BOOST_CHECK(addrman2.size() == 0);
    adb.Read(addrman2, ssPeers2);
    BOOST_CHECK(addrman2.size() == 0);
}

BOOST_AUTO_TEST_CASE(cnode_simple_test)
{
    SOCKET hSocket = INVALID_SOCKET;
    NodeId id = 0;
    int height = 0;

    in_addr ipv4Addr;
    ipv4Addr.s_addr = 0xa0b0c001;

    CAddress addr = CAddress(CService(ipv4Addr, 7777), NODE_NETWORK);
    std::string pszDest = "";
    bool fInboundIn = false;

    // Test that fFeeler is false by default.
    std::unique_ptr<CNode> pnode1(new CNode(id++, NODE_NETWORK, height, hSocket, addr, 0, 0, CAddress(), pszDest, fInboundIn));
    BOOST_CHECK(pnode1->fInbound == false);
    BOOST_CHECK(pnode1->fFeeler == false);

    fInboundIn = true;
    std::unique_ptr<CNode> pnode2(new CNode(id++, NODE_NETWORK, height, hSocket, addr, 1, 1, CAddress(), pszDest, fInboundIn));
    BOOST_CHECK(pnode2->fInbound == true);
    BOOST_CHECK(pnode2->fFeeler == false);
}

// prior to PR #14728, this test triggers an undefined behavior
BOOST_AUTO_TEST_CASE(ipv4_peer_with_ipv6_addrMe_test)
{
    // set up local addresses; all that's necessary to reproduce the bug is
    // that a normal IPv4 address is among the entries, but if this address is
    // !IsRoutable the undefined behavior is easier to trigger deterministically
    {
        LOCK(cs_mapLocalHost);
        in_addr ipv4AddrLocal;
        ipv4AddrLocal.s_addr = 0x0100007f;
        CNetAddr addr = CNetAddr(ipv4AddrLocal);
        LocalServiceInfo lsi;
        lsi.nScore = 23;
        lsi.nPort = 42;
        mapLocalHost[addr] = lsi;
    }

    // create a peer with an IPv4 address
    in_addr ipv4AddrPeer;
    ipv4AddrPeer.s_addr = 0xa0b0c001;
    CAddress addr = CAddress(CService(ipv4AddrPeer, 7777), NODE_NETWORK);
    std::unique_ptr<CNode> pnode(new CNode(0, NODE_NETWORK, 0, INVALID_SOCKET, addr, 0, 0, CAddress(), std::string{}, false));
    pnode->fSuccessfullyConnected.store(true);

    // the peer claims to be reaching us via IPv6
    in6_addr ipv6AddrLocal;
    memset(ipv6AddrLocal.s6_addr, 0, 16);
    ipv6AddrLocal.s6_addr[0] = 0xcc;
    CAddress addrLocal = CAddress(CService(ipv6AddrLocal, 7777), NODE_NETWORK);
    pnode->SetAddrLocal(addrLocal);

    // before patch, this causes undefined behavior detectable with clang's -fsanitize=memory
    AdvertiseLocal(&*pnode);

    // suppress no-checks-run warning; if this test fails, it's by triggering a sanitizer
    BOOST_CHECK(1);
}

static CAddress MakeAddr(const char* ipstr, unsigned short port = 22556)
{
    CService svc;
    Lookup(ipstr, svc, port, false);
    return CAddress(svc, NODE_NETWORK);
}

// Verify that the subnet counting logic identifies peers in the same public /16.
BOOST_AUTO_TEST_CASE(inbound_subnet_limit_ipv4_same_subnet_counted)
{
    SOCKET hSocket = INVALID_SOCKET;
    NodeId id = 0;
    int height = 0;

    // Four inbound peers from 8.8.8.x (same /16 = 8.8.0.0/16, Google DNS range)
    CAddress addr1 = MakeAddr("8.8.8.1");
    CAddress addr2 = MakeAddr("8.8.8.2");
    CAddress addr3 = MakeAddr("8.8.8.3");
    CAddress addr4 = MakeAddr("8.8.8.4");
    CAddress addr5 = MakeAddr("8.8.8.5");

    std::unique_ptr<CNode> n1(new CNode(id++, NODE_NETWORK, height, hSocket, addr1, 0, 0, "", true));
    std::unique_ptr<CNode> n2(new CNode(id++, NODE_NETWORK, height, hSocket, addr2, 0, 0, "", true));
    std::unique_ptr<CNode> n3(new CNode(id++, NODE_NETWORK, height, hSocket, addr3, 0, 0, "", true));
    std::unique_ptr<CNode> n4(new CNode(id++, NODE_NETWORK, height, hSocket, addr4, 0, 0, "", true));

    std::vector<CNode*> nodes = {n1.get(), n2.get(), n3.get(), n4.get()};

    CCriticalSection cs_nodes;
    InboundPeerCounts counts;
    {
        LOCK(cs_nodes);
        // A fifth peer from same /16 should be counted at the limit
        counts = CountInboundPeers(nodes, addr5, cs_nodes);
    }
    BOOST_CHECK_EQUAL(counts.nSubnet, 4U);
    BOOST_CHECK(counts.nSubnet >= DEFAULT_MAX_INBOUND_PER_SUBNET);
}

// Verify that peers from a different /16 are not counted toward the limit.
BOOST_AUTO_TEST_CASE(inbound_subnet_limit_ipv4_different_subnet_not_counted)
{
    SOCKET hSocket = INVALID_SOCKET;
    NodeId id = 0;
    int height = 0;

    CAddress addr1 = MakeAddr("8.8.8.1");
    CAddress addr2 = MakeAddr("8.8.8.2");
    CAddress addr3 = MakeAddr("8.8.8.3");
    CAddress addr4 = MakeAddr("8.8.8.4");

    std::unique_ptr<CNode> n1(new CNode(id++, NODE_NETWORK, height, hSocket, addr1, 0, 0, "", true));
    std::unique_ptr<CNode> n2(new CNode(id++, NODE_NETWORK, height, hSocket, addr2, 0, 0, "", true));
    std::unique_ptr<CNode> n3(new CNode(id++, NODE_NETWORK, height, hSocket, addr3, 0, 0, "", true));
    std::unique_ptr<CNode> n4(new CNode(id++, NODE_NETWORK, height, hSocket, addr4, 0, 0, "", true));

    std::vector<CNode*> nodes = {n1.get(), n2.get(), n3.get(), n4.get()};

    // A peer from a completely different /16 should see count zero
    CAddress addr_other = MakeAddr("1.2.3.4");
    CCriticalSection cs_nodes;
    InboundPeerCounts counts;
    {
        LOCK(cs_nodes);
        counts = CountInboundPeers(nodes, addr_other, cs_nodes);
    }
    BOOST_CHECK_EQUAL(counts.nSubnet, 0U);
    BOOST_CHECK(counts.nSubnet < DEFAULT_MAX_INBOUND_PER_SUBNET);
}

// Verify that whitelisted peers are excluded from the limited-peer count,
// so whitelisted nodes in a subnet do not consume quota that would block
// non-whitelisted peers from the same subnet.
BOOST_AUTO_TEST_CASE(inbound_subnet_limit_whitelisted_not_counted)
{
    SOCKET hSocket = INVALID_SOCKET;
    NodeId id = 0;
    int height = 0;

    CAddress addr1 = MakeAddr("8.8.8.1");
    CAddress addr2 = MakeAddr("8.8.8.2");
    CAddress addr3 = MakeAddr("8.8.8.3");
    CAddress addr4 = MakeAddr("8.8.8.4");
    CAddress addr5 = MakeAddr("8.8.8.5");

    std::unique_ptr<CNode> n1(new CNode(id++, NODE_NETWORK, height, hSocket, addr1, 0, 0, "", true));
    std::unique_ptr<CNode> n2(new CNode(id++, NODE_NETWORK, height, hSocket, addr2, 0, 0, "", true));
    std::unique_ptr<CNode> n3(new CNode(id++, NODE_NETWORK, height, hSocket, addr3, 0, 0, "", true));
    std::unique_ptr<CNode> n4(new CNode(id++, NODE_NETWORK, height, hSocket, addr4, 0, 0, "", true));

    // Mark two of them as whitelisted
    n3->fWhitelisted = true;
    n4->fWhitelisted = true;

    std::vector<CNode*> nodes = {n1.get(), n2.get(), n3.get(), n4.get()};

    CCriticalSection cs_nodes;
    InboundPeerCounts counts;
    {
        LOCK(cs_nodes);
        // Fifth peer from same /16: only 2 non-whitelisted match, under limit
        counts = CountInboundPeers(nodes, addr5, cs_nodes);
    }
    BOOST_CHECK_EQUAL(counts.nSubnet, 2U);
    BOOST_CHECK(counts.nSubnet < DEFAULT_MAX_INBOUND_PER_SUBNET);
}

// Verify that outbound peers in the same /16 are not counted toward the
// inbound subnet limit.
BOOST_AUTO_TEST_CASE(inbound_subnet_limit_outbound_not_counted)
{
    SOCKET hSocket = INVALID_SOCKET;
    NodeId id = 0;
    int height = 0;

    CAddress addr1 = MakeAddr("8.8.8.1");
    CAddress addr2 = MakeAddr("8.8.8.2");
    CAddress addr3 = MakeAddr("8.8.8.3");
    CAddress addr4 = MakeAddr("8.8.8.4");
    CAddress addr5 = MakeAddr("8.8.8.5");

    // All four are outbound (fInbound = false)
    std::unique_ptr<CNode> n1(new CNode(id++, NODE_NETWORK, height, hSocket, addr1, 0, 0, "", false));
    std::unique_ptr<CNode> n2(new CNode(id++, NODE_NETWORK, height, hSocket, addr2, 0, 0, "", false));
    std::unique_ptr<CNode> n3(new CNode(id++, NODE_NETWORK, height, hSocket, addr3, 0, 0, "", false));
    std::unique_ptr<CNode> n4(new CNode(id++, NODE_NETWORK, height, hSocket, addr4, 0, 0, "", false));

    std::vector<CNode*> nodes = {n1.get(), n2.get(), n3.get(), n4.get()};

    CCriticalSection cs_nodes;
    InboundPeerCounts counts;
    {
        LOCK(cs_nodes);
        counts = CountInboundPeers(nodes, addr5, cs_nodes);
    }
    BOOST_CHECK_EQUAL(counts.nSubnet, 0U);
    BOOST_CHECK(counts.nSubnet < DEFAULT_MAX_INBOUND_PER_SUBNET);
}

// Verify that non-routable addresses (e.g. RFC1918) are ignored by the
// per-subnet counter, so private-range nodes don't affect public subnet limits.
BOOST_AUTO_TEST_CASE(inbound_subnet_limit_private_addr_not_counted)
{
    SOCKET hSocket = INVALID_SOCKET;
    NodeId id = 0;
    int height = 0;

    // Use 192.168.1.x — RFC1918 (non-routable)
    CAddress addr1 = MakeAddr("192.168.1.1");
    CAddress addr2 = MakeAddr("192.168.1.2");
    CAddress addr3 = MakeAddr("192.168.1.3");
    CAddress addr4 = MakeAddr("192.168.1.4");
    CAddress addr5 = MakeAddr("192.168.1.5");

    std::unique_ptr<CNode> n1(new CNode(id++, NODE_NETWORK, height, hSocket, addr1, 0, 0, "", true));
    std::unique_ptr<CNode> n2(new CNode(id++, NODE_NETWORK, height, hSocket, addr2, 0, 0, "", true));
    std::unique_ptr<CNode> n3(new CNode(id++, NODE_NETWORK, height, hSocket, addr3, 0, 0, "", true));
    std::unique_ptr<CNode> n4(new CNode(id++, NODE_NETWORK, height, hSocket, addr4, 0, 0, "", true));

    std::vector<CNode*> nodes = {n1.get(), n2.get(), n3.get(), n4.get()};

    // Incoming from the same private range — count should be 0 since non-routable
    // nodes are skipped by the counter (and AcceptConnection skips the check entirely
    // for non-routable incoming addresses)
    CCriticalSection cs_nodes;
    InboundPeerCounts counts;
    {
        LOCK(cs_nodes);
        counts = CountInboundPeers(nodes, addr5, cs_nodes);
    }
    BOOST_CHECK_EQUAL(counts.nSubnet, 0U);
}

// Verify that IPv6 peers in the same public /32 are counted correctly.
BOOST_AUTO_TEST_CASE(inbound_subnet_limit_ipv6_same_subnet_counted)
{
    SOCKET hSocket = INVALID_SOCKET;
    NodeId id = 0;
    int height = 0;

    // 2600:1400::/32 is a real public range (Akamai).
    // Build four addresses in 2600:1400::/32, one more to test against.
    CAddress addr1 = MakeAddr("2600:1400:0:1::1");
    CAddress addr2 = MakeAddr("2600:1400:0:2::1");
    CAddress addr3 = MakeAddr("2600:1400:0:3::1");
    CAddress addr4 = MakeAddr("2600:1400:0:4::1");
    CAddress addr5 = MakeAddr("2600:1400:0:5::1");

    std::unique_ptr<CNode> n1(new CNode(id++, NODE_NETWORK, height, hSocket, addr1, 0, 0, "", true));
    std::unique_ptr<CNode> n2(new CNode(id++, NODE_NETWORK, height, hSocket, addr2, 0, 0, "", true));
    std::unique_ptr<CNode> n3(new CNode(id++, NODE_NETWORK, height, hSocket, addr3, 0, 0, "", true));
    std::unique_ptr<CNode> n4(new CNode(id++, NODE_NETWORK, height, hSocket, addr4, 0, 0, "", true));

    std::vector<CNode*> nodes = {n1.get(), n2.get(), n3.get(), n4.get()};

    CCriticalSection cs_nodes;
    InboundPeerCounts counts;
    {
        LOCK(cs_nodes);
        counts = CountInboundPeers(nodes, addr5, cs_nodes);
    }

    // All four addresses are in 2600:1400::/32, so nSubnet should equal 4
    // and be at the limit.
    BOOST_CHECK_EQUAL(counts.nSubnet, 4U);
    BOOST_CHECK(counts.nSubnet >= DEFAULT_MAX_INBOUND_PER_SUBNET);
}

BOOST_AUTO_TEST_SUITE_END()
