// Copyright (c) 2022 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "netmessagemaker.h"
#include "netbase.h"
#include "protocol.h"
#include "version.h"

#include "test/test_bitcoin.h"

#include <boost/test/unit_test.hpp>

// This tests message bodies for the networking part of the p2p protocol
BOOST_FIXTURE_TEST_SUITE(protocol_net_tests, TestingSetup)

static CNetAddr ResolveIP(const std::string& ip)
{
    CNetAddr addr;
    LookupHost(ip, addr, false);
    return addr;
}

// impl independent sequential reader for vector<unsigned char>
static std::vector<unsigned char> ReadVch(std::vector<unsigned char> vch, std::vector<unsigned char>::const_iterator* it, size_t num)
{
  std::vector<unsigned char> out(*it, *it + num);
  std::advance(*it, num);
  return out;
}

BOOST_AUTO_TEST_CASE(addr_msg_body)
{
  std::vector<CAddress> vAddr;
  vAddr.reserve(4);
  vAddr.push_back(CAddress(CService(ResolveIP("1.2.3.4"), 1337), NODE_NONE));
  vAddr.push_back(CAddress(CService(ResolveIP("211.222.233.244"), 1337), NODE_NETWORK));
  vAddr.push_back(CAddress(CService(ResolveIP("::1"), 1337), NODE_BLOOM));
  vAddr.push_back(CAddress(CService(ResolveIP("2001:2001:9999:9999:9999:9999:9999:9999"), 1337), NODE_GETUTXO));

  auto addrmsg = CNetMsgMaker(PROTOCOL_VERSION).Make(NetMsgType::ADDR, vAddr);
  std::vector<unsigned char>::const_iterator it(addrmsg.data.cbegin());

  // number of entries in message, as varint
  BOOST_CHECK_EQUAL(HexStr(ReadVch(addrmsg.data, &it, 1)), "04");

  // first entry
  BOOST_CHECK_EQUAL(HexStr(ReadVch(addrmsg.data, &it, 4)), "00e1f505"); // timestamp
  BOOST_CHECK_EQUAL(HexStr(ReadVch(addrmsg.data, &it, 8)), "0000000000000000"); // NODE_NONE
  BOOST_CHECK_EQUAL(HexStr(ReadVch(addrmsg.data, &it, 16)), "00000000000000000000ffff01020304"); // ipv4 in ipv6
  BOOST_CHECK_EQUAL(HexStr(ReadVch(addrmsg.data, &it, 2)), "0539"); // port

  // second entry
  BOOST_CHECK_EQUAL(HexStr(ReadVch(addrmsg.data, &it, 4)), "00e1f505"); // timestamp
  BOOST_CHECK_EQUAL(HexStr(ReadVch(addrmsg.data, &it, 8)), "0100000000000000"); // NODE_NETWORK
  BOOST_CHECK_EQUAL(HexStr(ReadVch(addrmsg.data, &it, 16)), "00000000000000000000ffffd3dee9f4"); // ipv4 in ipv6
  BOOST_CHECK_EQUAL(HexStr(ReadVch(addrmsg.data, &it, 2)), "0539"); // port

  // third entry
  BOOST_CHECK_EQUAL(HexStr(ReadVch(addrmsg.data, &it, 4)), "00e1f505"); // timestamp
  BOOST_CHECK_EQUAL(HexStr(ReadVch(addrmsg.data, &it, 8)), "0400000000000000"); // NODE_BLOOM
  BOOST_CHECK_EQUAL(HexStr(ReadVch(addrmsg.data, &it, 16)), "00000000000000000000000000000001"); // ipv6
  BOOST_CHECK_EQUAL(HexStr(ReadVch(addrmsg.data, &it, 2)), "0539"); // port

  // fourth entry
  BOOST_CHECK_EQUAL(HexStr(ReadVch(addrmsg.data, &it, 4)), "00e1f505"); // timestamp
  BOOST_CHECK_EQUAL(HexStr(ReadVch(addrmsg.data, &it, 8)), "0200000000000000"); // NODE_GETUTXO
  BOOST_CHECK_EQUAL(HexStr(ReadVch(addrmsg.data, &it, 16)), "20012001999999999999999999999999"); // ipv6
  BOOST_CHECK_EQUAL(HexStr(ReadVch(addrmsg.data, &it, 2)), "0539"); // port

  // test entry size encoding for 1000 entries
  vAddr.clear();
  for (size_t i = 0; i<1000; i++) {
    vAddr.push_back(CAddress(CService(), NODE_NONE));
  }
  addrmsg = CNetMsgMaker(PROTOCOL_VERSION).Make(NetMsgType::ADDR, vAddr);
  it = std::vector<unsigned char>::const_iterator(addrmsg.data.cbegin());

  // number of entries in message, as varint
  BOOST_CHECK_EQUAL(HexStr(ReadVch(addrmsg.data, &it, 3)), "fde803");

}

BOOST_AUTO_TEST_CASE(version_msg_body)
{
  CAddress addrUs = CAddress(CService(), NODE_NETWORK);
  CAddress addrThem = CAddress(CService(ResolveIP("2001:2001:9999:9999:9999:9999:9999:9999"), 22556), NODE_NETWORK);

  auto versionMsg = CNetMsgMaker(INIT_PROTO_VERSION).Make(
    NetMsgType::VERSION,
    PROTOCOL_VERSION,
    (uint64_t) NODE_NETWORK,
    int64_t(100000000),
    addrThem,
    addrUs,
    uint64_t(1337),
    std::string("/Sub::Version/"),
    7331,
    true
  );
  std::vector<unsigned char>::const_iterator it(versionMsg.data.cbegin());

  BOOST_CHECK_EQUAL(HexStr(ReadVch(versionMsg.data, &it, 4)), "7f110100"); // protocol version
  BOOST_CHECK_EQUAL(HexStr(ReadVch(versionMsg.data, &it, 8)), "0100000000000000"); // our services
  BOOST_CHECK_EQUAL(HexStr(ReadVch(versionMsg.data, &it, 8)), "00e1f50500000000"); // timestamp
  BOOST_CHECK_EQUAL(HexStr(ReadVch(versionMsg.data, &it, 8)), "0100000000000000"); // their services
  BOOST_CHECK_EQUAL(HexStr(ReadVch(versionMsg.data, &it, 16)), "20012001999999999999999999999999"); // their ip
  BOOST_CHECK_EQUAL(HexStr(ReadVch(versionMsg.data, &it, 2)), "581c"); // their port
  BOOST_CHECK_EQUAL(HexStr(ReadVch(versionMsg.data, &it, 8)), "0100000000000000"); // our services
  BOOST_CHECK_EQUAL(HexStr(ReadVch(versionMsg.data, &it, 16)), "00000000000000000000000000000000"); // our ip (blank)
  BOOST_CHECK_EQUAL(HexStr(ReadVch(versionMsg.data, &it, 2)), "0000"); // our port (blank)
  BOOST_CHECK_EQUAL(HexStr(ReadVch(versionMsg.data, &it, 8)), "3905000000000000"); // nonce
  BOOST_CHECK_EQUAL(HexStr(ReadVch(versionMsg.data, &it, 1)), "0e"); // subversion string length
  BOOST_CHECK_EQUAL(HexStr(ReadVch(versionMsg.data, &it, 0x0e)), "2f5375623a3a56657273696f6e2f"); // subversion
  BOOST_CHECK_EQUAL(HexStr(ReadVch(versionMsg.data, &it, 4)), "a31c0000"); //height
  BOOST_CHECK_EQUAL(HexStr(ReadVch(versionMsg.data, &it, 1)), "01"); // relay tx bool
}

BOOST_AUTO_TEST_CASE(ping)
{
    //BIP31 ping
    auto pingBIP31 = CNetMsgMaker(PROTOCOL_VERSION).Make(NetMsgType::PING, uint64_t(1337));
    BOOST_CHECK_EQUAL(HexStr(pingBIP31.data), "3905000000000000");

    //pre-BIP31 ping
    auto pingPreBIP31 = CNetMsgMaker(PROTOCOL_VERSION).Make(NetMsgType::PING);
    BOOST_CHECK_EQUAL(HexStr(pingPreBIP31.data), "");
}

BOOST_AUTO_TEST_CASE(pong)
{
  auto pong = CNetMsgMaker(PROTOCOL_VERSION).Make(NetMsgType::PONG, uint64_t(1337));
  BOOST_CHECK_EQUAL(HexStr(pong.data), "3905000000000000");
}

BOOST_AUTO_TEST_SUITE_END()
