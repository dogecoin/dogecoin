// Copyright (c) 2012-2013 The Bitcoin Core developers
// Distributed under the MIT/X11 software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "bloom.h"

#include "base58.h"
#include "key.h"
#include "main.h"
#include "auxpow.h"
#include "serialize.h"
#include "uint256.h"
#include "util.h"

#include <vector>

#include <boost/test/unit_test.hpp>

using namespace std;
using namespace boost::tuples;

BOOST_AUTO_TEST_SUITE(bloom_tests)

BOOST_AUTO_TEST_CASE(bloom_create_insert_serialize)
{
    CBloomFilter filter(3, 0.01, 0, BLOOM_UPDATE_ALL);

    filter.insert(ParseHex("99108ad8ed9bb6274d3980bab5a85c048f0950c8"));
    BOOST_CHECK_MESSAGE( filter.contains(ParseHex("99108ad8ed9bb6274d3980bab5a85c048f0950c8")), "BloomFilter doesn't contain just-inserted object!");
    // One bit different in first byte
    BOOST_CHECK_MESSAGE(!filter.contains(ParseHex("19108ad8ed9bb6274d3980bab5a85c048f0950c8")), "BloomFilter contains something it shouldn't!");

    filter.insert(ParseHex("b5a2c786d9ef4658287ced5914b37a1b4aa32eee"));
    BOOST_CHECK_MESSAGE(filter.contains(ParseHex("b5a2c786d9ef4658287ced5914b37a1b4aa32eee")), "BloomFilter doesn't contain just-inserted object (2)!");

    filter.insert(ParseHex("b9300670b4c5366e95b2699e8b18bc75e5f729c5"));
    BOOST_CHECK_MESSAGE(filter.contains(ParseHex("b9300670b4c5366e95b2699e8b18bc75e5f729c5")), "BloomFilter doesn't contain just-inserted object (3)!");

    CDataStream stream(SER_NETWORK, PROTOCOL_VERSION);
    filter.Serialize(stream, SER_NETWORK, PROTOCOL_VERSION);

    vector<unsigned char> vch = ParseHex("03614e9b050000000000000001");
    vector<char> expected(vch.size());

    for (unsigned int i = 0; i < vch.size(); i++)
        expected[i] = (char)vch[i];

    BOOST_CHECK_EQUAL_COLLECTIONS(stream.begin(), stream.end(), expected.begin(), expected.end());
}

BOOST_AUTO_TEST_CASE(bloom_create_insert_serialize_with_tweak)
{
    // Same test as bloom_create_insert_serialize, but we add a nTweak of 100
    CBloomFilter filter(3, 0.01, 2147483649, BLOOM_UPDATE_ALL);

    filter.insert(ParseHex("99108ad8ed9bb6274d3980bab5a85c048f0950c8"));
    BOOST_CHECK_MESSAGE( filter.contains(ParseHex("99108ad8ed9bb6274d3980bab5a85c048f0950c8")), "BloomFilter doesn't contain just-inserted object!");
    // One bit different in first byte
    BOOST_CHECK_MESSAGE(!filter.contains(ParseHex("19108ad8ed9bb6274d3980bab5a85c048f0950c8")), "BloomFilter contains something it shouldn't!");

    filter.insert(ParseHex("b5a2c786d9ef4658287ced5914b37a1b4aa32eee"));
    BOOST_CHECK_MESSAGE(filter.contains(ParseHex("b5a2c786d9ef4658287ced5914b37a1b4aa32eee")), "BloomFilter doesn't contain just-inserted object (2)!");

    filter.insert(ParseHex("b9300670b4c5366e95b2699e8b18bc75e5f729c5"));
    BOOST_CHECK_MESSAGE(filter.contains(ParseHex("b9300670b4c5366e95b2699e8b18bc75e5f729c5")), "BloomFilter doesn't contain just-inserted object (3)!");

    CDataStream stream(SER_NETWORK, PROTOCOL_VERSION);
    filter.Serialize(stream, SER_NETWORK, PROTOCOL_VERSION);

    vector<unsigned char> vch = ParseHex("03ce4299050000000100008001");
    vector<char> expected(vch.size());

    for (unsigned int i = 0; i < vch.size(); i++)
        expected[i] = (char)vch[i];

    BOOST_CHECK_EQUAL_COLLECTIONS(stream.begin(), stream.end(), expected.begin(), expected.end());
}

BOOST_AUTO_TEST_CASE(bloom_create_insert_key)
{
    string strSecret = string("6KmNiPzEBEdSHVPEWqv6YFVtRC1LSnKXj4zJrd7Yn3FibwY2fHc");
    CBitcoinSecret vchSecret;
    BOOST_CHECK(vchSecret.SetString(strSecret));

    CKey key = vchSecret.GetKey();
    CPubKey pubkey = key.GetPubKey();
    vector<unsigned char> vchPubKey(pubkey.begin(), pubkey.end());

    CBloomFilter filter(2, 0.001, 0, BLOOM_UPDATE_ALL);
    filter.insert(vchPubKey);
    uint160 hash = pubkey.GetID();
    filter.insert(vector<unsigned char>(hash.begin(), hash.end()));

    CDataStream stream(SER_NETWORK, PROTOCOL_VERSION);
    filter.Serialize(stream, SER_NETWORK, PROTOCOL_VERSION);

    vector<unsigned char> vch = ParseHex("03193e76080000000000000001");
    vector<char> expected(vch.size());

    for (unsigned int i = 0; i < vch.size(); i++)
        expected[i] = (char)vch[i];

    BOOST_CHECK_EQUAL_COLLECTIONS(stream.begin(), stream.end(), expected.begin(), expected.end());
}

BOOST_AUTO_TEST_CASE(bloom_match)
{
    // Random real transaction (516a1229cd8dbf59b33597772484e4db14b91bd5620cad567b8bfe0a5ea99ff9)
    CTransaction tx;
    CDataStream stream(ParseHex("01000000014507ef214d78ee79cb8396d76c0be2453f1518e219578c7ba0c792b3729cd252000000006b483045022100e676203127a390a526438bcbb3506f94a67d20f1a871a431c862004187c72bd802203bcf3a5daab349c98fe1199c96eb838501e0dd4387e431fc42a1586082208806012103861fe3437b6102a6bdb5664bd15d1e4c695978675c0897a8510f49932eac8d83ffffffff0200cde7a08b0000001976a91414e568f3b94de18a4b0131e0dba1c0ee0d76b96b88ac00e1f505000000001976a914e37c8e7fd5c53d6288794bedc19b55758ddd941288ac00000000"), SER_DISK, CLIENT_VERSION);
    stream >> tx;

    // and one which spends it (df18521c0c210c7a2965444ea87381e35b049d258451a9eb90e3ba862fedb3a9)
    unsigned char ch[] = {0x01, 0x00, 0x00, 0x00, 0x05, 0xdd, 0x4a, 0xeb, 0x0c, 0xe1, 0xc8, 0xfe, 0xf4, 0xc3, 0x5c, 0x77, 0x6d, 0xea, 0x07, 0xfc, 0xfe, 0xf5, 0x0c, 0x79, 0x76, 0x91, 0xe7, 0xfc, 0xa4, 0x96, 0x73, 0xf5, 0xa6, 0xee, 0x3e, 0xa9, 0xf6, 0x00, 0x00, 0x00, 0x00, 0x6b, 0x48, 0x30, 0x45, 0x02, 0x20, 0x34, 0x35, 0x93, 0x9e, 0xaf, 0x27, 0xe0, 0xe1, 0x70, 0xec, 0x01, 0x46, 0x50, 0x3c, 0xc8, 0x4b, 0x61, 0x34, 0x60, 0x7b, 0x20, 0xc4, 0x54, 0xa1, 0x3f, 0x4e, 0x40, 0xe6, 0x06, 0x79, 0x23, 0xb6, 0x02, 0x21, 0x00, 0x8b, 0x7d, 0xb6, 0xbd, 0x49, 0xfb, 0x35, 0x07, 0x71, 0x6a, 0xb5, 0x90, 0x88, 0x96, 0x11, 0x66, 0x59, 0xba, 0x8c, 0x15, 0x26, 0x8f, 0x64, 0xbd, 0x08, 0x4b, 0xf4, 0xaa, 0xed, 0x43, 0x68, 0x9b, 0x01, 0x21, 0x02, 0x59, 0xa2, 0xf6, 0xb7, 0x12, 0x84, 0x5c, 0x01, 0x72, 0x19, 0xae, 0x1a, 0xf9, 0xdd, 0x7d, 0x66, 0x1f, 0xb0, 0x5d, 0xbf, 0x13, 0x40, 0x5c, 0x12, 0xce, 0xfb, 0x09, 0x3b, 0xad, 0xd0, 0x7a, 0x9e, 0xff, 0xff, 0xff, 0xff, 0xe9, 0xc1, 0x07, 0xe7, 0xc3, 0x2f, 0x11, 0x5b, 0xff, 0x16, 0x1c, 0xa8, 0x75, 0x35, 0x61, 0x8b, 0x7b, 0x03, 0xfe, 0xcb, 0x8b, 0x3e, 0xa9, 0xf3, 0xd4, 0xfd, 0x75, 0x6a, 0x7e, 0x00, 0xa9, 0x92, 0x01, 0x00, 0x00, 0x00, 0x6c, 0x49, 0x30, 0x46, 0x02, 0x21, 0x00, 0xc3, 0x2a, 0x77, 0xdb, 0xce, 0x04, 0x5c, 0x94, 0xf8, 0x8a, 0xac, 0x97, 0x6e, 0x29, 0x68, 0xba, 0x6e, 0xff, 0x34, 0xe3, 0xd2, 0xbb, 0x78, 0xad, 0xe7, 0x60, 0x87, 0x2b, 0x60, 0xad, 0x04, 0xdc, 0x02, 0x21, 0x00, 0xde, 0xa2, 0xee, 0x23, 0x18, 0xdd, 0x9b, 0x4e, 0x47, 0xd0, 0x85, 0x92, 0x5c, 0xa3, 0x5b, 0xce, 0x23, 0x8b, 0x26, 0x49, 0x17, 0x38, 0xe4, 0xa7, 0x96, 0x55, 0xa0, 0x39, 0x20, 0xb0, 0x53, 0x93, 0x01, 0x21, 0x03, 0xd1, 0xbf, 0xa3, 0xeb, 0xf6, 0x9a, 0x54, 0x8a, 0x6b, 0x91, 0x6f, 0xe1, 0x7b, 0x02, 0x50, 0x8a, 0x90, 0x6e, 0x77, 0xb4, 0x37, 0xe8, 0x93, 0xf2, 0xcc, 0xb8, 0xf8, 0x04, 0x15, 0x8b, 0xdf, 0xac, 0xff, 0xff, 0xff, 0xff, 0xf9, 0x9f, 0xa9, 0x5e, 0x0a, 0xfe, 0x8b, 0x7b, 0x56, 0xad, 0x0c, 0x62, 0xd5, 0x1b, 0xb9, 0x14, 0xdb, 0xe4, 0x84, 0x24, 0x77, 0x97, 0x35, 0xb3, 0x59, 0xbf, 0x8d, 0xcd, 0x29, 0x12, 0x6a, 0x51, 0x00, 0x00, 0x00, 0x00, 0x6a, 0x47, 0x30, 0x44, 0x02, 0x20, 0x1e, 0x22, 0xd0, 0x20, 0x5d, 0xd8, 0x6e, 0x98, 0x23, 0x05, 0xe6, 0xc7, 0x3c, 0x25, 0x35, 0x0b, 0x4d, 0x50, 0x6f, 0x7f, 0x66, 0x83, 0xd8, 0xed, 0x8f, 0xd6, 0x4a, 0xc0, 0x68, 0xfa, 0x2c, 0xb6, 0x02, 0x20, 0x5d, 0x80, 0xc9, 0xb4, 0xbf, 0x0b, 0x59, 0x8f, 0xef, 0x44, 0xca, 0xa6, 0x30, 0x8f, 0x18, 0x57, 0xeb, 0x67, 0xb6, 0x63, 0xf0, 0x28, 0x08, 0x38, 0x47, 0xb2, 0xf4, 0xa5, 0x2b, 0x1c, 0x1b, 0x63, 0x01, 0x21, 0x02, 0x59, 0xa2, 0xf6, 0xb7, 0x12, 0x84, 0x5c, 0x01, 0x72, 0x19, 0xae, 0x1a, 0xf9, 0xdd, 0x7d, 0x66, 0x1f, 0xb0, 0x5d, 0xbf, 0x13, 0x40, 0x5c, 0x12, 0xce, 0xfb, 0x09, 0x3b, 0xad, 0xd0, 0x7a, 0x9e, 0xff, 0xff, 0xff, 0xff, 0x39, 0xea, 0x46, 0xc4, 0x04, 0x13, 0x7d, 0x49, 0xe7, 0x15, 0x5f, 0x00, 0x76, 0x94, 0xcd, 0x00, 0xca, 0xa8, 0xa2, 0xf5, 0xec, 0x63, 0x15, 0x47, 0x3d, 0x80, 0x8a, 0xb6, 0xdc, 0x2a, 0x6c, 0x94, 0x01, 0x00, 0x00, 0x00, 0x6a, 0x47, 0x30, 0x44, 0x02, 0x20, 0x53, 0xea, 0x93, 0x3d, 0x81, 0x4e, 0x3e, 0xa5, 0x15, 0xd3, 0xaf, 0xdf, 0xb8, 0x63, 0xd4, 0x25, 0xfe, 0xa7, 0x71, 0x02, 0xf1, 0xf2, 0xd7, 0x60, 0x89, 0xb0, 0xb0, 0x5a, 0x16, 0xd5, 0xb1, 0x47, 0x02, 0x20, 0x6e, 0xac, 0xdf, 0xba, 0x18, 0x66, 0xc8, 0xdc, 0xab, 0x06, 0x88, 0x56, 0x67, 0x3c, 0x88, 0xb7, 0x5d, 0xf1, 0xb8, 0xe7, 0xcc, 0xc9, 0xee, 0x5a, 0x0d, 0xad, 0x14, 0xb1, 0xc1, 0xe0, 0x35, 0x16, 0x01, 0x21, 0x03, 0xd1, 0xbf, 0xa3, 0xeb, 0xf6, 0x9a, 0x54, 0x8a, 0x6b, 0x91, 0x6f, 0xe1, 0x7b, 0x02, 0x50, 0x8a, 0x90, 0x6e, 0x77, 0xb4, 0x37, 0xe8, 0x93, 0xf2, 0xcc, 0xb8, 0xf8, 0x04, 0x15, 0x8b, 0xdf, 0xac, 0xff, 0xff, 0xff, 0xff, 0x7c, 0x7f, 0x40, 0xba, 0xc5, 0x0d, 0x09, 0x53, 0x3b, 0xc0, 0x1b, 0xbc, 0xa8, 0x4b, 0xd4, 0x70, 0x93, 0x40, 0x9a, 0x85, 0x4b, 0xae, 0x30, 0x01, 0xfc, 0xa2, 0x29, 0xb8, 0xb7, 0xfc, 0x61, 0xde, 0x00, 0x00, 0x00, 0x00, 0x6b, 0x48, 0x30, 0x45, 0x02, 0x21, 0x00, 0xbf, 0x33, 0x69, 0x86, 0x83, 0xab, 0xcd, 0x93, 0xef, 0x54, 0x49, 0xf4, 0xc6, 0x9d, 0x44, 0x80, 0x05, 0x9e, 0xee, 0xc2, 0xca, 0x30, 0x4d, 0x90, 0x03, 0x38, 0xe0, 0xf1, 0x7b, 0x43, 0xc0, 0xf1, 0x02, 0x20, 0x1f, 0xf2, 0xb1, 0x82, 0x04, 0x17, 0x97, 0x2f, 0x38, 0xc1, 0x71, 0x06, 0x6b, 0x9c, 0x44, 0xb5, 0x9a, 0x26, 0x36, 0xe5, 0x9d, 0x93, 0xec, 0x57, 0xc7, 0x9a, 0x09, 0xbe, 0x11, 0xc1, 0xbc, 0x3a, 0x01, 0x21, 0x02, 0xa8, 0x8a, 0x37, 0x5c, 0xfa, 0xa9, 0x30, 0x85, 0x50, 0x00, 0x4f, 0xb5, 0xcb, 0x7d, 0x2f, 0xe1, 0x85, 0xc1, 0xd2, 0x7f, 0xbb, 0x6e, 0x86, 0xe1, 0x9f, 0xf5, 0x4f, 0x09, 0x13, 0x4e, 0x28, 0x21, 0xff, 0xff, 0xff, 0xff, 0x01, 0x00, 0x4c, 0xea, 0x26, 0x23, 0x02, 0x00, 0x00, 0x19, 0x76, 0xa9, 0x14, 0x24, 0x1c, 0x9c, 0xce, 0x1f, 0x08, 0x6a, 0xc4, 0x90, 0xc7, 0xff, 0x9c, 0xfb, 0xf5, 0x5d, 0x4b, 0x81, 0x5f, 0xc1, 0x3c, 0x88, 0xac, 0x00, 0x00, 0x00, 0x00, 0x00};
    vector<unsigned char> vch(ch, ch + sizeof(ch) -1);
    CDataStream spendStream(vch, SER_DISK, CLIENT_VERSION);
    CTransaction spendingTx;
    spendStream >> spendingTx;

    CBloomFilter filter(10, 0.000001, 0, BLOOM_UPDATE_ALL);
    filter.insert(uint256("0x516a1229cd8dbf59b33597772484e4db14b91bd5620cad567b8bfe0a5ea99ff9"));
    BOOST_CHECK_MESSAGE(filter.IsRelevantAndUpdate(tx, tx.GetHash()), "Simple Bloom filter didn't match tx hash");

    filter = CBloomFilter(10, 0.000001, 0, BLOOM_UPDATE_ALL);
    // byte-reversed tx hash
    filter.insert(ParseHex("f99fa95e0afe8b7b56ad0c62d51bb914dbe48424779735b359bf8dcd29126a51"));
    BOOST_CHECK_MESSAGE(filter.IsRelevantAndUpdate(tx, tx.GetHash()), "Simple Bloom filter didn't match manually serialized tx hash");

    filter = CBloomFilter(10, 0.000001, 0, BLOOM_UPDATE_ALL);
    filter.insert(ParseHex("3045022100e676203127a390a526438bcbb3506f94a67d20f1a871a431c862004187c72bd802203bcf3a5daab349c98fe1199c96eb838501e0dd4387e431fc42a158608220880601"));
    BOOST_CHECK_MESSAGE(filter.IsRelevantAndUpdate(tx, tx.GetHash()), "Simple Bloom filter didn't match input signature");

    filter = CBloomFilter(10, 0.000001, 0, BLOOM_UPDATE_ALL);
    filter.insert(ParseHex("03861fe3437b6102a6bdb5664bd15d1e4c695978675c0897a8510f49932eac8d83"));
    BOOST_CHECK_MESSAGE(filter.IsRelevantAndUpdate(tx, tx.GetHash()), "Simple Bloom filter didn't match input pub key");

    filter = CBloomFilter(10, 0.000001, 0, BLOOM_UPDATE_ALL);
    filter.insert(ParseHex("14e568f3b94de18a4b0131e0dba1c0ee0d76b96b"));
    BOOST_CHECK_MESSAGE(filter.IsRelevantAndUpdate(tx, tx.GetHash()), "Simple Bloom filter didn't match output address");
    BOOST_CHECK_MESSAGE(filter.IsRelevantAndUpdate(spendingTx, spendingTx.GetHash()), "Simple Bloom filter didn't add output");

    filter = CBloomFilter(10, 0.000001, 0, BLOOM_UPDATE_ALL);
    filter.insert(ParseHex("e37c8e7fd5c53d6288794bedc19b55758ddd9412"));
    BOOST_CHECK_MESSAGE(filter.IsRelevantAndUpdate(tx, tx.GetHash()), "Simple Bloom filter didn't match output address");

    filter = CBloomFilter(10, 0.000001, 0, BLOOM_UPDATE_ALL);
    filter.insert(COutPoint(uint256("0x52d29c72b392c7a07b8c5719e218153f45e20b6cd79683cb79ee784d21ef0745"), 0));
    BOOST_CHECK_MESSAGE(filter.IsRelevantAndUpdate(tx, tx.GetHash()), "Simple Bloom filter didn't match COutPoint");

    filter = CBloomFilter(10, 0.000001, 0, BLOOM_UPDATE_ALL);
    COutPoint prevOutPoint(uint256("0x52d29c72b392c7a07b8c5719e218153f45e20b6cd79683cb79ee784d21ef0745"), 0);
    {
        vector<unsigned char> data(32 + sizeof(unsigned int));
        memcpy(&data[0], prevOutPoint.hash.begin(), 32);
        memcpy(&data[32], &prevOutPoint.n, sizeof(unsigned int));
        filter.insert(data);
    }
    BOOST_CHECK_MESSAGE(filter.IsRelevantAndUpdate(tx, tx.GetHash()), "Simple Bloom filter didn't match manually serialized COutPoint");

    filter = CBloomFilter(10, 0.000001, 0, BLOOM_UPDATE_ALL);
    filter.insert(uint256("00000009e784f32f62ef849763d4f45b98e07ba658647343b915ff832b110436"));
    BOOST_CHECK_MESSAGE(!filter.IsRelevantAndUpdate(tx, tx.GetHash()), "Simple Bloom filter matched random tx hash");

    filter = CBloomFilter(10, 0.000001, 0, BLOOM_UPDATE_ALL);
    filter.insert(ParseHex("0000006d2965547608b9e15d9032a7b9d64fa431"));
    BOOST_CHECK_MESSAGE(!filter.IsRelevantAndUpdate(tx, tx.GetHash()), "Simple Bloom filter matched random address");

    filter = CBloomFilter(10, 0.000001, 0, BLOOM_UPDATE_ALL);
    filter.insert(COutPoint(uint256("0x52d29c72b392c7a07b8c5719e218153f45e20b6cd79683cb79ee784d21ef0745"), 1));
    BOOST_CHECK_MESSAGE(!filter.IsRelevantAndUpdate(tx, tx.GetHash()), "Simple Bloom filter matched COutPoint for an output we didn't care about");

    filter = CBloomFilter(10, 0.000001, 0, BLOOM_UPDATE_ALL);
    filter.insert(COutPoint(uint256("0x000000d70786e899529d71dbeba91ba216982fb6ba58f3bdaab65e73b7e9260b"), 0));
    BOOST_CHECK_MESSAGE(!filter.IsRelevantAndUpdate(tx, tx.GetHash()), "Simple Bloom filter matched COutPoint for an output we didn't care about");
}

BOOST_AUTO_TEST_CASE(merkle_block_1)
{
    // Random real block (61b66a20124080028c846d6f953e0505096257b8902a412ef2f7dec47415aa28)
    // With 9 txes
    CBlock block;
    CDataStream stream(ParseHex("02000000be93bd60000f8f016d4f6d550526b46621703e309e2b3673b599b8951799c9e4f2ea1a46e5b55b2c389aa843d65beb5f9a6dc5681ffdfe5dad40edbf9c283d29611c515322ee371b008018fb0901000000010000000000000000000000000000000000000000000000000000000000000000ffffffff27039ed802062f503253482f04611c51530810000038040000000d2f6e6f64655374726174756d2f000000000100f5ebe1bc1600001976a9146f7854e81a35de4aec0a36deca6497b35320a62b88ac000000000100000005f687e1b20ff1cead9b75fe660a31a4ce439a4246f6fbce97039fc8851b9bc912060000006c493046022100932f6e8c9fc1947ee07f78a8814abbc345762da8d9fe619556bfcc04c4e672a9022100ea74cc09750ca1b89c5b816ebda90a16370a675df2f673e03d1bb14eecc1db080121035351df6c0f42202cb8bc073cfe9ad5202bf5196d39505adaffdd6723552fb036ffffffff368c53bcdc62c2cc4b476a4759df53ac8c4e39d89798073889352bdc460c4507020000006b483045022100be221b8a23dda3fafa4d97f90caacc8e3f6f41d8d7c4df6f2ea570d6d6da497702202e08f7b125b040d4b6fb60dedc23038b2a0ace48a24842836539820ec6cc7b390121035351df6c0f42202cb8bc073cfe9ad5202bf5196d39505adaffdd6723552fb036ffffffff29098bd0a13e550d0488a733e74d0dc534c9b89f38c3e87b7de857e789f6dafd020000006b4830450220652b47082faff6bdc9726404527d8e5d8fe6c5204e175b8c34fc9770704753da022100ae38571a73102b67e0dc97f3e5b365d92da6019e34d3be2f9384db90d502b4d60121035351df6c0f42202cb8bc073cfe9ad5202bf5196d39505adaffdd6723552fb036ffffffffbcdf8a90ea76bc9ae5a2b10c305c8466bd5d873c5784668b16805f38ee5c3c6d030000006a47304402206798d4109a0f5c7f90b219c974567170f2e975f43e18f4448d5093ebec998ada022073fce300cfa0b848e0711d6b624a3ee9bdcdbf15f0c0f21aecea32399d5fb54c0121035351df6c0f42202cb8bc073cfe9ad5202bf5196d39505adaffdd6723552fb036ffffffffd76a56e283857b34b8b33513689f05a86f24058a0da791c5a32a52a346e8c605020000006c493046022100f53a6a0f12d9e838f2c98fb1a76cb8478665a2bea2b09b1c19ded29883687cd50221008e3e3a949906dfa37d48e3775aec2931cecdff066b7e1a91a77a87e26edd7c3e0121035351df6c0f42202cb8bc073cfe9ad5202bf5196d39505adaffdd6723552fb036ffffffff02948300b0000000001976a9145424671b568c291219aa1eea1221ed7a7f856cef88ac00182b4c031d00001976a91478b7a2cf3e92cc7d77bab3647c01472622b5af9788ac00000000010000001706dd2998af19673bf49d94645e9783f0b78914e848b60501252d05b2ff86dc440e0000006c4930460221008fafe8848ff653fdd84e36b65ce2dba2858653d41b61b7beb367f26319d368af022100ce043630a8e594d1fb034722aed7d64b854fa63375d8973f31c83641229291900121026ac9e93e84ff8f7291cd9d61706a8a43fe7f450e1f750626b979198f8f0b63c3ffffffff02a8ddde58b0f07a6367ffbca18cd79793c9bbab76c51492b9e1cecf4f152dff000000006b48304502206fa1a08c688a47c2a068e8fcd756e832d66ecc8b36fb84bbd69c8f09506cd5ba022100f4304458fff39d5fe777e7dccce8b98d0dae3c9f926b9e660cf368fcfd915b9c012103fea7de3ce890f2ff42a0d7f26f80794d6ad31acf4798dbb4e68e47cf86b4e661ffffffff2132906fd9ea511829000979762417a18114f08aaf697c1e0af9455a899cf69a000000006a4730440220772e80b70b70fd1e58ceba37baccc9a292e9aff6d2ef17d09d54584d518161a3022022e6a0c1fc5512423db1dbf6563cfdde216bfa42c33659fab7af63e426aa6f5b0121026ac9e93e84ff8f7291cd9d61706a8a43fe7f450e1f750626b979198f8f0b63c3ffffffff47137626a059f33006792954486ba2f04cf766ed32b74ad2c5095dec8c995b6c460000006a47304402203c0389a17f1c2d4b46512d21eb72967fac9b589b52882d2099da349210818bba02207e79e6cde5b10fafe3e1327279029e0247fc1bd56bd191616f9c032e6594b2e0012103676fe6318ff93763d2b8d38765bcd438036f5908776d60cc427b5d8867da7a4cffffffff2e6fd297f91b7c3c4820852e7495a74ae24409275b516a7fbe62b3392e6ce1f6110000006c493046022100839382918db517af2e6e54ca340ab5d88600617b866d32cb7b61a83e8c544549022100bb9c300c1222a157de34d67ef1579646afd3ce402bc1f6556f241ef457f78dbd012103676fe6318ff93763d2b8d38765bcd438036f5908776d60cc427b5d8867da7a4cffffffff0d203335959775ddc959e499f75ef8cceb9c0f45582aef0ed44923a72eb43f831c0000006b48304502201fb991edf4704ae245c0f1bf2166e4a29ee37ac27d62ba124d9d6c4b0acd745e022100b316a0acbcbab14dde2c20134699ace0e08f31755f659ef8659108bc43b08bcc012103676fe6318ff93763d2b8d38765bcd438036f5908776d60cc427b5d8867da7a4cffffffff44ab98298016bb732e5a2a7efe110296aed1e1cd42288a661d167a17261f3f825a0000006c493046022100bc54bc8636a79a810ef1b7798aca10ab17fe19f601ddd4170dc14f2bef526001022100cf6720d774bc180ddef2c2dc1a4ff163c2c8827b08ee8923289dbef51eec19d9012103676fe6318ff93763d2b8d38765bcd438036f5908776d60cc427b5d8867da7a4cffffffff42ee5c50f8733fb6d53dfbbebca7c83ff1c71b1af4b63552da35b4d4366f6dae910000006c493046022100b607a8f137e248e863d650740a0a6fc818a61bda1d0702d95724af261585f80c022100a83388305e43107a7741e03c6131b73b15de858df5ed3b89e8efea1306242fa4012103676fe6318ff93763d2b8d38765bcd438036f5908776d60cc427b5d8867da7a4cffffffffc8424126ec227d3937578f0d5d9f532a1d836974a0ef00ceb1255fbca03cfbae620000006b483045022042e1705d21d7cbe8048875ff0b50642e0bada350dd3bf44f1fc475d7e00511140221009b95128996bbe36715aabb627bbefb376ac96bf0db13b4e5ef551643d65cdcb1012103676fe6318ff93763d2b8d38765bcd438036f5908776d60cc427b5d8867da7a4cffffffff9edd8de4a2e852cf120339bb8d7b87f690b885e280cb8682b53e8bf2807c92fd490000006c493046022100f2ec94aa85f0e16c4fd5a1c5443c56ee5968731c77cedaa54544509047e97048022100a7c76e4c8bf29553ff44410b30150ca45c810992c885866e8eea7837fc68c4f4012103676fe6318ff93763d2b8d38765bcd438036f5908776d60cc427b5d8867da7a4cffffffff570f5ac1b115ecd5212d9a7d202936dfacdacbcc1ec543b6cfdb6f0100322781110000006c493046022100c746d6b27b83325b6e8a9c3eb35687c83dfa246c658f3187d941c03de8a299c9022100d4f080a7527d70c5cd7a3358bba2fe76591634f9c9d85e998615de27db862d530121026ac9e93e84ff8f7291cd9d61706a8a43fe7f450e1f750626b979198f8f0b63c3ffffffff6155664b353cd861f65b308fe594b29a49444eda882ff1708da7778d8e4e9d4f010000006c493046022100d72445addc1cda74ac309a2f3bf76fa41dd3aaf61fe4ded5b3b5df5ca7f00211022100fbe9614110a2d1de1bca8cd57c06ef71dba740fb1f32ddf6b8d3ba83a2d545330121022a06c8f071ebc4fabe2b8f136ac7a5652e6c85a8ddff0a74752bf164272562daffffffff4cb7cfbaba8b66a823c78462aef5c3c123111b01e832317d7887be2d08b726bf6b0000006b483045022100dfa242e827fcaed8b3b8c0a2244e8236e0a132773ee2c959800cc4ffa748dc8102200f560224b5cafd9364ad7f1bf3516159a92da2f6ab1fc89aca6499b85e719e8f012103676fe6318ff93763d2b8d38765bcd438036f5908776d60cc427b5d8867da7a4cffffffffaf934e6e0747b137be6c11b40fa4fa2844998858400755fd4d5098731e51f8f1010000006b483045022100811fea3157e9a69a9d9b57ab596d20e8fb4963d9fe7d7f37819d582a61d4c09e022074c343556af04ff3d7ba1981fb3e7da946d9e8e58f90958d905f32095cffee32012102e30920d125fb5cca2c3d359d49836a1a9eb3e0ac22a3962055febcc917200babffffffff8d67f5585127eed7a7becf32c3b092e6cd28b748f96ddbcba6dc1d729448ad8b2c0000006a4730440220445edc37ef8628ae46ade7f076736eb344bc8468ce6f391e740838af56e3928b02205ae85a93dce60ba6066da020a880b958ee2d15cef4d2aa146d1c44f1779e5c80012103676fe6318ff93763d2b8d38765bcd438036f5908776d60cc427b5d8867da7a4cffffffff5e2210ad461995143f81f57d7c973db5ab21666cfe84d7a94bd96c3184788e27420000006b48304502201f1370e93743526e36183c04fa991a389e26599b6028a68bbd4e7fd1ae313b1f02210088e6924d8186bdd5bc4ab38fac5fe1984afb81ac8daf685c20837c8526383536012103676fe6318ff93763d2b8d38765bcd438036f5908776d60cc427b5d8867da7a4cffffffffbec698a24f6cc825bacd9dbd79a519102bd841d88bed319973a1609bce024c520b0000006c49304602210094c079bdc53021cc738827c4b06036aabb90cd8df91612cea41a1b78e276aa50022100e4e8e1b9b4d0a12c4fda9e54e082f366c41142868d096aef3e74f5924263eb1b012103676fe6318ff93763d2b8d38765bcd438036f5908776d60cc427b5d8867da7a4cffffffffae45b479bb0735138ffbc73efa5d4ead15bc4937ef18d5883e89c8919f547972450000006b48304502203b779b9bbecdb30a85e9c3e8d9462dfa8a5a5b0dfb33304a9d27c1706d2628c0022100e076ebf59cbe41337865ad53e3fab1b74865f8b9ae59083e5cd246606cc95e20012103676fe6318ff93763d2b8d38765bcd438036f5908776d60cc427b5d8867da7a4cffffffffcf373dac619c3ce96984e6cba69822e1f5bbf2dc1ffbb58143e8212b5c523fdd5d0000006c493046022100fd97457859adc823719077813fab06e0c6cff9d10ad0a72cab22ca1a4bb75ab3022100ca1c8934e94ab4d720186f4e4a451e0040751adf5db9d3722797091d99733249012103676fe6318ff93763d2b8d38765bcd438036f5908776d60cc427b5d8867da7a4cffffffffd8f9fb423f7e8ca13c1b53c33833cc1166e8f625ff115ef2486060f2534487f3580000006b483045022100edd3d093d62fe84e086706d2489902ab32f9d03c487f614309ea1966a0ac1a4e02202b78e8972a548d3c340df277f6d5a04db90ca9d228436ad3f76a4b53f6185478012103676fe6318ff93763d2b8d38765bcd438036f5908776d60cc427b5d8867da7a4cffffffffdcfd08dca11a619c14f3a72cdbcca34b6de0f2d5a19f8b88fc487a75b4697f79310000006c493046022100add906be5284a7857b86a977385cafebfd49e6542bda8c1cf0bc6d25ee4e992e022100d7a45f576095e70b995b828e72eb9ea1425f354d5c382c279ef3432e40e2c3c10121026ac9e93e84ff8f7291cd9d61706a8a43fe7f450e1f750626b979198f8f0b63c3ffffffffdfc8de73e56b69f28aa040328e3d2c7be895fbdfaa2a01604794b04061a63921500000006a47304402202e0cdbafe29c5c66307bb96a629cb783b20f55292a7f2bac61691edb52391126022067f915632046f3c19f8c3a6a2c70c39a8fad10ece24e889a7a188f214c098bf9012103676fe6318ff93763d2b8d38765bcd438036f5908776d60cc427b5d8867da7a4cfffffffff5790c45b3bcb78e780dafc82e99aaf964bcc2f0adb63e078d2bc213941b44d42f0000006b483045022100c64c3123e66f7433dedc602c705cd0c87384d779720c030d40eed0e8f5435c6802204a69c8d9d4a06a26f4dc46113f44d20570a0cbc21bae70701a076e5b895be069012102e30920d125fb5cca2c3d359d49836a1a9eb3e0ac22a3962055febcc917200babffffffff01f00eec839f0000001976a9142195c316dd8968f429838ffb5f8ebbb6c4241ac788ac000000000100000012cc16dbf7e4fa0956febffb0c497566f3d3dfb747d932181b2522ec39f03c0f3d010000006a4730440220707e6cfe54a77afaad3f310672d5d3b9295db24c1023d5d5b0ff571e7da9e2fa022050ed4158a8942660984ed2f37e2b8d328ba3f4c2f1dea2eb08bf0d493b0310d80121025aee294342369fed15ed52acec35f859d502f87ec4351f14cb1f02502fa9cba1ffffffff027afe7959c7e05871a1607d76b86d589b8d3f0d439dea69ccb953be896cf61e000000006b4830450221008a17ef441072829c6ff6465ff75f7e5c9acacd82e28c12ddf2cc615ac241a0080220406340f80fe31d8993963a4f4f513219ace53db04f6e1bfae208e2eec69793700121025383107649c1644f290a652dea78e1d3c11b9d8c23b5430b2f1ca6cbc506d016ffffffffd5261f6d9dfd9d096e40711c3e2755fee08afce04a4ab7f2a6141399550f9e2a010000006b483045022100b0b8aee86da038305886451952661dec5894e27082e7244fb0b24d88d70c234c022029e35dafb1b926d797571919eb250fa4dae8f5be04c9f79b4d9423966e2a1d1f012102f236b9f05df79b5146054ba09e300ef92cd730bc318e2d6dcc2b12ddeb735ccaffffffffa60038ac4bcd70cdbd7f14d55cd157fb3b8b60802f930c99258325d92b71144a000000006a473044022051828d14e9e55bc58bed941aac4ba2f20fc6b072ec68fe4c4ec2ff603af3f9f102206ecb7bb850f5c863361ce11414dd6c8213dbafb608545aa638a6a46693703f08012102716da99991bb15383edc4b0226ae17681a0a4990410795fd8732083c295f6e11ffffffff1486b50c466227d4d4c628a4fb97832718e0062125bbeebfb5e825088af9beb8060000006b48304502203e8eb7ef5b9cda96eade93b133e15ae35e83ffcabc09fda24621d8cf98f69531022100cdb606c8562fca0a5840616f903da53c4e518c6adfcda7c4d029c27800cfe82b012102e8e380a2bda7f91a1a1eb5ed074b2740b029c71a6d9909d6b156ecc825325257ffffffff99250b40d726615bd6a7fba35b8b3963d80c6a53af9bc08c32f9153bc0cdee4b000000006b4830450220087b0a5d3b06c15e6aa082619d8276d219f6ddbedcbd13f63f49244a699be76c022100a489809759a454803b2f510843ca382d5f611b088d3ae4f0e7600bf045e7f9f2012103661e0218fafb5c9f4a0122fe39d8a11d43bd3777c9f7d064dfc8610163abf7d3ffffffffe31ecb710240f59d95f0beb445540027183a859eeb00af78644bd413124c4a19000000006a4730440220155d77ab0442ef48b09c59d9d9407e09bbececd9078ac32f23c8021acd1b8bbf02201dc712a85d8514306849aef4aa18ba88c595003f850c19ba7aa5298c689f3905012102cc0f7a33b4410efe47ccf5eefe95dc5c079cd6fae3a84be7f25d2dcbf983ff72ffffffff9678a8c854edea062f28aeca9b79508445da1e0aa17c1b9f1ac26792966e6bac010000006b48304502200a3968154209fdbd1e6d5d2ccb9e38b7b44138e9253c96afe19b175312ef7d4e022100d01b3f6d0f58f6f992c004fe881a9ede3ef46b1ad1bfb5d065c932f6773238de012103385cae722dc0ff48eed552f18716d7adae61ba3e1eb63fa5253af75f52ff9827ffffffff842891f398fa10f9e2ebcb109376c53a65a3d3cb1ea47bbf03eb44e9490f18070f0000006b48304502200d1eb9b02535b4e457b09b9a57daf151250536d6ca0c6741dcced79adc76e75f022100cb0b5bc368866cd6f8df8525aacce2ea3c5fdd7efa66ecad8958a36fc52542a00121037fdbf416236634bd1bedda3ac49616d1e9b30c9d6999a1c1cf0d9d5c57ee8e9affffffff842891f398fa10f9e2ebcb109376c53a65a3d3cb1ea47bbf03eb44e9490f1807110000006b4830450221008b95b65312777f0bf960a618d15e9df22a96fd75d71f402f960523e2a6d7537902201b6e9b5e056b780b9529bb0ad592f30e78d348c5d5d282b9d96350d75a0236600121028fd9032536c66ce92afc3fb63afd91f5f9e2bfe3d9a2fa9f9ed275581ae436e4ffffffff8cd32d6598e59edb4b473a010a3ab32227694de5bce1338fca18d13fa7c5e5fc000000006b483045022100b45557196ee3d3146aba1bb8e52c2e82c06b7a4fb4ae01ae3a1fd3a8e8181a5502200121df38394b2a4a76be660bd506ebd9906cdad6418c6cbb23fa3ae656703f54012103c29b40e044d56cc1c6c4f5e1e3a7be1c576010f892fcefd952103896dce09a82ffffffff816992968e94cb003ea7261ef4fb10b42b928bd8b4cd6f547b279b8807699ba9000000006b483045022100c045f8220c7a40059dabb828044db5fa495ab012c90dffca2da84d407a2e8cc1022044a03b9c23bea9f3e17da32c11a08b3dd22a7fb700d9ad4fcf1b70970f06d9a3012102e720b78c0440fb8258ea73102a9b5620612b8566fb1169ab4d561f6c9dffadacffffffff1b3d9d58dc116af937ee8d60644772d882703ff5205dae735fb1e685be2d1f79000000006c493046022100b1da4daef233655c650327b6a98efb23219287c3621d6bb7402429e2f4bf7b8902210096e22ad4a3c87632d238f124401103197e1378038206a0ac5709364560a6eeb9012103f320f95dbf91795d651abf9ae39ae231cfb92cb911686f91f77301976831166bffffffffdde2d5777ba75c73e57e44a13e9519c7032211067328ff0b43fc101afca55737000000006b48304502206274d02b8be3173c776c0d030df737c8cc666a62ba7baa02cf4928f2b1b5e6ba022100aa177b2d48d9c0a026dadf8118387d7a5b8c7dec95116f49d4f4ef334e25bf4b012102933c75ec87cdd98c096f4a7b92839c83d25372a1c036db25b393b329ec938e23ffffffff90730ff65ff21ab1f83dbe6dd9eb7a0ff9d5b93c79e1eb19db85600662546c26000000006b4830450221008071860eca5cf91af4ee4bebdaa0d1e8c9dd49ef3621c1737647daea5209d2860220550cf4d9b50d3906b8af0c17470042922b292338054b60e476fa9719c645505901210231954c06593264542b5e501fd2414651e0ffa1b78db471acbf40a7bd5adc80cfffffffff3d3a019763fb78db6d90d71be14cf4e61bf7685e86976b24fa44cb15fe3ed123140000006b48304502205f85f05afec153a322b0846faa3dac3ab3740f38fb16078b3c6c150001e8e2f8022100f1f6cfda95ac47560b2f5390f19a63689c2e8daecc14812bab2fdefdcfa9210b012102481bf8186729c0b5c3e6bd41e09354dae071c24d450195c9a13943bb2a7b0b15ffffffff2561dd82c30a1087988a907129b339cf2d79e2468650ccf78af872552e34df97000000006b483045022100969b1a7713e437345956a1e07f399a7a156da421f47a939bb18a3c62fa172ed402206ba5654c84a7d64450ceed55ee312e8cab0c5cacbbf4d9c578d078e5110770490121021869237032773a94abae7746d3f7ca7cffd5743ddd02df2754a764e4d5c49e7dffffffffc100de95afcaa8aa1e1f0ee2adb90e0508e06e113b1906ccdda02f8589a91b33010000006b483045022100ce7c0d4664201522b0587d74445e99991cd507fbdf95ef51ca59025c4dc6521f022005a27bfae79f4f3e23774502b644c0b302d24ba086eec3fffa58bf300777d35d012103405dfbc83a8725ba5ad104f3a9b7a92cab7285bba8c67045f8ed2fdf49755ab5ffffffff02c130fd05000000001976a914c181bcf8310a1a0f48d779542719c0b550954f6388ac0057a027f35b00001976a9148c836ebe6d6c069210313cb4b12640892a18b16e88ac000000000100000001e52689829911a9929a8e1bf93e840d6ae76d3cbc3ef5a459e5d074be6a426148000000006a473044022062b81a9a7ef2526ba58dd1d775ad8b044073d2870c61a37089ee4fe81f88a4b702206368230b666f959d4c9af1735fedcb867e5dc3639119bceab94c120a8dffa23a01210271e872268ee254dd442dca50a961f8737c686f03e25cd83f6b4978dc1d12321bffffffff020058e5403e0200001976a91485e6838eabaf40c28cacffad0d26eb481063ce8988ac00cdb089020000001976a914abe53121cb50ba3ad3ad841d0f747b2f6cb27d2a88ac000000000100000001bd911a141144d2bba186258b26766b8ff9ae068c9543456ed337c4948fbe23c0000000006c493046022100a17355dedaa63c2dee27f4ae74c8c69fb2b5814a8b23eae2e08bd0bbd22dfc240221008baa6bcfef96e79908f32f5f897443ceee3bbdc7f41f67f3cb7455e0bbabcdc60121020c9563b6fd673f0ede1189d6d8d273405b452f13caf80daba3a57a90b99fb1a2ffffffff0200a3e111000000001976a914b678be79d0b3022a5ac33701b865b8190314b07588ac00172d146b0000001976a914d2bc2634fc4e90110ce3279e8d8ba6a930c32b9b88ac0000000001000000012343d68de93e2c5c7a3968ac7720ea84ec6342fc29ff9e28709a7a72a8153b76010000006b483045022100ec2a47f13c418e3a081d08f85973c27191360ff2ef9b8861b3e1e7f9c5e1eb3d022030beb37dad4c7317efc08680b52bfee0ff7a510c327ec7e0eb545d59e1ac1fee0121034e4106c620ddf4d4c5c22d2c92e60a645145151d2a66076e70820f96b16bcd94ffffffff02a85ca8fa3a0000001976a914b83406da0cd1fa8fce9ce5106787bb629a01d29b88ac00e1f505000000001976a9149d6ccd255d416f5388ea0ef6f7adcf39ae1c229688ac0000000001000000010e3771c7224e7038184a575c9c4a2cfbc3777e77a5b9232e22ed04ab8a10c47a000000006a47304402207b1290b0fea687039b59e03e2b1816599b61c2a3c4c00d3a8b9869222f16702c02201747b04f73736ddf6c6fd03664c8f5e7b6ca32873eecdf153cb329fd5cb9d1dd012102a2ca9960c365643887ed4d1565f7bf882a0bcef1f67ef4c1d6621749103fa936ffffffff02a76fbb20000000001976a914da9d7fb9e9f54847719905160e3cd96fe11feaf088ac59819844000000001976a914c14de2741e29ef8343e378de781998ec999c106b88ac000000000100000001dba8d4be01fa0ad0bb586a8348627303c5579f299e1f3b85b9afc35064503a44000000006b48304502206823e835c0f92a3e21a628e77be64c021e13761eee735715d54ae86a73fa731f02210097d4e1d507957fd31fb590dd8ef9f59a731255275950b1d65f9cc1897c0d3c6401210308c5a2f185cdf2becb38129a7db2a3f2e1aa78d26f1a499cae44d045c4cc913affffffff02002f6859000000001976a914a998e3566412300c799efdbca918502af1d2267088ac004887e13d0200001976a9148c78ca287a9b7053632a3c9f6b4b354dc55666a088ac00000000"), SER_NETWORK, PROTOCOL_VERSION);
    stream >> block;

    CBloomFilter filter(10, 0.000001, 0, BLOOM_UPDATE_ALL);
    // Match the last transaction
    filter.insert(uint256("0x9d9a6172f6082ab368e901b26e8f0a5258aa75c7291137bdce62ac0839e93ef1"));

    CMerkleBlock merkleBlock(block, filter);
    BOOST_CHECK(merkleBlock.header.GetHash() == block.GetHash());

    BOOST_CHECK(merkleBlock.vMatchedTxn.size() == 1);
    pair<unsigned int, uint256> pair = merkleBlock.vMatchedTxn[0];

    BOOST_CHECK(merkleBlock.vMatchedTxn[0].second == uint256("0x9d9a6172f6082ab368e901b26e8f0a5258aa75c7291137bdce62ac0839e93ef1"));
    BOOST_CHECK(merkleBlock.vMatchedTxn[0].first == 8);

    vector<uint256> vMatched;
    BOOST_CHECK(merkleBlock.txn.ExtractMatches(vMatched) == block.hashMerkleRoot);
    BOOST_CHECK(vMatched.size() == merkleBlock.vMatchedTxn.size());
    for (unsigned int i = 0; i < vMatched.size(); i++)
        BOOST_CHECK(vMatched[i] == merkleBlock.vMatchedTxn[i].second);

    // Also match the 8th transaction
    filter.insert(uint256("0xc218bab168eb6e4dd1e9bd10a5cf9ca23eba81fb4f917bbb86709f3c1c2ecb68"));
    merkleBlock = CMerkleBlock(block, filter);
    BOOST_CHECK(merkleBlock.header.GetHash() == block.GetHash());

    BOOST_CHECK(merkleBlock.vMatchedTxn.size() == 2);

    BOOST_CHECK(merkleBlock.vMatchedTxn[1] == pair);

    BOOST_CHECK(merkleBlock.vMatchedTxn[0].second == uint256("0xc218bab168eb6e4dd1e9bd10a5cf9ca23eba81fb4f917bbb86709f3c1c2ecb68"));
    BOOST_CHECK(merkleBlock.vMatchedTxn[0].first == 7);

    BOOST_CHECK(merkleBlock.txn.ExtractMatches(vMatched) == block.hashMerkleRoot);
    BOOST_CHECK(vMatched.size() == merkleBlock.vMatchedTxn.size());
    for (unsigned int i = 0; i < vMatched.size(); i++)
        BOOST_CHECK(vMatched[i] == merkleBlock.vMatchedTxn[i].second);
}

BOOST_AUTO_TEST_CASE(merkle_block_2)
{
    // Random real block (000000005a4ded781e667e06ceefafb71410b511fe0d5adc3e5a27ecbec34ae6)
    // With 4 txes
    CBlock block;
    CDataStream stream(ParseHex("0100000075616236cc2126035fadb38deb65b9102cc2c41c09cdf29fc051906800000000fe7d5e12ef0ff901f6050211249919b1c0653771832b3a80c66cea42847f0ae1d4d26e49ffff001d00f0a4410401000000010000000000000000000000000000000000000000000000000000000000000000ffffffff0804ffff001d029105ffffffff0100f2052a010000004341046d8709a041d34357697dfcb30a9d05900a6294078012bf3bb09c6f9b525f1d16d5503d7905db1ada9501446ea00728668fc5719aa80be2fdfc8a858a4dbdd4fbac00000000010000000255605dc6f5c3dc148b6da58442b0b2cd422be385eab2ebea4119ee9c268d28350000000049483045022100aa46504baa86df8a33b1192b1b9367b4d729dc41e389f2c04f3e5c7f0559aae702205e82253a54bf5c4f65b7428551554b2045167d6d206dfe6a2e198127d3f7df1501ffffffff55605dc6f5c3dc148b6da58442b0b2cd422be385eab2ebea4119ee9c268d2835010000004847304402202329484c35fa9d6bb32a55a70c0982f606ce0e3634b69006138683bcd12cbb6602200c28feb1e2555c3210f1dddb299738b4ff8bbe9667b68cb8764b5ac17b7adf0001ffffffff0200e1f505000000004341046a0765b5865641ce08dd39690aade26dfbf5511430ca428a3089261361cef170e3929a68aee3d8d4848b0c5111b0a37b82b86ad559fd2a745b44d8e8d9dfdc0cac00180d8f000000004341044a656f065871a353f216ca26cef8dde2f03e8c16202d2e8ad769f02032cb86a5eb5e56842e92e19141d60a01928f8dd2c875a390f67c1f6c94cfc617c0ea45afac0000000001000000025f9a06d3acdceb56be1bfeaa3e8a25e62d182fa24fefe899d1c17f1dad4c2028000000004847304402205d6058484157235b06028c30736c15613a28bdb768ee628094ca8b0030d4d6eb0220328789c9a2ec27ddaec0ad5ef58efded42e6ea17c2e1ce838f3d6913f5e95db601ffffffff5f9a06d3acdceb56be1bfeaa3e8a25e62d182fa24fefe899d1c17f1dad4c2028010000004a493046022100c45af050d3cea806cedd0ab22520c53ebe63b987b8954146cdca42487b84bdd6022100b9b027716a6b59e640da50a864d6dd8a0ef24c76ce62391fa3eabaf4d2886d2d01ffffffff0200e1f505000000004341046a0765b5865641ce08dd39690aade26dfbf5511430ca428a3089261361cef170e3929a68aee3d8d4848b0c5111b0a37b82b86ad559fd2a745b44d8e8d9dfdc0cac00180d8f000000004341046a0765b5865641ce08dd39690aade26dfbf5511430ca428a3089261361cef170e3929a68aee3d8d4848b0c5111b0a37b82b86ad559fd2a745b44d8e8d9dfdc0cac000000000100000002e2274e5fea1bf29d963914bd301aa63b64daaf8a3e88f119b5046ca5738a0f6b0000000048473044022016e7a727a061ea2254a6c358376aaa617ac537eb836c77d646ebda4c748aac8b0220192ce28bf9f2c06a6467e6531e27648d2b3e2e2bae85159c9242939840295ba501ffffffffe2274e5fea1bf29d963914bd301aa63b64daaf8a3e88f119b5046ca5738a0f6b010000004a493046022100b7a1a755588d4190118936e15cd217d133b0e4a53c3c15924010d5648d8925c9022100aaef031874db2114f2d869ac2de4ae53908fbfea5b2b1862e181626bb9005c9f01ffffffff0200e1f505000000004341044a656f065871a353f216ca26cef8dde2f03e8c16202d2e8ad769f02032cb86a5eb5e56842e92e19141d60a01928f8dd2c875a390f67c1f6c94cfc617c0ea45afac00180d8f000000004341046a0765b5865641ce08dd39690aade26dfbf5511430ca428a3089261361cef170e3929a68aee3d8d4848b0c5111b0a37b82b86ad559fd2a745b44d8e8d9dfdc0cac00000000"), SER_NETWORK, PROTOCOL_VERSION);
    stream >> block;

    CBloomFilter filter(10, 0.000001, 0, BLOOM_UPDATE_ALL);
    // Match the first transaction
    filter.insert(uint256("0xe980fe9f792d014e73b95203dc1335c5f9ce19ac537a419e6df5b47aecb93b70"));

    CMerkleBlock merkleBlock(block, filter);
    BOOST_CHECK(merkleBlock.header.GetHash() == block.GetHash());

    BOOST_CHECK(merkleBlock.vMatchedTxn.size() == 1);
    pair<unsigned int, uint256> pair = merkleBlock.vMatchedTxn[0];

    BOOST_CHECK(merkleBlock.vMatchedTxn[0].second == uint256("0xe980fe9f792d014e73b95203dc1335c5f9ce19ac537a419e6df5b47aecb93b70"));
    BOOST_CHECK(merkleBlock.vMatchedTxn[0].first == 0);

    vector<uint256> vMatched;
    BOOST_CHECK(merkleBlock.txn.ExtractMatches(vMatched) == block.hashMerkleRoot);
    BOOST_CHECK(vMatched.size() == merkleBlock.vMatchedTxn.size());
    for (unsigned int i = 0; i < vMatched.size(); i++)
        BOOST_CHECK(vMatched[i] == merkleBlock.vMatchedTxn[i].second);

    // Match an output from the second transaction (the pubkey for address 1DZTzaBHUDM7T3QvUKBz4qXMRpkg8jsfB5)
    // This should match the third transaction because it spends the output matched
    // It also matches the fourth transaction, which spends to the pubkey again
    filter.insert(ParseHex("044a656f065871a353f216ca26cef8dde2f03e8c16202d2e8ad769f02032cb86a5eb5e56842e92e19141d60a01928f8dd2c875a390f67c1f6c94cfc617c0ea45af"));

    merkleBlock = CMerkleBlock(block, filter);
    BOOST_CHECK(merkleBlock.header.GetHash() == block.GetHash());

    BOOST_CHECK(merkleBlock.vMatchedTxn.size() == 4);

    BOOST_CHECK(pair == merkleBlock.vMatchedTxn[0]);

    BOOST_CHECK(merkleBlock.vMatchedTxn[1].second == uint256("0x28204cad1d7fc1d199e8ef4fa22f182de6258a3eaafe1bbe56ebdcacd3069a5f"));
    BOOST_CHECK(merkleBlock.vMatchedTxn[1].first == 1);

    BOOST_CHECK(merkleBlock.vMatchedTxn[2].second == uint256("0x6b0f8a73a56c04b519f1883e8aafda643ba61a30bd1439969df21bea5f4e27e2"));
    BOOST_CHECK(merkleBlock.vMatchedTxn[2].first == 2);

    BOOST_CHECK(merkleBlock.vMatchedTxn[3].second == uint256("0x3c1d7e82342158e4109df2e0b6348b6e84e403d8b4046d7007663ace63cddb23"));
    BOOST_CHECK(merkleBlock.vMatchedTxn[3].first == 3);

    BOOST_CHECK(merkleBlock.txn.ExtractMatches(vMatched) == block.hashMerkleRoot);
    BOOST_CHECK(vMatched.size() == merkleBlock.vMatchedTxn.size());
    for (unsigned int i = 0; i < vMatched.size(); i++)
        BOOST_CHECK(vMatched[i] == merkleBlock.vMatchedTxn[i].second);
}

BOOST_AUTO_TEST_CASE(merkle_block_2_with_update_none)
{
    // Random real block (000000005a4ded781e667e06ceefafb71410b511fe0d5adc3e5a27ecbec34ae6)
    // With 4 txes
    CBlock block;
    CDataStream stream(ParseHex("0100000075616236cc2126035fadb38deb65b9102cc2c41c09cdf29fc051906800000000fe7d5e12ef0ff901f6050211249919b1c0653771832b3a80c66cea42847f0ae1d4d26e49ffff001d00f0a4410401000000010000000000000000000000000000000000000000000000000000000000000000ffffffff0804ffff001d029105ffffffff0100f2052a010000004341046d8709a041d34357697dfcb30a9d05900a6294078012bf3bb09c6f9b525f1d16d5503d7905db1ada9501446ea00728668fc5719aa80be2fdfc8a858a4dbdd4fbac00000000010000000255605dc6f5c3dc148b6da58442b0b2cd422be385eab2ebea4119ee9c268d28350000000049483045022100aa46504baa86df8a33b1192b1b9367b4d729dc41e389f2c04f3e5c7f0559aae702205e82253a54bf5c4f65b7428551554b2045167d6d206dfe6a2e198127d3f7df1501ffffffff55605dc6f5c3dc148b6da58442b0b2cd422be385eab2ebea4119ee9c268d2835010000004847304402202329484c35fa9d6bb32a55a70c0982f606ce0e3634b69006138683bcd12cbb6602200c28feb1e2555c3210f1dddb299738b4ff8bbe9667b68cb8764b5ac17b7adf0001ffffffff0200e1f505000000004341046a0765b5865641ce08dd39690aade26dfbf5511430ca428a3089261361cef170e3929a68aee3d8d4848b0c5111b0a37b82b86ad559fd2a745b44d8e8d9dfdc0cac00180d8f000000004341044a656f065871a353f216ca26cef8dde2f03e8c16202d2e8ad769f02032cb86a5eb5e56842e92e19141d60a01928f8dd2c875a390f67c1f6c94cfc617c0ea45afac0000000001000000025f9a06d3acdceb56be1bfeaa3e8a25e62d182fa24fefe899d1c17f1dad4c2028000000004847304402205d6058484157235b06028c30736c15613a28bdb768ee628094ca8b0030d4d6eb0220328789c9a2ec27ddaec0ad5ef58efded42e6ea17c2e1ce838f3d6913f5e95db601ffffffff5f9a06d3acdceb56be1bfeaa3e8a25e62d182fa24fefe899d1c17f1dad4c2028010000004a493046022100c45af050d3cea806cedd0ab22520c53ebe63b987b8954146cdca42487b84bdd6022100b9b027716a6b59e640da50a864d6dd8a0ef24c76ce62391fa3eabaf4d2886d2d01ffffffff0200e1f505000000004341046a0765b5865641ce08dd39690aade26dfbf5511430ca428a3089261361cef170e3929a68aee3d8d4848b0c5111b0a37b82b86ad559fd2a745b44d8e8d9dfdc0cac00180d8f000000004341046a0765b5865641ce08dd39690aade26dfbf5511430ca428a3089261361cef170e3929a68aee3d8d4848b0c5111b0a37b82b86ad559fd2a745b44d8e8d9dfdc0cac000000000100000002e2274e5fea1bf29d963914bd301aa63b64daaf8a3e88f119b5046ca5738a0f6b0000000048473044022016e7a727a061ea2254a6c358376aaa617ac537eb836c77d646ebda4c748aac8b0220192ce28bf9f2c06a6467e6531e27648d2b3e2e2bae85159c9242939840295ba501ffffffffe2274e5fea1bf29d963914bd301aa63b64daaf8a3e88f119b5046ca5738a0f6b010000004a493046022100b7a1a755588d4190118936e15cd217d133b0e4a53c3c15924010d5648d8925c9022100aaef031874db2114f2d869ac2de4ae53908fbfea5b2b1862e181626bb9005c9f01ffffffff0200e1f505000000004341044a656f065871a353f216ca26cef8dde2f03e8c16202d2e8ad769f02032cb86a5eb5e56842e92e19141d60a01928f8dd2c875a390f67c1f6c94cfc617c0ea45afac00180d8f000000004341046a0765b5865641ce08dd39690aade26dfbf5511430ca428a3089261361cef170e3929a68aee3d8d4848b0c5111b0a37b82b86ad559fd2a745b44d8e8d9dfdc0cac00000000"), SER_NETWORK, PROTOCOL_VERSION);
    stream >> block;

    CBloomFilter filter(10, 0.000001, 0, BLOOM_UPDATE_NONE);
    // Match the first transaction
    filter.insert(uint256("0xe980fe9f792d014e73b95203dc1335c5f9ce19ac537a419e6df5b47aecb93b70"));

    CMerkleBlock merkleBlock(block, filter);
    BOOST_CHECK(merkleBlock.header.GetHash() == block.GetHash());

    BOOST_CHECK(merkleBlock.vMatchedTxn.size() == 1);
    pair<unsigned int, uint256> pair = merkleBlock.vMatchedTxn[0];

    BOOST_CHECK(merkleBlock.vMatchedTxn[0].second == uint256("0xe980fe9f792d014e73b95203dc1335c5f9ce19ac537a419e6df5b47aecb93b70"));
    BOOST_CHECK(merkleBlock.vMatchedTxn[0].first == 0);

    vector<uint256> vMatched;
    BOOST_CHECK(merkleBlock.txn.ExtractMatches(vMatched) == block.hashMerkleRoot);
    BOOST_CHECK(vMatched.size() == merkleBlock.vMatchedTxn.size());
    for (unsigned int i = 0; i < vMatched.size(); i++)
        BOOST_CHECK(vMatched[i] == merkleBlock.vMatchedTxn[i].second);

    // Match an output from the second transaction (the pubkey for address 1DZTzaBHUDM7T3QvUKBz4qXMRpkg8jsfB5)
    // This should not match the third transaction though it spends the output matched
    // It will match the fourth transaction, which has another pay-to-pubkey output to the same address
    filter.insert(ParseHex("044a656f065871a353f216ca26cef8dde2f03e8c16202d2e8ad769f02032cb86a5eb5e56842e92e19141d60a01928f8dd2c875a390f67c1f6c94cfc617c0ea45af"));

    merkleBlock = CMerkleBlock(block, filter);
    BOOST_CHECK(merkleBlock.header.GetHash() == block.GetHash());

    BOOST_CHECK(merkleBlock.vMatchedTxn.size() == 3);

    BOOST_CHECK(pair == merkleBlock.vMatchedTxn[0]);

    BOOST_CHECK(merkleBlock.vMatchedTxn[1].second == uint256("0x28204cad1d7fc1d199e8ef4fa22f182de6258a3eaafe1bbe56ebdcacd3069a5f"));
    BOOST_CHECK(merkleBlock.vMatchedTxn[1].first == 1);

    BOOST_CHECK(merkleBlock.vMatchedTxn[2].second == uint256("0x3c1d7e82342158e4109df2e0b6348b6e84e403d8b4046d7007663ace63cddb23"));
    BOOST_CHECK(merkleBlock.vMatchedTxn[2].first == 3);

    BOOST_CHECK(merkleBlock.txn.ExtractMatches(vMatched) == block.hashMerkleRoot);
    BOOST_CHECK(vMatched.size() == merkleBlock.vMatchedTxn.size());
    for (unsigned int i = 0; i < vMatched.size(); i++)
        BOOST_CHECK(vMatched[i] == merkleBlock.vMatchedTxn[i].second);
}

BOOST_AUTO_TEST_CASE(merkle_block_3_and_serialize)
{
    // Random real block (9244f2bffea72e43dc1b822ed0da09c484e409369bd1afafbd59a703e4262c5e)
    // With one tx
    CBlock block;
    CDataStream stream(ParseHex("02000000268bb30f3080eb5d14d36698c5508ae7ad8a5dc8a3eeedffa92bd3b0c7e1a75e7f16e2d3227be00ac6995eb407aad098762306a373872ca904cd0b06d466a1cd6dff4e53fc333b1b004680df0101000000010000000000000000000000000000000000000000000000000000000000000000ffffffff270347d002062f503253482f046dff4e530808000786030000000d2f6e6f64655374726174756d2f000000000100901ec4bc1600001976a9146f7854e81a35de4aec0a36deca6497b35320a62b88ac00000000"), SER_NETWORK, PROTOCOL_VERSION);
    stream >> block;

    CBloomFilter filter(10, 0.000001, 0, BLOOM_UPDATE_ALL);
    // Match the only transaction
    filter.insert(uint256("0xcda166d4060bcd04a92c8773a306237698d0aa07b45e99c60ae07b22d3e2167f"));

    CMerkleBlock merkleBlock(block, filter);
    BOOST_CHECK(merkleBlock.header.GetHash() == block.GetHash());

    BOOST_CHECK(merkleBlock.vMatchedTxn.size() == 1);

    BOOST_CHECK(merkleBlock.vMatchedTxn[0].second == uint256("0xcda166d4060bcd04a92c8773a306237698d0aa07b45e99c60ae07b22d3e2167f"));
    BOOST_CHECK(merkleBlock.vMatchedTxn[0].first == 0);

    vector<uint256> vMatched;
    BOOST_CHECK(merkleBlock.txn.ExtractMatches(vMatched) == block.hashMerkleRoot);
    BOOST_CHECK(vMatched.size() == merkleBlock.vMatchedTxn.size());
    for (unsigned int i = 0; i < vMatched.size(); i++)
        BOOST_CHECK(vMatched[i] == merkleBlock.vMatchedTxn[i].second);

    CDataStream merkleStream(SER_NETWORK, PROTOCOL_VERSION);
    merkleStream << merkleBlock;

    vector<unsigned char> vch = ParseHex("02000000268bb30f3080eb5d14d36698c5508ae7ad8a5dc8a3eeedffa92bd3b0c7e1a75e7f16e2d3227be00ac6995eb407aad098762306a373872ca904cd0b06d466a1cd6dff4e53fc333b1b004680df01000000017f16e2d3227be00ac6995eb407aad098762306a373872ca904cd0b06d466a1cd0101");
    vector<char> expected(vch.size());

    for (unsigned int i = 0; i < vch.size(); i++)
        expected[i] = (char)vch[i];

    BOOST_CHECK_EQUAL_COLLECTIONS(expected.begin(), expected.end(), merkleStream.begin(), merkleStream.end());
}

BOOST_AUTO_TEST_CASE(merkle_block_4)
{
    // Random real block (000000000000b731f2eef9e8c63173adfb07e41bd53eb0ef0a6b720d6cb6dea4)
    // With 7 txes
    CBlock block;
    CDataStream stream(ParseHex("0100000082bb869cf3a793432a66e826e05a6fc37469f8efb7421dc880670100000000007f16c5962e8bd963659c793ce370d95f093bc7e367117b3c30c1f8fdd0d9728776381b4d4c86041b554b85290701000000010000000000000000000000000000000000000000000000000000000000000000ffffffff07044c86041b0136ffffffff0100f2052a01000000434104eaafc2314def4ca98ac970241bcab022b9c1e1f4ea423a20f134c876f2c01ec0f0dd5b2e86e7168cefe0d81113c3807420ce13ad1357231a2252247d97a46a91ac000000000100000001bcad20a6a29827d1424f08989255120bf7f3e9e3cdaaa6bb31b0737fe048724300000000494830450220356e834b046cadc0f8ebb5a8a017b02de59c86305403dad52cd77b55af062ea10221009253cd6c119d4729b77c978e1e2aa19f5ea6e0e52b3f16e32fa608cd5bab753901ffffffff02008d380c010000001976a9142b4b8072ecbba129b6453c63e129e643207249ca88ac0065cd1d000000001976a9141b8dd13b994bcfc787b32aeadf58ccb3615cbd5488ac000000000100000003fdacf9b3eb077412e7a968d2e4f11b9a9dee312d666187ed77ee7d26af16cb0b000000008c493046022100ea1608e70911ca0de5af51ba57ad23b9a51db8d28f82c53563c56a05c20f5a87022100a8bdc8b4a8acc8634c6b420410150775eb7f2474f5615f7fccd65af30f310fbf01410465fdf49e29b06b9a1582287b6279014f834edc317695d125ef623c1cc3aaece245bd69fcad7508666e9c74a49dc9056d5fc14338ef38118dc4afae5fe2c585caffffffff309e1913634ecb50f3c4f83e96e70b2df071b497b8973a3e75429df397b5af83000000004948304502202bdb79c596a9ffc24e96f4386199aba386e9bc7b6071516e2b51dda942b3a1ed022100c53a857e76b724fc14d45311eac5019650d415c3abb5428f3aae16d8e69bec2301ffffffff2089e33491695080c9edc18a428f7d834db5b6d372df13ce2b1b0e0cbcb1e6c10000000049483045022100d4ce67c5896ee251c810ac1ff9ceccd328b497c8f553ab6e08431e7d40bad6b5022033119c0c2b7d792d31f1187779c7bd95aefd93d90a715586d73801d9b47471c601ffffffff0100714460030000001976a914c7b55141d097ea5df7a0ed330cf794376e53ec8d88ac0000000001000000045bf0e214aa4069a3e792ecee1e1bf0c1d397cde8dd08138f4b72a00681743447000000008b48304502200c45de8c4f3e2c1821f2fc878cba97b1e6f8807d94930713aa1c86a67b9bf1e40221008581abfef2e30f957815fc89978423746b2086375ca8ecf359c85c2a5b7c88ad01410462bb73f76ca0994fcb8b4271e6fb7561f5c0f9ca0cf6485261c4a0dc894f4ab844c6cdfb97cd0b60ffb5018ffd6238f4d87270efb1d3ae37079b794a92d7ec95ffffffffd669f7d7958d40fc59d2253d88e0f248e29b599c80bbcec344a83dda5f9aa72c000000008a473044022078124c8beeaa825f9e0b30bff96e564dd859432f2d0cb3b72d3d5d93d38d7e930220691d233b6c0f995be5acb03d70a7f7a65b6bc9bdd426260f38a1346669507a3601410462bb73f76ca0994fcb8b4271e6fb7561f5c0f9ca0cf6485261c4a0dc894f4ab844c6cdfb97cd0b60ffb5018ffd6238f4d87270efb1d3ae37079b794a92d7ec95fffffffff878af0d93f5229a68166cf051fd372bb7a537232946e0a46f53636b4dafdaa4000000008c493046022100c717d1714551663f69c3c5759bdbb3a0fcd3fab023abc0e522fe6440de35d8290221008d9cbe25bffc44af2b18e81c58eb37293fd7fe1c2e7b46fc37ee8c96c50ab1e201410462bb73f76ca0994fcb8b4271e6fb7561f5c0f9ca0cf6485261c4a0dc894f4ab844c6cdfb97cd0b60ffb5018ffd6238f4d87270efb1d3ae37079b794a92d7ec95ffffffff27f2b668859cd7f2f894aa0fd2d9e60963bcd07c88973f425f999b8cbfd7a1e2000000008c493046022100e00847147cbf517bcc2f502f3ddc6d284358d102ed20d47a8aa788a62f0db780022100d17b2d6fa84dcaf1c95d88d7e7c30385aecf415588d749afd3ec81f6022cecd701410462bb73f76ca0994fcb8b4271e6fb7561f5c0f9ca0cf6485261c4a0dc894f4ab844c6cdfb97cd0b60ffb5018ffd6238f4d87270efb1d3ae37079b794a92d7ec95ffffffff0100c817a8040000001976a914b6efd80d99179f4f4ff6f4dd0a007d018c385d2188ac000000000100000001834537b2f1ce8ef9373a258e10545ce5a50b758df616cd4356e0032554ebd3c4000000008b483045022100e68f422dd7c34fdce11eeb4509ddae38201773dd62f284e8aa9d96f85099d0b002202243bd399ff96b649a0fad05fa759d6a882f0af8c90cf7632c2840c29070aec20141045e58067e815c2f464c6a2a15f987758374203895710c2d452442e28496ff38ba8f5fd901dc20e29e88477167fe4fc299bf818fd0d9e1632d467b2a3d9503b1aaffffffff0280d7e636030000001976a914f34c3e10eb387efe872acb614c89e78bfca7815d88ac404b4c00000000001976a914a84e272933aaf87e1715d7786c51dfaeb5b65a6f88ac00000000010000000143ac81c8e6f6ef307dfe17f3d906d999e23e0189fda838c5510d850927e03ae7000000008c4930460221009c87c344760a64cb8ae6685a3eec2c1ac1bed5b88c87de51acd0e124f266c16602210082d07c037359c3a257b5c63ebd90f5a5edf97b2ac1c434b08ca998839f346dd40141040ba7e521fa7946d12edbb1d1e95a15c34bd4398195e86433c92b431cd315f455fe30032ede69cad9d1e1ed6c3c4ec0dbfced53438c625462afb792dcb098544bffffffff0240420f00000000001976a9144676d1b820d63ec272f1900d59d43bc6463d96f888ac40420f00000000001976a914648d04341d00d7968b3405c034adc38d4d8fb9bd88ac00000000010000000248cc917501ea5c55f4a8d2009c0567c40cfe037c2e71af017d0a452ff705e3f1000000008b483045022100bf5fdc86dc5f08a5d5c8e43a8c9d5b1ed8c65562e280007b52b133021acd9acc02205e325d613e555f772802bf413d36ba807892ed1a690a77811d3033b3de226e0a01410429fa713b124484cb2bd7b5557b2c0b9df7b2b1fee61825eadc5ae6c37a9920d38bfccdc7dc3cb0c47d7b173dbc9db8d37db0a33ae487982c59c6f8606e9d1791ffffffff41ed70551dd7e841883ab8f0b16bf04176b7d1480e4f0af9f3d4c3595768d068000000008b4830450221008513ad65187b903aed1102d1d0c47688127658c51106753fed0151ce9c16b80902201432b9ebcb87bd04ceb2de66035fbbaf4bf8b00d1cfe41f1a1f7338f9ad79d210141049d4cf80125bf50be1709f718c07ad15d0fc612b7da1f5570dddc35f2a352f0f27c978b06820edca9ef982c35fda2d255afba340068c5035552368bc7200c1488ffffffff0100093d00000000001976a9148edb68822f1ad580b043c7b3df2e400f8699eb4888ac00000000"), SER_NETWORK, PROTOCOL_VERSION);
    stream >> block;

    CBloomFilter filter(10, 0.000001, 0, BLOOM_UPDATE_ALL);
    // Match the last transaction
    filter.insert(uint256("0x0a2a92f0bda4727d0a13eaddf4dd9ac6b5c61a1429e6b2b818f19b15df0ac154"));

    CMerkleBlock merkleBlock(block, filter);
    BOOST_CHECK(merkleBlock.header.GetHash() == block.GetHash());

    BOOST_CHECK(merkleBlock.vMatchedTxn.size() == 1);
    pair<unsigned int, uint256> pair = merkleBlock.vMatchedTxn[0];

    BOOST_CHECK(merkleBlock.vMatchedTxn[0].second == uint256("0x0a2a92f0bda4727d0a13eaddf4dd9ac6b5c61a1429e6b2b818f19b15df0ac154"));
    BOOST_CHECK(merkleBlock.vMatchedTxn[0].first == 6);

    vector<uint256> vMatched;
    BOOST_CHECK(merkleBlock.txn.ExtractMatches(vMatched) == block.hashMerkleRoot);
    BOOST_CHECK(vMatched.size() == merkleBlock.vMatchedTxn.size());
    for (unsigned int i = 0; i < vMatched.size(); i++)
        BOOST_CHECK(vMatched[i] == merkleBlock.vMatchedTxn[i].second);

    // Also match the 4th transaction
    filter.insert(uint256("0x02981fa052f0481dbc5868f4fc2166035a10f27a03cfd2de67326471df5bc041"));
    merkleBlock = CMerkleBlock(block, filter);
    BOOST_CHECK(merkleBlock.header.GetHash() == block.GetHash());

    BOOST_CHECK(merkleBlock.vMatchedTxn.size() == 2);

    BOOST_CHECK(merkleBlock.vMatchedTxn[0].second == uint256("0x02981fa052f0481dbc5868f4fc2166035a10f27a03cfd2de67326471df5bc041"));
    BOOST_CHECK(merkleBlock.vMatchedTxn[0].first == 3);

    BOOST_CHECK(merkleBlock.vMatchedTxn[1] == pair);

    BOOST_CHECK(merkleBlock.txn.ExtractMatches(vMatched) == block.hashMerkleRoot);
    BOOST_CHECK(vMatched.size() == merkleBlock.vMatchedTxn.size());
    for (unsigned int i = 0; i < vMatched.size(); i++)
        BOOST_CHECK(vMatched[i] == merkleBlock.vMatchedTxn[i].second);
}

BOOST_AUTO_TEST_CASE(merkle_block_4_test_p2pubkey_only)
{
    // Random real block (000000000000b731f2eef9e8c63173adfb07e41bd53eb0ef0a6b720d6cb6dea4)
    // With 7 txes
    CBlock block;
    CDataStream stream(ParseHex("0100000082bb869cf3a793432a66e826e05a6fc37469f8efb7421dc880670100000000007f16c5962e8bd963659c793ce370d95f093bc7e367117b3c30c1f8fdd0d9728776381b4d4c86041b554b85290701000000010000000000000000000000000000000000000000000000000000000000000000ffffffff07044c86041b0136ffffffff0100f2052a01000000434104eaafc2314def4ca98ac970241bcab022b9c1e1f4ea423a20f134c876f2c01ec0f0dd5b2e86e7168cefe0d81113c3807420ce13ad1357231a2252247d97a46a91ac000000000100000001bcad20a6a29827d1424f08989255120bf7f3e9e3cdaaa6bb31b0737fe048724300000000494830450220356e834b046cadc0f8ebb5a8a017b02de59c86305403dad52cd77b55af062ea10221009253cd6c119d4729b77c978e1e2aa19f5ea6e0e52b3f16e32fa608cd5bab753901ffffffff02008d380c010000001976a9142b4b8072ecbba129b6453c63e129e643207249ca88ac0065cd1d000000001976a9141b8dd13b994bcfc787b32aeadf58ccb3615cbd5488ac000000000100000003fdacf9b3eb077412e7a968d2e4f11b9a9dee312d666187ed77ee7d26af16cb0b000000008c493046022100ea1608e70911ca0de5af51ba57ad23b9a51db8d28f82c53563c56a05c20f5a87022100a8bdc8b4a8acc8634c6b420410150775eb7f2474f5615f7fccd65af30f310fbf01410465fdf49e29b06b9a1582287b6279014f834edc317695d125ef623c1cc3aaece245bd69fcad7508666e9c74a49dc9056d5fc14338ef38118dc4afae5fe2c585caffffffff309e1913634ecb50f3c4f83e96e70b2df071b497b8973a3e75429df397b5af83000000004948304502202bdb79c596a9ffc24e96f4386199aba386e9bc7b6071516e2b51dda942b3a1ed022100c53a857e76b724fc14d45311eac5019650d415c3abb5428f3aae16d8e69bec2301ffffffff2089e33491695080c9edc18a428f7d834db5b6d372df13ce2b1b0e0cbcb1e6c10000000049483045022100d4ce67c5896ee251c810ac1ff9ceccd328b497c8f553ab6e08431e7d40bad6b5022033119c0c2b7d792d31f1187779c7bd95aefd93d90a715586d73801d9b47471c601ffffffff0100714460030000001976a914c7b55141d097ea5df7a0ed330cf794376e53ec8d88ac0000000001000000045bf0e214aa4069a3e792ecee1e1bf0c1d397cde8dd08138f4b72a00681743447000000008b48304502200c45de8c4f3e2c1821f2fc878cba97b1e6f8807d94930713aa1c86a67b9bf1e40221008581abfef2e30f957815fc89978423746b2086375ca8ecf359c85c2a5b7c88ad01410462bb73f76ca0994fcb8b4271e6fb7561f5c0f9ca0cf6485261c4a0dc894f4ab844c6cdfb97cd0b60ffb5018ffd6238f4d87270efb1d3ae37079b794a92d7ec95ffffffffd669f7d7958d40fc59d2253d88e0f248e29b599c80bbcec344a83dda5f9aa72c000000008a473044022078124c8beeaa825f9e0b30bff96e564dd859432f2d0cb3b72d3d5d93d38d7e930220691d233b6c0f995be5acb03d70a7f7a65b6bc9bdd426260f38a1346669507a3601410462bb73f76ca0994fcb8b4271e6fb7561f5c0f9ca0cf6485261c4a0dc894f4ab844c6cdfb97cd0b60ffb5018ffd6238f4d87270efb1d3ae37079b794a92d7ec95fffffffff878af0d93f5229a68166cf051fd372bb7a537232946e0a46f53636b4dafdaa4000000008c493046022100c717d1714551663f69c3c5759bdbb3a0fcd3fab023abc0e522fe6440de35d8290221008d9cbe25bffc44af2b18e81c58eb37293fd7fe1c2e7b46fc37ee8c96c50ab1e201410462bb73f76ca0994fcb8b4271e6fb7561f5c0f9ca0cf6485261c4a0dc894f4ab844c6cdfb97cd0b60ffb5018ffd6238f4d87270efb1d3ae37079b794a92d7ec95ffffffff27f2b668859cd7f2f894aa0fd2d9e60963bcd07c88973f425f999b8cbfd7a1e2000000008c493046022100e00847147cbf517bcc2f502f3ddc6d284358d102ed20d47a8aa788a62f0db780022100d17b2d6fa84dcaf1c95d88d7e7c30385aecf415588d749afd3ec81f6022cecd701410462bb73f76ca0994fcb8b4271e6fb7561f5c0f9ca0cf6485261c4a0dc894f4ab844c6cdfb97cd0b60ffb5018ffd6238f4d87270efb1d3ae37079b794a92d7ec95ffffffff0100c817a8040000001976a914b6efd80d99179f4f4ff6f4dd0a007d018c385d2188ac000000000100000001834537b2f1ce8ef9373a258e10545ce5a50b758df616cd4356e0032554ebd3c4000000008b483045022100e68f422dd7c34fdce11eeb4509ddae38201773dd62f284e8aa9d96f85099d0b002202243bd399ff96b649a0fad05fa759d6a882f0af8c90cf7632c2840c29070aec20141045e58067e815c2f464c6a2a15f987758374203895710c2d452442e28496ff38ba8f5fd901dc20e29e88477167fe4fc299bf818fd0d9e1632d467b2a3d9503b1aaffffffff0280d7e636030000001976a914f34c3e10eb387efe872acb614c89e78bfca7815d88ac404b4c00000000001976a914a84e272933aaf87e1715d7786c51dfaeb5b65a6f88ac00000000010000000143ac81c8e6f6ef307dfe17f3d906d999e23e0189fda838c5510d850927e03ae7000000008c4930460221009c87c344760a64cb8ae6685a3eec2c1ac1bed5b88c87de51acd0e124f266c16602210082d07c037359c3a257b5c63ebd90f5a5edf97b2ac1c434b08ca998839f346dd40141040ba7e521fa7946d12edbb1d1e95a15c34bd4398195e86433c92b431cd315f455fe30032ede69cad9d1e1ed6c3c4ec0dbfced53438c625462afb792dcb098544bffffffff0240420f00000000001976a9144676d1b820d63ec272f1900d59d43bc6463d96f888ac40420f00000000001976a914648d04341d00d7968b3405c034adc38d4d8fb9bd88ac00000000010000000248cc917501ea5c55f4a8d2009c0567c40cfe037c2e71af017d0a452ff705e3f1000000008b483045022100bf5fdc86dc5f08a5d5c8e43a8c9d5b1ed8c65562e280007b52b133021acd9acc02205e325d613e555f772802bf413d36ba807892ed1a690a77811d3033b3de226e0a01410429fa713b124484cb2bd7b5557b2c0b9df7b2b1fee61825eadc5ae6c37a9920d38bfccdc7dc3cb0c47d7b173dbc9db8d37db0a33ae487982c59c6f8606e9d1791ffffffff41ed70551dd7e841883ab8f0b16bf04176b7d1480e4f0af9f3d4c3595768d068000000008b4830450221008513ad65187b903aed1102d1d0c47688127658c51106753fed0151ce9c16b80902201432b9ebcb87bd04ceb2de66035fbbaf4bf8b00d1cfe41f1a1f7338f9ad79d210141049d4cf80125bf50be1709f718c07ad15d0fc612b7da1f5570dddc35f2a352f0f27c978b06820edca9ef982c35fda2d255afba340068c5035552368bc7200c1488ffffffff0100093d00000000001976a9148edb68822f1ad580b043c7b3df2e400f8699eb4888ac00000000"), SER_NETWORK, PROTOCOL_VERSION);
    stream >> block;

    CBloomFilter filter(10, 0.000001, 0, BLOOM_UPDATE_P2PUBKEY_ONLY);
    // Match the generation pubkey
    filter.insert(ParseHex("04eaafc2314def4ca98ac970241bcab022b9c1e1f4ea423a20f134c876f2c01ec0f0dd5b2e86e7168cefe0d81113c3807420ce13ad1357231a2252247d97a46a91"));
    // ...and the output address of the 4th transaction
    filter.insert(ParseHex("b6efd80d99179f4f4ff6f4dd0a007d018c385d21"));

    CMerkleBlock merkleBlock(block, filter);
    BOOST_CHECK(merkleBlock.header.GetHash() == block.GetHash());

    // We should match the generation outpoint
    BOOST_CHECK(filter.contains(COutPoint(uint256("0x147caa76786596590baa4e98f5d9f48b86c7765e489f7a6ff3360fe5c674360b"), 0)));
    // ... but not the 4th transaction's output (its not pay-2-pubkey)
    BOOST_CHECK(!filter.contains(COutPoint(uint256("0x02981fa052f0481dbc5868f4fc2166035a10f27a03cfd2de67326471df5bc041"), 0)));
}

BOOST_AUTO_TEST_CASE(merkle_block_4_test_update_none)
{
    // Random real block (000000000000b731f2eef9e8c63173adfb07e41bd53eb0ef0a6b720d6cb6dea4)
    // With 7 txes
    CBlock block;
    CDataStream stream(ParseHex("0100000082bb869cf3a793432a66e826e05a6fc37469f8efb7421dc880670100000000007f16c5962e8bd963659c793ce370d95f093bc7e367117b3c30c1f8fdd0d9728776381b4d4c86041b554b85290701000000010000000000000000000000000000000000000000000000000000000000000000ffffffff07044c86041b0136ffffffff0100f2052a01000000434104eaafc2314def4ca98ac970241bcab022b9c1e1f4ea423a20f134c876f2c01ec0f0dd5b2e86e7168cefe0d81113c3807420ce13ad1357231a2252247d97a46a91ac000000000100000001bcad20a6a29827d1424f08989255120bf7f3e9e3cdaaa6bb31b0737fe048724300000000494830450220356e834b046cadc0f8ebb5a8a017b02de59c86305403dad52cd77b55af062ea10221009253cd6c119d4729b77c978e1e2aa19f5ea6e0e52b3f16e32fa608cd5bab753901ffffffff02008d380c010000001976a9142b4b8072ecbba129b6453c63e129e643207249ca88ac0065cd1d000000001976a9141b8dd13b994bcfc787b32aeadf58ccb3615cbd5488ac000000000100000003fdacf9b3eb077412e7a968d2e4f11b9a9dee312d666187ed77ee7d26af16cb0b000000008c493046022100ea1608e70911ca0de5af51ba57ad23b9a51db8d28f82c53563c56a05c20f5a87022100a8bdc8b4a8acc8634c6b420410150775eb7f2474f5615f7fccd65af30f310fbf01410465fdf49e29b06b9a1582287b6279014f834edc317695d125ef623c1cc3aaece245bd69fcad7508666e9c74a49dc9056d5fc14338ef38118dc4afae5fe2c585caffffffff309e1913634ecb50f3c4f83e96e70b2df071b497b8973a3e75429df397b5af83000000004948304502202bdb79c596a9ffc24e96f4386199aba386e9bc7b6071516e2b51dda942b3a1ed022100c53a857e76b724fc14d45311eac5019650d415c3abb5428f3aae16d8e69bec2301ffffffff2089e33491695080c9edc18a428f7d834db5b6d372df13ce2b1b0e0cbcb1e6c10000000049483045022100d4ce67c5896ee251c810ac1ff9ceccd328b497c8f553ab6e08431e7d40bad6b5022033119c0c2b7d792d31f1187779c7bd95aefd93d90a715586d73801d9b47471c601ffffffff0100714460030000001976a914c7b55141d097ea5df7a0ed330cf794376e53ec8d88ac0000000001000000045bf0e214aa4069a3e792ecee1e1bf0c1d397cde8dd08138f4b72a00681743447000000008b48304502200c45de8c4f3e2c1821f2fc878cba97b1e6f8807d94930713aa1c86a67b9bf1e40221008581abfef2e30f957815fc89978423746b2086375ca8ecf359c85c2a5b7c88ad01410462bb73f76ca0994fcb8b4271e6fb7561f5c0f9ca0cf6485261c4a0dc894f4ab844c6cdfb97cd0b60ffb5018ffd6238f4d87270efb1d3ae37079b794a92d7ec95ffffffffd669f7d7958d40fc59d2253d88e0f248e29b599c80bbcec344a83dda5f9aa72c000000008a473044022078124c8beeaa825f9e0b30bff96e564dd859432f2d0cb3b72d3d5d93d38d7e930220691d233b6c0f995be5acb03d70a7f7a65b6bc9bdd426260f38a1346669507a3601410462bb73f76ca0994fcb8b4271e6fb7561f5c0f9ca0cf6485261c4a0dc894f4ab844c6cdfb97cd0b60ffb5018ffd6238f4d87270efb1d3ae37079b794a92d7ec95fffffffff878af0d93f5229a68166cf051fd372bb7a537232946e0a46f53636b4dafdaa4000000008c493046022100c717d1714551663f69c3c5759bdbb3a0fcd3fab023abc0e522fe6440de35d8290221008d9cbe25bffc44af2b18e81c58eb37293fd7fe1c2e7b46fc37ee8c96c50ab1e201410462bb73f76ca0994fcb8b4271e6fb7561f5c0f9ca0cf6485261c4a0dc894f4ab844c6cdfb97cd0b60ffb5018ffd6238f4d87270efb1d3ae37079b794a92d7ec95ffffffff27f2b668859cd7f2f894aa0fd2d9e60963bcd07c88973f425f999b8cbfd7a1e2000000008c493046022100e00847147cbf517bcc2f502f3ddc6d284358d102ed20d47a8aa788a62f0db780022100d17b2d6fa84dcaf1c95d88d7e7c30385aecf415588d749afd3ec81f6022cecd701410462bb73f76ca0994fcb8b4271e6fb7561f5c0f9ca0cf6485261c4a0dc894f4ab844c6cdfb97cd0b60ffb5018ffd6238f4d87270efb1d3ae37079b794a92d7ec95ffffffff0100c817a8040000001976a914b6efd80d99179f4f4ff6f4dd0a007d018c385d2188ac000000000100000001834537b2f1ce8ef9373a258e10545ce5a50b758df616cd4356e0032554ebd3c4000000008b483045022100e68f422dd7c34fdce11eeb4509ddae38201773dd62f284e8aa9d96f85099d0b002202243bd399ff96b649a0fad05fa759d6a882f0af8c90cf7632c2840c29070aec20141045e58067e815c2f464c6a2a15f987758374203895710c2d452442e28496ff38ba8f5fd901dc20e29e88477167fe4fc299bf818fd0d9e1632d467b2a3d9503b1aaffffffff0280d7e636030000001976a914f34c3e10eb387efe872acb614c89e78bfca7815d88ac404b4c00000000001976a914a84e272933aaf87e1715d7786c51dfaeb5b65a6f88ac00000000010000000143ac81c8e6f6ef307dfe17f3d906d999e23e0189fda838c5510d850927e03ae7000000008c4930460221009c87c344760a64cb8ae6685a3eec2c1ac1bed5b88c87de51acd0e124f266c16602210082d07c037359c3a257b5c63ebd90f5a5edf97b2ac1c434b08ca998839f346dd40141040ba7e521fa7946d12edbb1d1e95a15c34bd4398195e86433c92b431cd315f455fe30032ede69cad9d1e1ed6c3c4ec0dbfced53438c625462afb792dcb098544bffffffff0240420f00000000001976a9144676d1b820d63ec272f1900d59d43bc6463d96f888ac40420f00000000001976a914648d04341d00d7968b3405c034adc38d4d8fb9bd88ac00000000010000000248cc917501ea5c55f4a8d2009c0567c40cfe037c2e71af017d0a452ff705e3f1000000008b483045022100bf5fdc86dc5f08a5d5c8e43a8c9d5b1ed8c65562e280007b52b133021acd9acc02205e325d613e555f772802bf413d36ba807892ed1a690a77811d3033b3de226e0a01410429fa713b124484cb2bd7b5557b2c0b9df7b2b1fee61825eadc5ae6c37a9920d38bfccdc7dc3cb0c47d7b173dbc9db8d37db0a33ae487982c59c6f8606e9d1791ffffffff41ed70551dd7e841883ab8f0b16bf04176b7d1480e4f0af9f3d4c3595768d068000000008b4830450221008513ad65187b903aed1102d1d0c47688127658c51106753fed0151ce9c16b80902201432b9ebcb87bd04ceb2de66035fbbaf4bf8b00d1cfe41f1a1f7338f9ad79d210141049d4cf80125bf50be1709f718c07ad15d0fc612b7da1f5570dddc35f2a352f0f27c978b06820edca9ef982c35fda2d255afba340068c5035552368bc7200c1488ffffffff0100093d00000000001976a9148edb68822f1ad580b043c7b3df2e400f8699eb4888ac00000000"), SER_NETWORK, PROTOCOL_VERSION);
    stream >> block;

    CBloomFilter filter(10, 0.000001, 0, BLOOM_UPDATE_NONE);
    // Match the generation pubkey
    filter.insert(ParseHex("04eaafc2314def4ca98ac970241bcab022b9c1e1f4ea423a20f134c876f2c01ec0f0dd5b2e86e7168cefe0d81113c3807420ce13ad1357231a2252247d97a46a91"));
    // ...and the output address of the 4th transaction
    filter.insert(ParseHex("b6efd80d99179f4f4ff6f4dd0a007d018c385d21"));

    CMerkleBlock merkleBlock(block, filter);
    BOOST_CHECK(merkleBlock.header.GetHash() == block.GetHash());

    // We shouldn't match any outpoints (UPDATE_NONE)
    BOOST_CHECK(!filter.contains(COutPoint(uint256("0x147caa76786596590baa4e98f5d9f48b86c7765e489f7a6ff3360fe5c674360b"), 0)));
    BOOST_CHECK(!filter.contains(COutPoint(uint256("0x02981fa052f0481dbc5868f4fc2166035a10f27a03cfd2de67326471df5bc041"), 0)));
}

BOOST_AUTO_TEST_SUITE_END()
