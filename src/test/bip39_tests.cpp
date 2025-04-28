// Copyright (c) 2013-2015 The Bitcoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include <boost/test/unit_test.hpp>

#include "base58.h"
#include "key.h"
#include "uint256.h"
#include "util.h"
#include "utilstrencodings.h"
#include "test/test_bitcoin.h"

#include <string>
#include <vector>

extern "C" {
#include "dogecoin/libdogecoin.h"
}

struct TestDerivation {
    std::string pub;
    std::string prv;
    unsigned int nChild;
};

struct TestVector {
    std::string strHexMaster;
    std::vector<TestDerivation> vDerive;

    TestVector(std::string strHexMasterIn) : strHexMaster(strHexMasterIn) {}

    TestVector& operator()(std::string pub, std::string prv, unsigned int nChild) {
        vDerive.push_back(TestDerivation());
        TestDerivation &der = vDerive.back();
        der.pub = pub;
        der.prv = prv;
        der.nChild = nChild;
        return *this;
    }
};

TestVector test =
  TestVector("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")
    ("dgub8kXBZ7ymNWy2Qzp9BQrvBLF1bHweXYvVrR58MHWAiHbiF2wamnFZT78p87FoQGudaiWHnM8e5peNeBpu2PcNRAx9xoKAsZ4K2YTy6eac7kJ",
     "dgpv51eADS3spNJh899z9BEx4UhR8ja1hRtnZpHvCB7BvAYhgyptN1hnoojAYsFq5wnEZG9ZKQ7SDcWG96RC664Dqvayna91soa3XZeQPsU8oic",
     0x80000000);

void bip39_test(const TestVector &test) {
    std::vector<unsigned char> seed = ParseHex(test.strHexMaster);
    CExtKey key;
    CExtPubKey pubkey;
    MNEMONIC mnemonic;
    SEED bip32_seed;
    generateEnglishMnemonic(test.strHexMaster.c_str(), "256", mnemonic);
    dogecoin_seed_from_mnemonic (mnemonic, NULL, bip32_seed);

    // Copy the bip32_seed to the seed vector
    seed.resize(sizeof(bip32_seed));
    memcpy(&seed[0], bip32_seed, sizeof(bip32_seed));
    key.SetMaster(&seed[0], seed.size());

    pubkey = key.Neuter();
    BOOST_FOREACH(const TestDerivation &derive, test.vDerive) {
        unsigned char data[74];
        key.Encode(data);
        pubkey.Encode(data);

        // Test private key
        CBitcoinExtKey b58key; b58key.SetKey(key);
        BOOST_CHECK(b58key.ToString() == derive.prv);

        CBitcoinExtKey b58keyDecodeCheck(derive.prv);
        CExtKey checkKey = b58keyDecodeCheck.GetKey();
        assert(checkKey == key); //ensure a base58 decoded key also matches

        // Test public key
        CBitcoinExtPubKey b58pubkey; b58pubkey.SetKey(pubkey);
        BOOST_CHECK(b58pubkey.ToString() == derive.pub);

        CBitcoinExtPubKey b58PubkeyDecodeCheck(derive.pub);
        CExtPubKey checkPubKey = b58PubkeyDecodeCheck.GetKey();
        assert(checkPubKey == pubkey); //ensure a base58 decoded pubkey also matches

        // Derive new keys
        CExtKey keyNew;
        BOOST_CHECK(key.Derive(keyNew, derive.nChild));
        CExtPubKey pubkeyNew = keyNew.Neuter();
        if (!(derive.nChild & 0x80000000)) {
            // Compare with public derivation
            CExtPubKey pubkeyNew2;
            BOOST_CHECK(pubkey.Derive(pubkeyNew2, derive.nChild));
            BOOST_CHECK(pubkeyNew == pubkeyNew2);
        }
        key = keyNew;
        pubkey = pubkeyNew;

        CDataStream ssPub(SER_DISK, CLIENT_VERSION);
        ssPub << pubkeyNew;
        BOOST_CHECK(ssPub.size() == 75);

        CDataStream ssPriv(SER_DISK, CLIENT_VERSION);
        ssPriv << keyNew;
        BOOST_CHECK(ssPriv.size() == 75);

        CExtPubKey pubCheck;
        CExtKey privCheck;
        ssPub >> pubCheck;
        ssPriv >> privCheck;

        BOOST_CHECK(pubCheck == pubkeyNew);
        BOOST_CHECK(privCheck == keyNew);
    }
}

BOOST_FIXTURE_TEST_SUITE(bip39_tests, BasicTestingSetup)

BOOST_AUTO_TEST_CASE(bip39_test1) {
    bip39_test(test);
}

BOOST_AUTO_TEST_SUITE_END()
