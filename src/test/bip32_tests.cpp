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

TestVector test1 =
  TestVector("000102030405060708090a0b0c0d0e0f")
    ("dgub8kXBZ7ymNWy2S8Q3jNgVjFUm5ZJ3QLLaSTdAA89ukSv7Q6MSXwE14b7Nv6eDpE9JJXinTKc8LeLVu19uDPrm5uJuhpKNzV2kAgncwo6bNpP",
     "dgpv51eADS3spNJh9Gjth94XcPwAczvQaDJs9rqx11kvxKs6r3Ek8AgERHhjLs6mzXQFHRzQqGwqdeoDkZmr8jQMBfi43b7sT3sx3cCSk5fGeUR",
     0x80000000)
    ("dgub8nnbYqHETn61ajXkw8Z8cHasQNrPnQpb85448DY2ie7PmNecxAm6BjTnhNCvZY3qJk1MKZ9Z5HQasQ83ARb99nmduT7dunvxgcvBFVHuvrq",
     "dgpv53uaD9MLudRgHssbttwAVS3GwpUkxHnsqUGqy793vX4PDKXvYQDKYS4988T7QEnCzUt7CaGi21e6UKoZnKgXyjna7To1h1aqkcqJBDM65ur",
     1)
    ("dgub8pxikcq7rUy5RBaCfPT1D2UXTkqVnSYt4PitiVJqfGubzv9kfyBQ9JN27SfVyUmBGTdQ6ybfBsu4Thrrdkm2qSbaCexVPRwEKMSxYLP2A41",
     "dgpv565hQvuEJLJk8Kv3d9q36Avw1CTrxKXAmnwgZNurs9rbSs34GCddVzxNYBeB1AZFSZdo1Ps96ibWcGKnufUWkuH1dEkjkmMhRR9fi7Po6B2",
     0x80000002)
    ("dgub8sZzo9eyZMpVHMNHuyrNa2Wfgui23z8sPvxZxpbzq9H3QmLsUj1q3juwfTrLRMCVcyj8iMaGZpU2v319LrJZttkQnYvdUNzv33N6dcqeZ8X",
     "dgpv58gyTTj61DA9zVi8skEQTAy5EMLPDs7A7LBMoiD232E2riEB4xU4QSWJ6DrnyQ4jx2fBbrp4X8RQqU4YVgPhszifyrKHuhbe2gttLnRB4a6",
     2)
    ("dgub8uoPdamvjqVUMpr1cF4TTXfymizkgaT4qQqsDn8U9aqemryEYViCFKNsLnqiq9ME6HrJrN4DcZN9UTM9S9jmcVDfhLUpJZtk3jGwnGkhd8u",
     "dgpv5AvNHtr3Bgq94yBra1SVLg8PKAd7rTRMYp4f4fjVMTneDorY8jARc1yDmYGFS4UB1pntDn3dRwsaJexzh6w45PJiP6QPTnRMBfN3rDUiyyH",
     1000000000)
    ("dgub8wXA7GPArxsftAdTindBmEfyZxa4W5G6dfERU4WcMfE9UzNd4uxrWRXvyckfgQRwZz8rMhz29m4k4skAY1EcTkNnZstu73UNrgts2MA5evC",
     "dgpv5Ce8maTHJpDLbJyJgZ1DeP8P7QCRfxEPM4TDJx7dZYB8vwFvf9R5s88HQQ3TLybFdEC9192aGzQhJpyNEAwnCLxFibAcahB4TzvQbJyp2im",
     0);

TestVector test2 =
  TestVector("fffcf9f6f3f0edeae7e4e1dedbd8d5d2cfccc9c6c3c0bdbab7b4b1aeaba8a5a29f9c999693908d8a8784817e7b7875726f6c696663605d5a5754514e4b484542")
    ("dgub8kXBZ7ymNWy2RjuNqXknBTCXKkSU5xbQ83QtT4tjiq2yh5Ndi5zwVVGyGCjCXUWGD5xaMzGHjiqkcnt8LamvDpJrZkWqpyXQV4TjDhfyo9Q",
     "dgpv51eADS3spNJh8tFDoJ8p4bevsC4qFqZgqSdgHxVkvhyy92FwJKTArBsKgvsqB2xLXUjqaZQHukqQr6VxB9o3o32pW1C7bPngcrpg75LUw8V",
     0)
    ("dgub8onvpqfirXo6x1VfyK8fFFc3giBinw5ggDAFcsvBoEtwP3pcHMM1eKrDqfh6KZWhRQSkEDG38ogimxJpDjULZQy8qoFWjKfncYaPesrSURc",
     "dgpv54uuV9jqJP8mf9qWw5Wh8Q4TE9p5xp3yPcP3TmXD17qvpzhusaoF12SaGS9dp6oAw8yfUZp2LvFYCc8mjSJ6jGCDWBcAysxRkGjEUK7pYvw",
     0xFFFFFFFF)
    ("dgub8pwz5ShFERyD7shrPm8JibHc5TQdLRFNmNEnYGpxSyfKqM44uEmKrPdpT3wD5J7oCvNHt47eS27KSdB9zdxTHZRmNssa63voUvqzVgkMK7p",
     "dgpv564xjkmMgHJsq23hMXWLbjk1cu2zWJDfUmTaPARyercKHHwNVUDZD6EAsndcYMXeqNJZFb1fPvkedqsYTouEJZdmvuqWkPggY44mEn4uizf",
     1)
    ("dgub8skxVTgBQ5GQDVNzTRGsgYDqQzH8ScAe5ojePLVHks1mWAvECkJ2kJ2CHr8LsAp5o6pqihCt59R9XRSAuYPQYttfyA5RJbN1QhWwkCcvPdA",
     "dgpv58sw9mkHqvc4vdiqRBeuZggExRuVcV8voCxSEE6Jxjxkx7oXnykG6zcYiaqEa4jM9KfFzt63oURrxYehWhRcK3T54gNKbVf51rVViRSkahZ",
     0xFFFFFFFE)
    ("dgub8tvzQRcY1UE7WScbBu6R43v2KikVVao97WTXv4BhbdH1xXFAJRE3GpPHvWFr4YLXkYUUXCGb7kk1B4bZbRVvGFb8F4PurtTRGvbXH6bMPN3",
     "dgpv5A3y4jgeTKZnDaxS9fUSwCNRsANrfTmRpugKkwnioWE1QU8TtegGdWyeMHZdCV7dgtwxJhs3Br1Smfk52eL6zt8EtcudjhghMSW1nDNfmHP",
     2)
    ("dgub8vJ2Mrq3XeYMFhoUuyE72oHzinHbjuiDmg5RKqYCER8c2iajU49oTPLHzuL8C7hEGbgho7n11TkzfVf4RXBpaWShtEDzoFk9xDnhwhVSxT7",
     "dgpv5BR12Au9yVt1xr9Ksjc8uwkQGDuxungWV5JDAj9DSJ5bUfU34Hc2p5veRhEdMWChjCogbTVrdwr8pDdakxhL3rrxhUR8o7pR3oqZrnPNxDt",
     0);

void RunTest(const TestVector &test) {
    std::vector<unsigned char> seed = ParseHex(test.strHexMaster);
    CExtKey key;
    CExtPubKey pubkey;
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

BOOST_FIXTURE_TEST_SUITE(bip32_tests, BasicTestingSetup)

BOOST_AUTO_TEST_CASE(bip32_test1) {
    RunTest(test1);
}

BOOST_AUTO_TEST_CASE(bip32_test2) {
    RunTest(test2);
}

BOOST_AUTO_TEST_SUITE_END()
