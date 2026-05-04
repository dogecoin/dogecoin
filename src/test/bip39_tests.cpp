// Copyright (c) 2013-2015 The Bitcoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include <boost/test/unit_test.hpp>

#include "base58.h"
#include "key.h"
#include "uint256.h"
#include "util.h"
#include "utilstrencodings.h"
#include "validation.h"
#include "chain.h"
#include "chainparams.h"
#include "script/standard.h"
#include "script/script.h"
#include "primitives/transaction.h"
#include "consensus/params.h"

#include <string>
#include <vector>
#include <cstring>

#include "wallet/bip39/bip39.h"
#include "wallet/wallet.h"
#include "wallet/test/wallet_test_fixture.h"

struct TestDerivation {
    std::string pub;
    std::string prv;
    unsigned int nChild;
};

struct TestVector {
    std::string strHexMaster;
    std::vector<TestDerivation> vDerive;

    explicit TestVector(std::string strHexMasterIn) : strHexMaster(std::move(strHexMasterIn)) {}

    TestVector& operator()(std::string pub, std::string prv, unsigned int nChild) {
        vDerive.push_back(TestDerivation());
        TestDerivation &der = vDerive.back();
        der.pub = std::move(pub);
        der.prv = std::move(prv);
        der.nChild = nChild;
        return *this;
    }
};

static TestVector test =
  TestVector("FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF")
    ("dgub8kXBZ7ymNWy2Qzp9BQrvBLF1bHweXYvVrR58MHWAiHbiF2wamnFZT78p87FoQGudaiWHnM8e5peNeBpu2PcNRAx9xoKAsZ4K2YTy6eac7kJ",
     "dgpv51eADS3spNJh899z9BEx4UhR8ja1hRtnZpHvCB7BvAYhgyptN1hnoojAYsFq5wnEZG9ZKQ7SDcWG96RC664Dqvayna91soa3XZeQPsU8oic",
     0x80000000U);

static void bip39_test(const TestVector &test)
{
    std::vector<unsigned char> seed = ParseHex(test.strHexMaster);
    CExtKey key;
    CExtPubKey pubkey;
    MNEMONIC mnemonic;
    SEED bip32_seed;
    generateEnglishMnemonic(test.strHexMaster.c_str(), "256", mnemonic);
    seedFromMnemonic(mnemonic, NULL, bip32_seed);

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
        assert(checkKey == key);

        // Test public key
        CBitcoinExtPubKey b58pubkey; b58pubkey.SetKey(pubkey);
        BOOST_CHECK(b58pubkey.ToString() == derive.pub);

        CBitcoinExtPubKey b58PubkeyDecodeCheck(derive.pub);
        CExtPubKey checkPubKey = b58PubkeyDecodeCheck.GetKey();
        assert(checkPubKey == pubkey);

        // Derive new keys
        CExtKey keyNew;
        BOOST_CHECK(key.Derive(keyNew, derive.nChild));
        CExtPubKey pubkeyNew = keyNew.Neuter();
        if (!(derive.nChild & 0x80000000U)) {
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

BOOST_AUTO_TEST_CASE(bip39_test1)
{
    bip39_test(test);
}

BOOST_AUTO_TEST_SUITE_END()

BOOST_FIXTURE_TEST_SUITE(bip39_sweep_tests, WalletTestingSetup)
BOOST_AUTO_TEST_CASE(sweep_test)
{
    CWallet& wallet = *pwalletMain;
    const std::string mnemonic_str =
        "abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon abandon about";
    MNEMONIC mnemonic;
    std::memset(mnemonic, 0, sizeof(mnemonic));
    std::strncpy(mnemonic, mnemonic_str.c_str(), sizeof(mnemonic) - 1);

    SEED seed;
    int ret = seedFromMnemonic(mnemonic, "", seed);
    BOOST_CHECK_EQUAL(ret, 0);

    // Derive first receive address m/44'/3'/0'/0/0
    CExtKey master_key;
    std::vector<unsigned char> vch_seed(seed, seed + 64);
    master_key.SetMaster(&vch_seed[0], vch_seed.size());
    const uint32_t HARD = 0x80000000U;
    CExtKey account_key;
    master_key.Derive(account_key, 44 | HARD);
    account_key.Derive(account_key, 3  | HARD);
    account_key.Derive(account_key, 0  | HARD);
    CExtKey receive_key;
    account_key.Derive(receive_key, 0);
    receive_key.Derive(receive_key, 0);
    CPubKey child_pub = receive_key.key.GetPubKey();
    CTxDestination dest = child_pub.GetID();
    CScript scriptPubKey = GetScriptForDestination(dest);

    // Create synthetic UTXO
    const CAmount nAmount = 50 * COIN;
    CMutableTransaction mtx;
    mtx.vin.clear();
    mtx.vout.push_back(CTxOut(nAmount, scriptPubKey));
    CTransactionRef tx = MakeTransactionRef(std::move(mtx));
    CWalletTx wtx(&wallet, tx);
    {
        LOCK2(cs_main, wallet.cs_wallet);
        wtx.hashBlock = chainActive.Tip()->GetBlockHash();
        wtx.nIndex = 0;
        wtx.nTimeReceived = GetTime();
        wtx.fFromMe = true;
        wallet.AddToWallet(wtx, true);
        wallet.MarkDirty();
    }

    // Sweep
    std::string extra = "";
    std::string path = "m/44'/3'/0'";
    int gap = 1;
    const CTxDestination* dest_ptr = nullptr;
    CBlockIndex* startAt = nullptr;
    bool dry_run = false;
    const CFeeRate* fee_rate = nullptr;
    SweepReport rep;
    std::string err;
    bool success;
    {
        LOCK(wallet.cs_wallet);
        success = wallet.SweepFromMnemonic(mnemonic, extra, path, gap,
                                           dest_ptr, startAt, dry_run,
                                           fee_rate, rep, err);
    }
    BOOST_CHECK_MESSAGE(success, err);
    BOOST_CHECK_EQUAL(rep.inputs, 1);
    BOOST_CHECK_EQUAL(rep.total, nAmount);
    BOOST_CHECK(!rep.txid.empty());
}
BOOST_AUTO_TEST_SUITE_END()
