// Copyright (c) 2010 Satoshi Nakamoto
// Copyright (c) 2009-2014 The Bitcoin developers
// Copyright (c) 2013-2014 The Dogecoin developers
// Copyright (c)      2014 The Inutoshi developers
// Distributed under the MIT/X11 software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "chainparams.h"

#include "assert.h"
#include "core.h"
#include "protocol.h"
#include "util.h"

#include <boost/assign/list_of.hpp>

using namespace boost::assign;

//
// Main network
//

unsigned int pnSeed[] =
{
    0x14851955, 0xc7113b25, 0xa6db048e, 0x77606505, 0x4f163b25, 0x567c692e, 0x25e02a64, 0x10143b25,
    0x93123b25, 0xa463f8a2, 0xef218abc, 0x25133b25, 0xe189bb25, 0xc0f83eb2, 0x6ab7c936, 0xa3624c90,
    0x6af8c4d9, 0x38c55b80, 0xb21f09b0, 0x33e7a5bc, 0xf558fb94, 0x6a5c4c90, 0x20d0048e, 0x14d1048e,
    0xcabd2e4e, 0x26bfd35f, 0x065e8abc, 0xccce1c2e, 0x2a55ec6d, 0xcd01f5da, 0x0bef32c6, 0x991d5fc0,
    0x6f4d4550, 0x0f35831f, 0xc7183b25, 0x8c7b0905, 0x7a9cbb25, 0x7eafa951, 0x86579bc0, 0xa15a4450,
    0xe5425177, 0x876e8ad5, 0xf6946257, 0x40c5ffad, 0x2e209d53, 0x6262e773, 0x42ef4c90, 0x9fc351c5,
    0x45ac795b, 0x6ac66d4f, 0x0a588abc, 0x9d411787, 0x8f9bbb25, 0xc4288368, 0xf745535b, 0x9c7086cf,
    0xc4422e4e, 0xc2533532, 0x84945250, 0x8b5d715e, 0x0f331c73, 0x228a9bd8, 0x4f3f0849, 0x17f4bbd5,
    0xc0e64a4a, 0xc53ebb25, 0x8dfab7ad, 0x36097e40, 0x2a27966b, 0x307d8abc, 0x1a32c405, 0xcf8d3c41,
    0x809f8705, 0x21f232c6, 0x64693660, 0x83b763c0, 0xc613ec68, 0xaf517462, 0x8f61e497, 0xae7b0451,
    0xafd61955, 0x2b85d772, 0xde0cf062, 0xee4ec780, 0xef6aa56f, 0x09b85d61, 0xac44f6cc, 0xc0b73505,
    0x0dedb944, 0x554d6f4c, 0x627dfe42, 0x48ca34ae, 0x272371d4, 0xf252a5bc, 0x8740f718, 0x262371d4,
    0x3ce82760, 0xe17c8845, 0x7c17332a, 0x4f5abb25, 0x07eccf57, 0x183f6018, 0xcb4b5f5c, 0xc34a0d47,
    0x60a4c780, 0xe9c2784c, 0xbccac24d, 0xc143f03c, 0x0260ba7c, 0x4a81ae58, 0xe131b843, 0x1dae7fd5,
    0x75d041ad, 0x390c7ed0, 0xaeac6d62, 0x8de20e4c, 0x915ee097, 0x1a3e1049, 0x8aca6f18, 0xd4470444,
    0x4856b3ad, 0x387e211b, 0xd191feb6, 0xe3bd0d41, 0x130009b0, 0xdf18be43, 0xdf143b25, 0x1e93e66d,
    0x41cfa262, 0x859baa51, 0x3618aa6b, 0x74c5f1c0, 0x3c14226c, 0x5c3cd070, 0xbc671b4f, 0x3f5871d5,
    0x37011e02, 0xfe53186c, 0xd39bd176, 0x3822b718, 0xe27d3060, 0xabee32c6, 0xc697c818, 0xd22cde50,
    0xc4cc6c3a, 0xd24ccd43, 0xd450bb25, 0x4101ee43, 0x20f232c6, 0xb7997845, 0xd4e3c162, 0x2fbe7670,
    0x41a5ee52, 0x800a201b, 0x4699ccda, 0xfcabb552, 0xde53ad32, 0x61668c18, 0xca9ec24e, 0x8b1eac49,
    0xb990b142, 0xf1e1784d, 0x30d04147, 0x785a995b, 0x12d14acc, 0x39d43680, 0x65e70844, 0x54783b18,
    0x232a3a63, 0x4e0a194f, 0x2b813b18, 0x2ac0bc49, 0x6af1ae51, 0x3256d062, 0x13d72bd9, 0xa5496444,
    0xbc944c47, 0x7d35bdd5, 0x109f2446, 0x16ef2e25, 0xe2f2784b, 0xee5ddaac, 0xc7889718, 0xaf49be6d,
    0xc914a443, 0x1d2bc24e, 0x82199d5d, 0x689c226c, 0x04441b46, 0x1feca832, 0x94208c55, 0x6756a149,
    0x09dee8d8, 0x1435e9a9, 0x82859f32, 0xadf0a662, 0x2f2bb856, 0x756bcd5a, 0x6b6be917, 0x1c40604a,
    0xeec4b749, 0x4d44a6b8, 0x162d1cb0, 0x22f232c6, 0x5f2012ad, 0x35612053, 0x02b35e47, 0x2d5c3f44,
    0x6904ef17, 0xef8a0b4c, 0xe6908932, 0x0df86c76, 0x3669343c, 0xdd8ba87c, 0x35841386, 0xb406e044,
    0x14a6946d, 0x6a448e3f, 0x2341295e, 0xb2837325, 0x82cf0c05, 0xa64d4141, 0xeb24c672, 0x4282ffda,
    0xccf2336c, 0x64813fbc, 0x9c81d259, 0xfbe6ed60, 0xc24a1c6c, 0x470ddc48, 0xd4dcaf42, 0x4c373fb2,
    0xbb9589d9, 0x7e431bc6, 0x3d24be43, 0xf417af44, 0x83f05243, 0xbe022363, 0x10b71d5d, 0xaf4b88d5,
    0xc3b66644, 0x31d30b71, 0xa056aa6b, 0x48db0364, 0x9780f5ac, 0x763aa0ad, 0x8ebcfe40, 0x241847ad,
    0xacdbf562, 0xfd09f743, 0x4f211c60, 0x0d963468, 0x80db149a, 0x18e8b944, 0xbcc27746, 0x8f6ce95f,
    0xde2cee2e, 0x45a71457, 0xde894ead, 0x024cd348, 0x2e0e74ce, 0x23300cac, 0x90527f62, 0x1d66e56d,
    0x83eddb8d, 0x0850f555, 0xe9a4e797, 0x0ef1ab56, 0x140ed98e, 0x2d3f944d, 0x0b6d18bc, 0xab33ca49,
    0xaed3b962, 0xc8a0f155, 0xb3eb9c32, 0xfc06424b, 0x927af8ac, 0xdf0119bc, 0xf7ba2452, 0x26590c05,
    0x0ec0cb6d, 0x337b252d, 0xe7b04a97, 0x5cc4a505, 0x9e81884a, 0xd052534b, 0xf7aa7761, 0x6666b527,
    0x49a4fd62, 0x3f715247, 0x902baa46, 0x7645812e, 0xbf771240, 0xe58e2451, 0xbb95fca2, 0x35204718,
    0x759138ae, 0x84300edf, 0x67140d47, 0x2c34465f, 0x260d0f18, 0xfd985c32, 0x3ba94cb2, 0x8067b545,
    0x062ac825, 0xdee9de59, 0x6254bf32, 0x9b6d2b44, 0xbebd7670, 0x3c55b46c, 0x5e8e5561, 0xa6d9a656,
    0x631fe149, 0x102f0378, 0xf4ede844, 0x34b94b3e, 0x73cdd55a, 0xd114c850, 0x7b6c5343, 0x6a3dde47,
    0x385da2b8, 0xf5f8c3d2, 0xe44c0654, 0xa4cb42bb, 0xf53fd176, 0xa424a14b, 0x79b38856, 0xdb717b5d,
    0x97f0b8bf, 0x031c4627, 0x9731d35a, 0x7bd81e4c, 0x43a850bc, 0xa336ff71, 0xbabd0b59, 0x9767bc73,
    0x1c131d4c, 0x06a47744, 0x1f5dbe5e, 0xf76f18bc, 0x560ab918, 0xd7cb0202, 0x569b326c, 0x52024c72,
    0x47525a5f, 0xd36d475f, 0x2e7d5dcc, 0x60330202, 0xd13c026c, 0x1660d7b0, 0x1a042f4e, 0xe644005e,
    0x5c53ba41, 0xc5adf651, 0x84e15d62, 0xcb76364d, 0x78ec8573, 0x5f6a06ae, 0xac8d3347, 0xd8758fd5,
    0x91c5351f, 0xdd6d67b1, 0x6027e53c, 0x308bba6d, 0x15448945, 0x612d454b, 0x3a10a143, 0x9b38cf56,
    0x3c44c556, 0xffb8784c, 0x5409b04b, 0xab2f7661, 0xa286534b, 0x0476c944, 0xd9cf4a52, 0x9cfa276c,
    0xaf07936b, 0xfcd02e32, 0x2361db8d, 0xc62c878d, 0x560ae740, 0xb21d0818, 0x7a2c4605, 0x8cc6a73b,
    0x5757be56, 0x50f1e218, 0x13b9194f, 0x48eb17ad, 0x4279112e, 0xf92a3fbc, 0x8ecc0202, 0xe93237ae,
    0x9bcfc80e, 0x4d72a343, 0xb97eed8d, 0x6732b55e, 0x14fdab43, 0x05e502d4, 0x88bd436d, 0xf4985a86,
    0xa12f8156, 0x2a91052e, 0xa651d98e, 0x2f750947, 0x61fd5455, 0x8bbcb66e, 0xd7383a60, 0xd9d3945c,
    0x52e5ac2e, 0x5f7a49be, 0x59036a44, 0x4b7b1978, 0x3a88ca4e, 0x910953b2, 0xc744996d, 0x0b6277c7,
    0x02a1a6b8, 0x0234d9ad, 0x0e7b8f45, 0x48738653, 0x3a1a5447, 0x0fdc4950, 0x672d5847, 0x04e44448,
    0x7750945c, 0x602f3260, 0xf5b42201, 0x5c05922e, 0x203c5332, 0x1f7c484b, 0x1b2d8caf, 0xbc99e797,
    0xe695ed62, 0x11b38375, 0x55b84bb2, 0xfce349be, 0x7181456a, 0x4f51f643, 0x66bb7e52, 0x5d235db2,
    0xb8c52ab2, 0x111b90d9, 0xa80eb118
};

class CMainParams : public CChainParams {
public:
    CMainParams() {
        // The message start string is designed to be unlikely to occur in normal data.
        // The characters are rarely used upper ASCII, not valid as UTF-8, and produce
        // a large 4-byte int at any alignment.
        pchMessageStart[0] = 0xc0;
        pchMessageStart[1] = 0xc0;
        pchMessageStart[2] = 0xc0;
        pchMessageStart[3] = 0xc0;
        vAlertPubKey = ParseHex("04d4da7a5dae4db797d9b0644d57a5cd50e05a70f36091cd62e2fc41c98ded06340be5a43a35e185690cd9cde5d72da8f6d065b499b06f51dcfba14aad859f443a");
        nDefaultPort = 22556;
        nRPCPort = 22555;
        bnProofOfWorkLimit = CBigNum(~uint256(0) >> 20);
        nSubsidyHalvingInterval = 210000;

        // Build the genesis block. Note that the output of the genesis coinbase cannot
        // be spent as it did not originally exist in the database.
        //
        // CBlock(hash=000000000019d6, ver=1, hashPrevBlock=00000000000000, hashMerkleRoot=4a5e1e, nTime=1231006505, nBits=1d00ffff, nNonce=2083236893, vtx=1)
        //   CTransaction(hash=4a5e1e, ver=1, vin.size=1, vout.size=1, nLockTime=0)
        //     CTxIn(COutPoint(000000, -1), coinbase 04ffff001d0104455468652054696d65732030332f4a616e2f32303039204368616e63656c6c6f72206f6e206272696e6b206f66207365636f6e64206261696c6f757420666f722062616e6b73)
        //     CTxOut(nValue=50.00000000, scriptPubKey=0x5F1DF16B2B704C8A578D0B)
        //   vMerkleTree: 4a5e1e
        const char* pszTimestamp = "Nintondo";
        CTransaction txNew;
        txNew.vin.resize(1);
        txNew.vout.resize(1);
        txNew.vin[0].scriptSig = CScript() << 486604799 << CScriptNum(4) << vector<unsigned char>((const unsigned char*)pszTimestamp, (const unsigned char*)pszTimestamp + strlen(pszTimestamp));
        txNew.vout[0].nValue = 88 * COIN;
        txNew.vout[0].scriptPubKey = CScript() << ParseHex("040184710fa689ad5023690c80f3a49c8f13f8d45b8c857fbcbc8bc4a8e4d3eb4b10f4d4604fa08dce601aaf0f470216fe1b51850b4acf21b179c45070ac7b03a9") << OP_CHECKSIG;
        genesis.vtx.push_back(txNew);
        genesis.hashPrevBlock = 0;
        genesis.hashMerkleRoot = genesis.BuildMerkleTree();
        genesis.nVersion = 1;
        genesis.nTime    = 1386325540;
        genesis.nBits    = 0x1e0ffff0;
        genesis.nNonce   = 99943;

        hashGenesisBlock = genesis.GetHash();
        assert(hashGenesisBlock == uint256("0x1a91e3dace36e2be3bf030a65679fe821aa1d6ef92e7c9902eb318182c355691"));
        assert(genesis.hashMerkleRoot == uint256("0x5b2a3f53f605d62c53e62932dac6925e3d74afa5a4b459745c36d42d0ed26a69"));

        vSeeds.push_back(CDNSSeedData("dogecoin.com", "seed.dogecoin.com"));
        vSeeds.push_back(CDNSSeedData("multidoge.org", "seed.multidoge.org"));
        vSeeds.push_back(CDNSSeedData("multidoge.org", "seed2.multidoge.org"));
        vSeeds.push_back(CDNSSeedData("doger.dogecoin.com", "seed.doger.dogecoin.com"));

        // Workaround for Boost not being quite compatible with C++11;
        std::vector<unsigned char> pka = list_of(30);
        base58Prefixes[PUBKEY_ADDRESS] = pka;
        
        std::vector<unsigned char> sca = list_of(22);
        base58Prefixes[SCRIPT_ADDRESS] = sca;
        
        std::vector<unsigned char> sk  = list_of(158);
        base58Prefixes[SECRET_KEY]     = sk;
        
        std::vector<unsigned char> epk = list_of(0x04)(0x88)(0xC4)(0x2E);
        base58Prefixes[EXT_PUBLIC_KEY] = epk;
        
        std::vector<unsigned char> esk = list_of(0x04)(0x88)(0xE1)(0xF4);
        base58Prefixes[EXT_SECRET_KEY] = esk;

        // Convert the pnSeeds array into usable address objects.
        for (unsigned int i = 0; i < ARRAYLEN(pnSeed); i++)
        {
            // It'll only connect to one or two seed nodes because once it connects,
            // it'll get a pile of addresses with newer timestamps.
            // Seed nodes are given a random 'last seen time' of between one and two
            // weeks ago.
            const uint64_t nOneWeek = 7*24*60*60;
            struct in_addr ip;
            memcpy(&ip, &pnSeed[i], sizeof(ip));
            CAddress addr(CService(ip, GetDefaultPort()));
            addr.nTime = GetTime() - GetRand(nOneWeek) - nOneWeek;
            vFixedSeeds.push_back(addr);
        }
    }

    virtual const CBlock& GenesisBlock() const { return genesis; }
    virtual Network NetworkID() const { return CChainParams::MAIN; }

    virtual const vector<CAddress>& FixedSeeds() const {
        return vFixedSeeds;
    }
protected:
    CBlock genesis;
    vector<CAddress> vFixedSeeds;
};
static CMainParams mainParams;


//
// Testnet (v3)
//
class CTestNetParams : public CMainParams {
public:
    CTestNetParams() {
        // The message start string is designed to be unlikely to occur in normal data.
        // The characters are rarely used upper ASCII, not valid as UTF-8, and produce
        // a large 4-byte int at any alignment.
        pchMessageStart[0] = 0xfc;
        pchMessageStart[1] = 0xc1;
        pchMessageStart[2] = 0xb7;
        pchMessageStart[3] = 0xdc;
        vAlertPubKey = ParseHex("042756726da3c7ef515d89212ee1705023d14be389e25fe15611585661b9a20021908b2b80a3c7200a0139dd2b26946606aab0eef9aa7689a6dc2c7eee237fa834");
        nDefaultPort = 44556;
        nRPCPort = 44555;
        strDataDir = "testnet3";

        // Modify the testnet genesis block so the timestamp is valid for a later start.
        genesis.nTime = 1391503289;
        genesis.nNonce = 997879;
        hashGenesisBlock = genesis.GetHash();
        assert(hashGenesisBlock == uint256("0xbb0a78264637406b6360aad926284d544d7049f45189db5664f3c4d07350559e"));

        vFixedSeeds.clear();
        vSeeds.clear();
        vSeeds.push_back(CDNSSeedData("testdoge.lionservers.de", "testdoge-seed.lionservers.de"));
        vSeeds.push_back(CDNSSeedData("lionservers.de", "testdoge-seed-static.lionservers.de"));

        // Boost sucks, and should not be used. Workaround for Boost not being compatible with C++11;
        
        std::vector<unsigned char> pka = list_of(113);
        base58Prefixes[PUBKEY_ADDRESS] = pka;
        std::vector<unsigned char> sca = list_of(196);
        base58Prefixes[SCRIPT_ADDRESS] = sca;
        std::vector<unsigned char> sk  = list_of(241);
        base58Prefixes[SECRET_KEY]     = sk;
        std::vector<unsigned char> epk = list_of(0x04)(0x35)(0xD1)(0xDF);
        base58Prefixes[EXT_PUBLIC_KEY] = epk;
        std::vector<unsigned char> esk = list_of(0x04)(0x35)(0x75)(0xA4);
        base58Prefixes[EXT_SECRET_KEY] = esk;
    }
    virtual Network NetworkID() const { return CChainParams::TESTNET; }
};
static CTestNetParams testNetParams;

//
// Regression test
//
class CRegTestParams : public CTestNetParams {
public:
    CRegTestParams() {
        pchMessageStart[0] = 0xfa;
        pchMessageStart[1] = 0xbf;
        pchMessageStart[2] = 0xb5;
        pchMessageStart[3] = 0xda;
        nSubsidyHalvingInterval = 150;
        bnProofOfWorkLimit = CBigNum(~uint256(0) >> 1);
        genesis.nTime = 1296688602;
        genesis.nBits = 0x207fffff;
        genesis.nNonce = 2;
        hashGenesisBlock = genesis.GetHash();
        nDefaultPort = 18444;
        strDataDir = "regtest";
        assert(hashGenesisBlock == uint256("0x3d2160a3b5dc4a9d62e7e66a295f70313ac808440ef7400d6c0772171ce973a5"));

        vSeeds.clear();  // Regtest mode doesn't have any DNS seeds.
    }

    virtual bool RequireRPCPassword() const { return false; }
    virtual bool SimplifiedRewards() const { return true; }
    virtual Network NetworkID() const { return CChainParams::REGTEST; }
};
static CRegTestParams regTestParams;

static CChainParams *pCurrentParams = &mainParams;

const CChainParams &Params() {
    return *pCurrentParams;
}

void SelectParams(CChainParams::Network network) {
    switch (network) {
        case CChainParams::MAIN:
            pCurrentParams = &mainParams;
            break;
        case CChainParams::TESTNET:
            pCurrentParams = &testNetParams;
            break;
        case CChainParams::REGTEST:
            pCurrentParams = &regTestParams;
            break;
        default:
            assert(false && "Unimplemented network");
            return;
    }
}

bool SelectParamsFromCommandLine() {
    bool fRegTest = GetBoolArg("-regtest", false);
    bool fTestNet = GetBoolArg("-testnet", false);

    if (fTestNet && fRegTest) {
        return false;
    }

    if (fRegTest) {
        SelectParams(CChainParams::REGTEST);
    } else if (fTestNet) {
        SelectParams(CChainParams::TESTNET);
    } else {
        SelectParams(CChainParams::MAIN);
    }
    return true;
}
