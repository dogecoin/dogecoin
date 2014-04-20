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
    0xdc7fa218, 0xb4b68d18, 0xd286166c, 0xb0bbb052, 0x5d9fe2bc, 0x0256c0ad, 0x4297d23e, 0x2cf5226c,
    0xa80dfb94, 0xf1a134d0, 0x4215042e, 0x7c51486d, 0x3a51486d, 0xd5c63832, 0xde5b7e02, 0xdb176018,
    0x332745ad, 0x7172c7c6, 0xfb90b742, 0x134dd9a2, 0x25e238ae, 0xc332ed40, 0x24a0a4c6, 0xcd5e0905,
    0xcaca87dc, 0xe28ac836, 0xc79c2942, 0x672baa6b, 0xa2012363, 0xf848f160, 0x34747b6d, 0xe6f7f1c0,
    0xbbc51955, 0x4e106d5b, 0x69e47046, 0x4ca0d54a, 0x6d0a65c6, 0x441d9c32, 0x436d5d81, 0xbdfffbce,
    0x1cd2f2a2, 0x112daa6b, 0x30b85f47, 0x02100ab9, 0x2e044bad, 0x2071ff45, 0x5f98b54b, 0x55a40905,
    0x651aaa6b, 0xe99ef2c5, 0x17c35ad0, 0x8db55d41, 0x587921cb, 0x3865ea2e, 0xba0d63c0, 0x8eb88b52,
    0x7a80a7c7, 0x4d5c5747, 0x3fd4f3a2, 0xf825be43, 0x161eaa6b, 0x43eda52e, 0x03b422b0, 0x5c0e63c0,
    0x6dfc47d4, 0x1a017ebc, 0xfb65c7c6, 0x535809b0, 0x8b58f181, 0x2454ed5e, 0x68220254, 0x7aaca143,
    0x91c10a55, 0x8bd74ece, 0xd67ef118, 0x0a15aa46, 0xa0c1b1ad, 0x4052d74a, 0xede8494b, 0xd5c49452,
    0x8fc97e18, 0x78c01c54, 0x2fbcf1c0, 0x335a046c, 0x3dc8834b, 0x6c550025, 0x278ab5d1, 0xc4422e4e,
    0x45e2073a, 0xa83fbd41, 0x738d1581, 0x9a3c0cc3, 0xf48d5436, 0x5ba835c6, 0xc63aaa6b, 0xd33328bc,
    0x9f703ab0, 0xee45ba36, 0x78417244, 0xf9e6e460, 0x288eb06c, 0x22a3e417, 0xdade69c6, 0x8b7bc447,
    0x7975c244, 0x51ef4cb7, 0x48ecffad, 0x32bf0918, 0xa98baa51, 0xedf5fa60, 0xe0aca658, 0x68fca0d9,
    0xe762c7c6, 0x1d2bc24e, 0x612d454b, 0x759fd295, 0xc218ef17, 0xb5c0b162, 0x5aaed56c, 0x1b52dc62,
    0x416c2d05, 0x42fbbfc3, 0xdc63134c, 0xc1b9e2bc, 0x0bd7f74d, 0x5eb9e2bc, 0xaf05a843, 0xafdf2e32,
    0x7d35bdd5, 0x8feeaa6b, 0x3e9e7d18, 0x47d1079d, 0x4d1d6a57, 0xc6cbffad, 0x0e80a7c7, 0x8dd0e8d8,
    0x334a066b, 0x31fae3bc, 0x0669e53a, 0x632c784c, 0x30381cb9, 0xaea5d118, 0xb7ee9ade, 0x4c37175e,
    0x9885f3a2, 0xbde5f6d8, 0xae62f3a2, 0xe276692e, 0xba5d1218, 0x511d5618, 0x7e81d78f, 0x14eb4c90,
    0x44e5c780, 0xe2708abc, 0xe261d3c6, 0x7aca584a, 0x8c037786, 0x7e3baa6b, 0xb912d8a2, 0x170763c0,
    0xbed1d23e, 0xdce46ccf, 0x5cdb1748, 0xc215f5ac, 0x99f7cabc, 0x4d658248, 0x3798bacd, 0x2dbb8932,
    0xe93faa6b, 0x639f534b, 0x43f6754c, 0x0440bd44, 0x4919aa6b, 0x30644aad, 0xc96e9c4e, 0x421daa6b,
    0x90760248, 0x096a1bc6, 0xba4b40ad, 0x3574fa5b, 0x20fe0732, 0x41535143, 0xdf843944, 0x103b8dd1,
    0xde76a032, 0xf405e447, 0xcc86bb25, 0xb3b22e6c, 0x12c35ad0, 0xa330aa6b, 0x14e69544, 0x177becd1,
    0xf32e484b, 0x15b1ce6d, 0x0d2c7662, 0x862d6441, 0xa2c39805, 0xc7eeaa6b, 0xf38f494c, 0x16d5b947,
    0x9fb8e2bc, 0xe13e175e, 0x656c39ad, 0x7a384946, 0x10460905, 0x9cf870d1, 0xb375c60e, 0x481cba6a,
    0x87ed6cd8, 0x2d7b67ae, 0x01540f48, 0x84028fc6, 0xdd85c144, 0x8f53ee54, 0x1a9e4fad, 0x84acc548,
    0x52d51248, 0xac47ab43, 0xf16ab743, 0x65ab6f18, 0xa8f73060, 0x56c84832, 0x9c6a604a, 0x8ef1ffad,
    0x4676a043, 0x6568c23e, 0x416ed459, 0xb586ec62, 0x122e98c2, 0x81a65618, 0x17db048e, 0x690f3b25,
    0xb58c4e32, 0x8e69e297, 0xd4dcaf42, 0x5158ac43, 0x0f9da082, 0x6cbaee60, 0x9e115b82, 0x55dffe96,
    0xa899f660, 0x846a0905, 0xf53a9252, 0xb283a4b8, 0x6db66b18, 0x867316b2, 0xdfad5753, 0x59995dd0,
    0x5e1caa6b, 0x08b3f160, 0x056a1bc6, 0x9c5403ae, 0x738cae43, 0x77fd211b, 0xd8e3a951, 0x150aad43,
    0x558809b0, 0xf04f454a, 0x09daf150, 0x3a433d63, 0xa4daf1c0, 0x4d551ac7, 0xd0edec69, 0xbe36f118,
    0x5b76b818, 0x8d919bd8, 0xbc92ea52, 0xe8096ec3, 0x1895e2bc, 0xfd053360, 0x701c42ad, 0x9a8e9bd8,
    0xac06aa6b, 0xc9851a56, 0x54f65e46, 0xb39e306c, 0x880763c0, 0x361f1fb0, 0x95a5b04c, 0x897319ad,
    0xc1b9be46, 0xe6d89d9d, 0xa75cd443, 0xb7361147, 0x630b8b25, 0x5a2e4ea6, 0x203bbb4b, 0x5b4b6c18,
    0x9dbbac43, 0x6d0663c0, 0x1f115536, 0x61d80718, 0xf4fde097, 0xfb50e162, 0x954a6276, 0x0006ee47,
    0x30b26eae, 0xf5737358, 0xf9a5fdce, 0xf2c75d18, 0xfda64b25, 0xd565d1a2, 0xf055a743, 0x10aaa545,
    0xa70c63c0, 0xa15a4450, 0xdbcb13ad, 0xdc705246, 0x02f7e9c7, 0xe57e6763, 0x65604ead, 0x07df1955,
    0x882faa6b, 0xb4286b18, 0x539747c0, 0xc9b6a1c0, 0xb76e7f46, 0xf3418432, 0xd5bbb153, 0xb840d3a2,
    0x6c0717d9, 0xb14109b0, 0x94076181, 0xf40aa953, 0x08de89c3, 0x06bdaa4d, 0x27efae32, 0x5ba4fe45,
    0x1e7410cc, 0x57e847d4, 0xe5425177, 0xb50b714c, 0x06513532, 0xfde617c6, 0xe05c3532, 0x0af30852,
    0xa2a33a60, 0x2cc7f3a2, 0x5777cf57, 0x313477d5, 0x311544c1, 0x49f4aa6b, 0xef01ef17, 0x07db33d4,
    0x0a60c947, 0xe4174e4d, 0x34058a12, 0x411e3ab8, 0xfc4a9532, 0x5bcaf082, 0x55c21955, 0x489a32b6,
    0xf1f7154c, 0x60fa9f6e, 0x4dc4ec57, 0x5fe7a743, 0x1309e552, 0x03e12c42, 0x07f4175e, 0xcb56aa43,
    0x9d411787, 0xc1686041, 0xeec21955, 0xe97cf3a2, 0x11cc6044, 0xce9b2b47, 0x32bb53d4, 0xc7153b25,
    0x7c24188d, 0x1640c947, 0x1606b941, 0x671d864b, 0xb4430118, 0x3c6641ad, 0xf99000c0, 0x57a745ad,
    0xf098880e, 0x7cd14298, 0xb53316b2, 0x5b0749ae, 0x80c734c6, 0x5bb26318, 0xf5734761, 0xb5a0f1c0,
    0x643f3cae, 0xd034324a, 0xd5ef81ae, 0xa383e447, 0x8c820298, 0xeba139ae, 0x1e73c80e, 0x0c3fe743,
    0x8ac65918, 0x03104081, 0xa486bb25, 0x24165343, 0x15c67118, 0x6204f948, 0x4e3d5bc6, 0x3ef9f382,
    0xe3e7524a, 0x9f10634c, 0xfba8794c, 0x0c2d306c, 0xb99df762, 0x3e1a4d47, 0x3f6c3951, 0xd373354d,
    0xca206c4a, 0x6f454aad, 0x02f5af43, 0x0f35831f, 0x31bcd4ad, 0xfa34da48, 0x3a598cae, 0x26ef7f4c,
    0xa104b648, 0x9cf2134c, 0xc5c2336c, 0x448f9c5b, 0x55b8f1c0, 0x115917ad, 0x33216f4b, 0x5c67692e,
    0xd436c658, 0x7c22da36, 0x0ce9e217, 0xa62a3160, 0xb08803ae, 0xf918c162, 0x0e9c096c, 0xf250119c,
    0x0d987f47, 0x40254832, 0x610df245, 0xa55cf3a2, 0xd4886e4a, 0xc172aa43, 0x1af4c04e, 0xbe2d47ae,
    0x58a0cf45, 0xa414dc48, 0xdde3f3a2, 0x26562843, 0x9aea0455, 0xb1b63c44, 0xcef89963, 0x12005d4d,
    0x21c21754, 0x18eef160, 0x4903a1cf, 0x9b023818, 0x4685336c, 0xc9f31556, 0xe6dd0a18, 0x099630ad,
    0x10dabd32, 0xcaaf708c, 0xc53ebb25, 0x42657a90, 0x74e3614c, 0x365c4048, 0x7cf9f3a2, 0x7f2addb0,
    0xe6bf09b0, 0x1165fa5b, 0xcef84e43, 0x895be7cf, 0xb2116c4a, 0xa423108a, 0xa3bec236, 0x196db14c,
    0xe05f6f4f, 0x07ee0118, 0xdcb8f8ce, 0xbd5e7918, 0x2159ab4d, 0x8ae54170, 0x527b0905, 0x7c8d3d44,
    0x0d552705, 0xe5b21cad, 0x1b8ae64e, 0xc79cab55, 0x4e88e2bc, 0x444c1581, 0x86147c4c, 0xb4222648,
    0x4136daad, 0x437ea22e, 0x2b92694c, 0x154baa6b, 0x0555795b, 0x9b484232, 0x6944f181, 0x036a6259,
    0x1808ff60, 0xb0403148, 0x7b5e1b59, 0xdd30fb62, 0x4583266c, 0xb289c96d, 0x35e4aa6b, 0x50a23532,
    0x3f1263c0, 0x763aae6c, 0xa729c144, 0x5ee5c64c, 0x02bf35d0, 0x367f3ac6, 0xfe547940, 0x2e1ea353,
    0xe798ee48, 0x6c058495, 0x755dd5cb, 0x2ca6ef63, 0x7a0574bc, 0x9ac81c2e, 0xa5b33e44, 0x4fe69953,
    0xf9556d62, 0x086a1bc6, 0xac28126c, 0x4fa7e6ad, 0xd4293d3e, 0x4e4f764b, 0x718849ad, 0x8139ff18,
    0x4c6118b9, 0x65444c90, 0x371eeda9, 0xe2b4c0ad, 0xcd01f5da, 0x7671c344, 0x3021f518, 0x26369c4b,
    0xd7f6f360, 0xdb55336c, 0xccdc7d18, 0x51d732c6, 0x335b5b4c, 0x094c2d05, 0x0dd546a6, 0x83d33118,
    0x33dcff6d, 0xfd40b64c, 0x86e93f18, 0x87e2c780, 0xf0e1c1b2, 0xc67be347, 0x1df9d243, 0x6a646dae,
    0xb0833b47, 0x8f457e4c, 0x0dbbe2bc, 0x4ac048c6, 0x7cc2194c, 0x1c84bb25, 0xb66b8218, 0xe3ee027b,
    0x5cd4048e, 0xadcb0b18, 0xdc036e18, 0xb62e7999, 0xdee95243, 0x1ccdc344, 0x91cde560, 0xc8e0f460,
    0xbea0f1c0, 0x35747b6d, 0x9686bb25, 0x28ce3c47, 0xaf0e5e48, 0x87f58f4b, 0x124dd9a2, 0xa21463c0,
    0xa8f94952, 0xf71fd586, 0xe6fa056c, 0x92b86bb8, 0x707d6981, 0x22870732, 0x116fef62, 0xe0fd3ead,
    0x78c2814e, 0x95d882c3, 0xdb3cba32, 0xe8b0e2bc, 0xb9c69b18, 0xe29ed783, 0x26c01118, 0xcbcd8451,
    0xf1d02e18, 0x1b55aead, 0xb5549bc0, 0x47baa232, 0xbcf8f1c0, 0xae29dbb2, 0x0b881cd2, 0x75c8bb25,
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
        bnProofOfWorkLimit = ~uint256(0) >> 20;
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
        vSeeds.push_back(CDNSSeedData("mophides.com", "seed.mophides.com"));
        vSeeds.push_back(CDNSSeedData("dglibrary.org", "seed.dglibrary.org"));
        vSeeds.push_back(CDNSSeedData("dogechain.info", "seed.dogechain.info"));

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
        bnProofOfWorkLimit = ~uint256(0) >> 1;
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
