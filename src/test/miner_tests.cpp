// Copyright (c) 2011-2014 The Bitcoin Core developers
// Distributed under the MIT/X11 software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "main.h"
#include "miner.h"
#include "uint256.h"
#include "util.h"

#include <boost/test/unit_test.hpp>

extern void SHA256Transform(void* pstate, void* pinput, const void* pinit);

BOOST_AUTO_TEST_SUITE(miner_tests)

// this array of extranonce/nonce combinations has been generated
// to be replayed on top of the DOGECOIN genesis block.
static
struct {
    unsigned char extranonce;
    unsigned int nonce;
} blockinfo[] = {
    {4, 0x127fad2d}, {2, 0x335d1b8f}, {1, 0x33d47094}, {2, 0x0b09ec28},
    {1, 0x06cf723b}, {2, 0x039202bc}, {1, 0x0a2c9d46}, {2, 0x6225cb92},
    {2, 0x6ea1513e}, {1, 0x4401bef3}, {1, 0x04d3a1d2}, {2, 0x1c512825},
    {2, 0x54a03b14}, {1, 0x6048e27d}, {1, 0x1b926afc}, {2, 0x68c4afbd},
    {2, 0x4439c313}, {1, 0x1263fceb}, {2, 0x834dee3e}, {2, 0xf21ed9dc},
    {1, 0xdcdac434}, {2, 0x4c1945be}, {1, 0x6d42a594}, {3, 0x20927a30},
    {3, 0xfd60f461}, {2, 0xd9ad2207}, {2, 0xe7f69d1a}, {1, 0x7fa9b932},
    {2, 0xb0511080}, {1, 0xe7d24cd5}, {2, 0x3c57e668}, {2, 0x83bfdc2e},
    {2, 0x6eeb4e10}, {2, 0x9cacbcfd}, {2, 0xb27ea98e}, {2, 0x6d57c5a7},
    {1, 0x6deb4fa8}, {2, 0xabf625c6}, {2, 0x27e7c569}, {1, 0x89c6e991},
    {2, 0xc359bc28}, {1, 0x6f25768d}, {2, 0x654a4c31}, {1, 0x5cd03bab},
    {1, 0xda405f69}, {3, 0xfea453e5}, {2, 0x137d2c3a}, {5, 0xdee2f36e},
    {1, 0xeccbcf26}, {5, 0x9237dbaa}, {1, 0xb7b9350b}, {1, 0xcd0c7eb2},
    {1, 0xf5ea5a32}, {2, 0x3486a7f3}, {1, 0xd0a0f2be}, {1, 0xe1238144},
    {1, 0x28b98a9b}, {1, 0xe79d02aa}, {5, 0xf4555d56}, {5, 0x74da0bb7},
    {1, 0x18728b91}, {1, 0x07ed3a93}, {6, 0xd7a5e106}, {2, 0xba50b06c},
    {2, 0x952c830d}, {1, 0xfbd1bb18}, {1, 0x36126967}, {1, 0xcce357d0},
    {2, 0xff1ec2d6}, {2, 0xbed5dfc9}, {1, 0x0d21fdd7}, {1, 0xd744edea},
    {1, 0xe09fc8f2}, {5, 0x2ad325c5}, {5, 0x466b6549}, {1, 0x10705d49},
    {1, 0xf88478ce}, {2, 0xbfda6c4a}, {2, 0x731fe414}, {1, 0x6f1b362e},
    {2, 0x6be709cf}, {1, 0x60553200}, {2, 0xf6a992f0}, {2, 0x1521f7f5},
    {1, 0x8b440273}, {1, 0xe9ade0c8}, {1, 0x4d414618}, {5, 0x7b48070d},
    {1, 0x1202ebae}, {1, 0xd23fe97e}, {1, 0x8d1d6505}, {1, 0xfafbaae3},
    {1, 0xf200353e}, {1, 0xe77bd65e}, {1, 0x9fa32102}, {2, 0x68dfa747},
    {0, 0x7c74d78e}, {1, 0x9b79cc6b}, {2, 0xad957cc2}, {2, 0x91acb818}
};

// NOTE: These tests rely on CreateNewBlock doing its own self-validation!
BOOST_AUTO_TEST_CASE(CreateNewBlock_validity)
{
    // changed this to dogecoin genesis pubkey script
    CScript scriptPubKey = CScript() << ParseHex("040184710fa689ad5023690c80f3a49c8f13f8d45b8c857fbcbc8bc4a8e4d3eb4b10f4d4604fa08dce601aaf0f470216fe1b51850b4acf21b179c45070ac7b03a9") << OP_CHECKSIG;
    CBlockTemplate *pblocktemplate;
    CTransaction tx,tx2;
    CScript script;
    uint256 hash;

    LOCK(cs_main);

    // Simple block creation, nothing special yet:
    BOOST_CHECK(pblocktemplate = CreateNewBlock(scriptPubKey));

    // We can't make transactions until we have inputs
    // Therefore, load 100 pregenerated blocks :)
    std::vector<CTransaction*>txFirst;
    for (unsigned int i = 0; i < sizeof(blockinfo)/sizeof(*blockinfo); ++i)
    {
        CBlock *pblock = &pblocktemplate->block; // pointer for convenience
        pblock->nVersion = 1;

        // Replaced chainActive.Tip()->GetMedianTimePast()+1 with an actual 60 second block
        // interval because median([1,2,2,3,3,3,4,4,4,4]) will eventually be problematic re:
        // block timing. Tests should be more stable than that.
        pblock->nTime = chainActive.Tip()->GetBlockTime() + 60;
        pblock->vtx[0].vin[0].scriptSig = CScript();
        pblock->vtx[0].vin[0].scriptSig.push_back(blockinfo[i].extranonce);
        pblock->vtx[0].vin[0].scriptSig.push_back(chainActive.Height());
        pblock->vtx[0].vout[0].scriptPubKey = CScript();
        if (txFirst.size() < 2)
            txFirst.push_back(new CTransaction(pblock->vtx[0]));
        pblock->hashMerkleRoot = pblock->BuildMerkleTree();
        pblock->nNonce = blockinfo[i].nonce;
        CValidationState state;
        BOOST_CHECK(ProcessBlock(state, NULL, pblock));
        BOOST_CHECK(state.IsValid());
        pblock->hashPrevBlock = pblock->GetHash();
    }
    delete pblocktemplate;

    // Make sure all created blocks are in the active chain
    BOOST_CHECK(chainActive.Height() == sizeof(blockinfo)/sizeof(*blockinfo));

    // Just to make sure we can still make simple blocks
    BOOST_CHECK(pblocktemplate = CreateNewBlock(scriptPubKey));
    delete pblocktemplate;

    // block sigops > limit: 1000 CHECKMULTISIG + 1
    tx.vin.resize(1);
    // NOTE: OP_NOP is used to force 20 SigOps for the CHECKMULTISIG
    tx.vin[0].scriptSig = CScript() << OP_0 << OP_0 << OP_0 << OP_NOP << OP_CHECKMULTISIG << OP_1;
    tx.vin[0].prevout.hash = txFirst[0]->GetHash();
    tx.vin[0].prevout.n = 0;
    tx.vout.resize(1);
    tx.vout[0].nValue = 5000000000LL;
    for (unsigned int i = 0; i < 1001; ++i)
    {
        tx.vout[0].nValue -= 1000000;
        hash = tx.GetHash();
        mempool.addUnchecked(hash, CTxMemPoolEntry(tx, 11, GetTime(), 111.0, 11));
        tx.vin[0].prevout.hash = hash;
    }
    BOOST_CHECK(pblocktemplate = CreateNewBlock(scriptPubKey));
    delete pblocktemplate;
    mempool.clear();

    // block size > limit
    tx.vin[0].scriptSig = CScript();
    // 18 * (520char + DROP) + OP_1 = 9433 bytes
    std::vector<unsigned char> vchData(520);
    for (unsigned int i = 0; i < 18; ++i)
        tx.vin[0].scriptSig << vchData << OP_DROP;
    tx.vin[0].scriptSig << OP_1;
    tx.vin[0].prevout.hash = txFirst[0]->GetHash();
    tx.vout[0].nValue = 5000000000LL;
    for (unsigned int i = 0; i < 128; ++i)
    {
        tx.vout[0].nValue -= 10000000;
        hash = tx.GetHash();
        mempool.addUnchecked(hash, CTxMemPoolEntry(tx, 11, GetTime(), 111.0, 11));
        tx.vin[0].prevout.hash = hash;
    }
    BOOST_CHECK(pblocktemplate = CreateNewBlock(scriptPubKey));
    delete pblocktemplate;
    mempool.clear();

    // orphan in mempool
    hash = tx.GetHash();
    mempool.addUnchecked(hash, CTxMemPoolEntry(tx, 11, GetTime(), 111.0, 11));
    BOOST_CHECK(pblocktemplate = CreateNewBlock(scriptPubKey));
    delete pblocktemplate;
    mempool.clear();

    // child with higher priority than parent
    tx.vin[0].scriptSig = CScript() << OP_1;
    tx.vin[0].prevout.hash = txFirst[1]->GetHash();
    tx.vout[0].nValue = 4900000000LL;
    hash = tx.GetHash();
    mempool.addUnchecked(hash, CTxMemPoolEntry(tx, 11, GetTime(), 111.0, 11));
    tx.vin[0].prevout.hash = hash;
    tx.vin.resize(2);
    tx.vin[1].scriptSig = CScript() << OP_1;
    tx.vin[1].prevout.hash = txFirst[0]->GetHash();
    tx.vin[1].prevout.n = 0;
    tx.vout[0].nValue = 5900000000LL;
    hash = tx.GetHash();
    mempool.addUnchecked(hash, CTxMemPoolEntry(tx, 11, GetTime(), 111.0, 11));
    BOOST_CHECK(pblocktemplate = CreateNewBlock(scriptPubKey));
    delete pblocktemplate;
    mempool.clear();

    // coinbase in mempool
    tx.vin.resize(1);
    tx.vin[0].prevout.SetNull();
    tx.vin[0].scriptSig = CScript() << OP_0 << OP_1;
    tx.vout[0].nValue = 0;
    hash = tx.GetHash();
    mempool.addUnchecked(hash, CTxMemPoolEntry(tx, 11, GetTime(), 111.0, 11));
    BOOST_CHECK(pblocktemplate = CreateNewBlock(scriptPubKey));
    delete pblocktemplate;
    mempool.clear();

    // invalid (pre-p2sh) txn in mempool
    tx.vin[0].prevout.hash = txFirst[0]->GetHash();
    tx.vin[0].prevout.n = 0;
    tx.vin[0].scriptSig = CScript() << OP_1;
    tx.vout[0].nValue = 4900000000LL;
    script = CScript() << OP_0;
    tx.vout[0].scriptPubKey.SetDestination(script.GetID());
    hash = tx.GetHash();
    mempool.addUnchecked(hash, CTxMemPoolEntry(tx, 11, GetTime(), 111.0, 11));
    tx.vin[0].prevout.hash = hash;
    tx.vin[0].scriptSig = CScript() << (std::vector<unsigned char>)script;
    tx.vout[0].nValue -= 1000000;
    hash = tx.GetHash();
    mempool.addUnchecked(hash, CTxMemPoolEntry(tx, 11, GetTime(), 111.0, 11));
    BOOST_CHECK(pblocktemplate = CreateNewBlock(scriptPubKey));
    delete pblocktemplate;
    mempool.clear();

    // double spend txn pair in mempool
    tx.vin[0].prevout.hash = txFirst[0]->GetHash();
    tx.vin[0].scriptSig = CScript() << OP_1;
    tx.vout[0].nValue = 4900000000LL;
    tx.vout[0].scriptPubKey = CScript() << OP_1;
    hash = tx.GetHash();
    mempool.addUnchecked(hash, CTxMemPoolEntry(tx, 11, GetTime(), 111.0, 11));
    tx.vout[0].scriptPubKey = CScript() << OP_2;
    hash = tx.GetHash();
    mempool.addUnchecked(hash, CTxMemPoolEntry(tx, 11, GetTime(), 111.0, 11));
    BOOST_CHECK(pblocktemplate = CreateNewBlock(scriptPubKey));
    delete pblocktemplate;
    mempool.clear();

    // subsidy changing
    int nHeight = chainActive.Height();
    chainActive.Tip()->nHeight = 209999;
    BOOST_CHECK(pblocktemplate = CreateNewBlock(scriptPubKey));
    delete pblocktemplate;
    chainActive.Tip()->nHeight = 210000;
    BOOST_CHECK(pblocktemplate = CreateNewBlock(scriptPubKey));
    delete pblocktemplate;
    chainActive.Tip()->nHeight = nHeight;

    // non-final txs in mempool
    // changed to 60 second block interval for consistency
    SetMockTime(chainActive.Tip()->GetBlockTime() + 60);

    // height locked
    tx.vin[0].prevout.hash = txFirst[0]->GetHash();
    tx.vin[0].scriptSig = CScript() << OP_1;
    tx.vin[0].nSequence = 0;
    tx.vout[0].nValue = 4900000000LL;
    tx.vout[0].scriptPubKey = CScript() << OP_1;
    tx.nLockTime = chainActive.Tip()->nHeight+1;
    hash = tx.GetHash();
    mempool.addUnchecked(hash, CTxMemPoolEntry(tx, 11, GetTime(), 111.0, 11));
    BOOST_CHECK(!IsFinalTx(tx, chainActive.Tip()->nHeight + 1));

    // time locked
    tx2.vin.resize(1);
    tx2.vin[0].prevout.hash = txFirst[1]->GetHash();
    tx2.vin[0].prevout.n = 0;
    tx2.vin[0].scriptSig = CScript() << OP_1;
    tx2.vin[0].nSequence = 0;
    tx2.vout.resize(1);
    tx2.vout[0].nValue = 4900000000LL;
    tx2.vout[0].scriptPubKey = CScript() << OP_1;
    // changed to 60 second block interval for consistency
    tx2.nLockTime = chainActive.Tip()->GetBlockTime() + 60;
    hash = tx2.GetHash();
    mempool.addUnchecked(hash, CTxMemPoolEntry(tx2, 11, GetTime(), 111.0, 11));
    BOOST_CHECK(!IsFinalTx(tx2));

    BOOST_CHECK(pblocktemplate = CreateNewBlock(scriptPubKey));

    // Neither tx should have make it into the template.
    BOOST_CHECK_EQUAL(pblocktemplate->block.vtx.size(), 1);
    delete pblocktemplate;

    // However if we advance height and time by one, both will.
    chainActive.Tip()->nHeight++;

    // changed to 60 second block interval for consistency (times 2 for this test)
    SetMockTime(chainActive.Tip()->GetBlockTime() + 120);

    BOOST_CHECK(IsFinalTx(tx, chainActive.Tip()->nHeight + 1));
    BOOST_CHECK(IsFinalTx(tx2));

    BOOST_CHECK(pblocktemplate = CreateNewBlock(scriptPubKey));
    BOOST_CHECK_EQUAL(pblocktemplate->block.vtx.size(), 3);
    delete pblocktemplate;

    chainActive.Tip()->nHeight--;
    SetMockTime(0);

    BOOST_FOREACH(CTransaction *tx, txFirst)
        delete tx;

}

BOOST_AUTO_TEST_CASE(sha256transform_equality)
{
    unsigned int pSHA256InitState[8] = {0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19};


    // unsigned char pstate[32];
    unsigned char pinput[64];

    int i;

    for (i = 0; i < 32; i++) {
        pinput[i] = i;
        pinput[i+32] = 0;
    }

    uint256 hash;

    SHA256Transform(&hash, pinput, pSHA256InitState);

    BOOST_TEST_MESSAGE(hash.GetHex());

    uint256 hash_reference("0x2df5e1c65ef9f8cde240d23cae2ec036d31a15ec64bc68f64be242b1da6631f3");

    BOOST_CHECK(hash == hash_reference);
}

BOOST_AUTO_TEST_SUITE_END()
