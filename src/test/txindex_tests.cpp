// Copyright (c) 2017-2018 The Bitcoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include <index/txindex.h>
#include <script/standard.h>
#include <test/test_bitcoin.h>
#include <util.h>
#include <utiltime.h>
#include <validation.h>

#include <boost/test/unit_test.hpp>

BOOST_AUTO_TEST_SUITE(txindex_tests)

BOOST_FIXTURE_TEST_CASE(txindex_initial_sync, TestChain240Setup)
{
    TxIndex txindex(1 << 20, true);

    CTransactionRef tx_disk;
    uint256 block_hash;

    // Transaction should not be found in the index before it is started.
    for (const auto& txn : coinbaseTxns) {
        BOOST_CHECK(!txindex.FindTx(txn.GetHash(), block_hash, tx_disk));
    }

    // BlockUntilSyncedToCurrentChain should return false before txindex is started.
    BOOST_CHECK(!txindex.BlockUntilSyncedToCurrentChain());

    txindex.Start();

    // Allow tx index to catch up with the block index.
    constexpr int64_t timeout_ms = 10 * 1000;
    int64_t time_start = GetTimeMillis();
    while (!txindex.BlockUntilSyncedToCurrentChain()) {
        BOOST_REQUIRE(time_start + timeout_ms > GetTimeMillis());
        MilliSleep(100);
    }

    // Check that txindex has all txs that were in the chain before it started.
    for (const auto& txn : coinbaseTxns) {
        if (!txindex.FindTx(txn.GetHash(), block_hash, tx_disk)) {
            BOOST_ERROR("FindTx failed");
        } else if (tx_disk->GetHash() != txn.GetHash()) {
            BOOST_ERROR("Read incorrect tx");
        }
    }

    // Check that new transactions in new blocks make it into the index.
    for (int i = 0; i < 10; i++) {
        CScript coinbase_script_pub_key = GetScriptForDestination(coinbaseKey.GetPubKey().GetID());
        std::vector<CMutableTransaction> no_txns;
        const CBlock block = CreateAndProcessBlock(no_txns, coinbase_script_pub_key);
        const CTransaction& txn = *block.vtx[0];

        BOOST_CHECK(txindex.BlockUntilSyncedToCurrentChain());
        if (!txindex.FindTx(txn.GetHash(), block_hash, tx_disk)) {
            BOOST_ERROR("FindTx failed");
        } else if (tx_disk->GetHash() != txn.GetHash()) {
            BOOST_ERROR("Read incorrect tx");
        }
    }
}

// BaseIndex persists its progress locator on a timer while syncing. The locator must never
// name a block the index has not finished writing, or a crash in that window leaves the block
// unindexed and the restart resumes past it.
namespace {
class ReviewCheckpointIndex : public BaseIndex
{
    mutable DB db;
public:
    std::atomic<int> premature_checkpoints{0};
    ReviewCheckpointIndex() : db(GetDataDir() / "review-checkpoint", 1 << 20, true) {}
    ~ReviewCheckpointIndex() { Interrupt(); Stop(); }
protected:
    DB& GetDB() const override { return db; }
    const char* GetName() const override { return "review-checkpoint"; }
    bool WriteBlock(const CBlock& block, const CBlockIndex* pindex) override
    {
        CBlockLocator locator;
        if (pindex->nHeight > 0 && db.ReadBestBlock(locator) &&
            !locator.vHave.empty() && locator.vHave.front() == pindex->GetBlockHash()) {
            ++premature_checkpoints;
        }
        // Advance the clock to exercise periodic persistence without waiting
        // 30 seconds per block. The chain itself was built by the fixture first.
        SetMockTime(GetTime() + 31);
        return true;
    }
};
}

BOOST_FIXTURE_TEST_CASE(review_checkpoint_after_write, TestChain240Setup)
{
    ReviewCheckpointIndex index;
    index.Start();
    const int64_t started = GetTimeMillis();
    while (!index.BlockUntilSyncedToCurrentChain() && GetTimeMillis() - started < 10000) {
        MilliSleep(10);
    }
    index.Interrupt();
    index.Stop();
    SetMockTime(0);
    BOOST_REQUIRE(index.GetSummary().synced);
    BOOST_CHECK_MESSAGE(index.premature_checkpoints.load() == 0,
        index.premature_checkpoints.load() << " block locators persisted before WriteBlock");
}

BOOST_AUTO_TEST_SUITE_END()
