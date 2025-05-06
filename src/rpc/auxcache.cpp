// Copyright (c) 2025 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "rpc/auxcache.h"

#include "script/standard.h"  // for CScriptID
#include "primitives/block.h" // for CBlock
#include "uint256.h"          // for uint256
#include "utilmemory.h"       // for MakeUnique
#include "utiltime.h"         // for GetTimeMicros

#include <boost/multi_index_container.hpp>
#include <boost/multi_index/ordered_index.hpp>

#include <cstdint>            // for uint64_t
#include <memory>             // for std::unique_ptr, std::shared_ptr
#include <mutex>              // for std::mutex
#include <tuple>              // for std::tuple

namespace {

//! Type that is stored in the cache
struct AuxCacheItem {
  int64_t cache_time;
  CScriptID scriptId;
  uint256 hash;
  std::shared_ptr<CBlock> pblock;
};

// The ByScriptId index sorts by CScriptID and item age
struct ByScriptId {};
using ByScriptIdView = std::tuple<const CScriptID&, const int64_t>;
struct ByScriptIdExtractor
{
    using result_type = ByScriptIdView;
    result_type operator()(const AuxCacheItem& item) const
    {
        // calculate age
        const int64_t age = GetMockableTimeMicros() - item.cache_time;
        return ByScriptIdView{item.scriptId, age};
    }
};

// The ByBlockHash index sorts by blockhash (uint256)
struct ByBlockHash {};
struct ByBlockHashExtractor
{
    using result_type = uint256;
    result_type operator()(const AuxCacheItem& item) const
    {
        return item.hash;
    }
};

/** Data type for the main data structure (AuxCacheItem objects with ByScriptID/ByBlockHash indexes). */
using AuxCacheIndex = boost::multi_index_container<
    AuxCacheItem,
    boost::multi_index::indexed_by<
        boost::multi_index::ordered_non_unique<boost::multi_index::tag<ByScriptId>, ByScriptIdExtractor>,
        boost::multi_index::ordered_unique<boost::multi_index::tag<ByBlockHash>, ByBlockHashExtractor>
    >
>;

} //anon namespace

class CAuxBlockCache::Impl {
    AuxCacheIndex m_index;

private:
  std::mutex mut;

public:
    Impl() : m_index(boost::make_tuple(
        boost::make_tuple(ByScriptIdExtractor(), std::less<ByScriptIdView>()),
        boost::make_tuple(ByBlockHashExtractor(), std::less<uint256>())
    )) {};

    Impl(const Impl&) = delete;
    Impl& operator=(const Impl&) = delete;

    bool Add(const CScriptID scriptId, std::shared_ptr<CBlock> pblock) {

        uint256 hash = pblock->GetHash();
        int64_t micros = GetMockableTimeMicros();

        std::lock_guard<std::mutex> guard(mut);
        auto ret = m_index.get<ByBlockHash>().emplace(AuxCacheItem{micros, scriptId, hash, pblock});

        return ret.second;
    }

    void Reset() {
        m_index.clear();
    }

    bool Get(const CScriptID scriptId, std::shared_ptr<CBlock>& pblock) const {
        auto it = m_index.get<ByScriptId>().lower_bound(ByScriptIdView{scriptId, 0LL});
        if (it != m_index.get<ByScriptId>().end() && it->scriptId == scriptId) {
            pblock = it->pblock;
            return true;
        }
        pblock.reset();
        return false;
    }

    bool Get(const uint256 blockhash, std::shared_ptr<CBlock>& pblock) const {
        auto it = m_index.get<ByBlockHash>().lower_bound(blockhash);
        if (it != m_index.get<ByBlockHash>().end() && it->hash == blockhash) {
            pblock = it->pblock;
            return true;
        }
        pblock.reset();
        return false;
    }
};

CAuxBlockCache::CAuxBlockCache() : m_impl(MakeUnique<CAuxBlockCache::Impl>()) {};
CAuxBlockCache::~CAuxBlockCache() = default;

bool CAuxBlockCache::Add(const CScriptID scriptId, std::shared_ptr<CBlock> pblock) {
    return m_impl->Add(scriptId, pblock);
}

void CAuxBlockCache::Reset() {
    m_impl->Reset();
}

bool CAuxBlockCache::Get(const CScriptID scriptId, std::shared_ptr<CBlock>& pblock) {
    return m_impl->Get(scriptId, pblock);
}

bool CAuxBlockCache::Get(const uint256 blockhash, std::shared_ptr<CBlock>& pblock) {
    return m_impl->Get(blockhash, pblock);
}
