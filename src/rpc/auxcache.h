// Copyright (c) 2025 The Dogecoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#ifndef DOGECOIN_AUXCACHE_H
#define DOGECOIN_AUXCACHE_H

#include "script/standard.h"  // for CScriptID
#include "primitives/block.h" // for CBlock
#include "uint256.h"          // for uint256

#include <memory>             // for std::unique_ptr, std::shared_ptr

/** Cache to keep track of blocks templated for AuxPoW mining, by CScriptID
 *  (coinbase output script.)
 *
 *  Searchable by coinbase scriptpubkey (CScriptID) and blockhash (uint256)
 *
 */
class CAuxBlockCache {
    // Do not put impementation details in the header because they are
    // heavy on includes. Instead use an implementation class.
    class Impl;
    const std::unique_ptr<Impl> m_impl;

public:
    explicit CAuxBlockCache();
    ~CAuxBlockCache();

    /** Adds a block to the cache */
    bool Add(const CScriptID scriptId, std::shared_ptr<CBlock> pblock);

    /** Resets the entire cache */
    void Reset();

    /** Get the cached CBlock (optional) for a CScriptID */
    bool Get(const CScriptID scriptId, std::shared_ptr<CBlock>& pblock);

    /** Get the cached CBlock (optional) by block hash */
    bool Get(const uint256 blockhash, std::shared_ptr<CBlock>& pblock);

};

#endif //DOGECOIN_AUXCACHE_H
