// Copyright (c) 2019 The Bitcoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include <bench/data.h>

namespace benchmark {
namespace data {

#include <bench/data/block41385.raw.h>
const std::vector<uint8_t> block41385{block41385_raw, block41385_raw + sizeof(block41385_raw) / sizeof(block41385_raw[0])};

} // namespace data
} // namespace benchmark
