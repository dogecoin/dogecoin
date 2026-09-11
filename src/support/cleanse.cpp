// Copyright (c) 2009-2010 Satoshi Nakamoto
// Copyright (c) 2009-2015 The Bitcoin Core developers
// Distributed under the MIT software license, see the accompanying
// file COPYING or http://www.opensource.org/licenses/mit-license.php.

#include "cleanse.h"

#include <cstring>
#include <atomic>

void memory_cleanse(void *ptr, size_t len)
{
    // Set memory to zero
    std::memset(ptr, 0, len);

    // Use an atomic signal fence to prevent the compiler from optimizing the memset away
    std::atomic_signal_fence(std::memory_order_seq_cst);

    // Volatile pointer to prevent compiler optimizing the memory access away
    __asm__ __volatile__("" : : "r"(ptr) : "memory");
}
